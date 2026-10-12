namespace FSharp.Graphics.Host.Skia

open System
open System.Collections.Generic
open SkiaSharp
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Host

module private Conv =
    let color (c: Color) = SKColor(c.R, c.G, c.B, c.A)
    let rect (r: Rect) = SKRect(r.X, r.Y, r.X + r.W, r.Y + r.H)

    let blend (b: Blend) =
        match b with
        | Normal -> SKBlendMode.SrcOver | Multiply -> SKBlendMode.Multiply | Screen -> SKBlendMode.Screen
        | Overlay -> SKBlendMode.Overlay | Darken -> SKBlendMode.Darken | Lighten -> SKBlendMode.Lighten
        | ColorDodge -> SKBlendMode.ColorDodge | ColorBurn -> SKBlendMode.ColorBurn
        | HardLight -> SKBlendMode.HardLight | SoftLight -> SKBlendMode.SoftLight
        | Difference -> SKBlendMode.Difference | Exclusion -> SKBlendMode.Exclusion

    /// Grow (or shrink, if negative) a shape by `d` on every side - used for shadow spread.
    let inflate (g: ShapeGeometry) (d: float32) : ShapeGeometry =
        match g with
        | GRect(b, r) ->
            let f (x: float32) = if x > 0.0f then MathF.Max(x + d, 0.0f) else 0.0f
            GRect(Rect.expand d b, { TopLeft = f r.TopLeft; TopRight = f r.TopRight; BottomRight = f r.BottomRight; BottomLeft = f r.BottomLeft })
        | GCircle(c, r)       -> GCircle(c, MathF.Max(r + d, 0.0f))
        | GEllipse(c, rx, ry) -> GEllipse(c, MathF.Max(rx + d, 0.0f), MathF.Max(ry + d, 0.0f))
        | GLine(a, b, t)      -> GLine(a, b, MathF.Max(t + 2.0f * d, 0.0f))
        | GOpaque b           -> GOpaque(Rect.expand d b)
        | GPolygon _          -> g

    let roundRect (b: Rect) (r: CornerRadius) : SKRoundRect =
        let rr = new SKRoundRect()
        rr.SetRectRadii(rect b, [| SKPoint(r.TopLeft, r.TopLeft); SKPoint(r.TopRight, r.TopRight)
                                   SKPoint(r.BottomRight, r.BottomRight); SKPoint(r.BottomLeft, r.BottomLeft) |])
        rr

    let toPath (g: ShapeGeometry) : SKPath =
        let p = new SKPath()
        match g with
        | GRect(b, r)         -> use rr = roundRect b r in p.AddRoundRect rr
        | GCircle(c, r)       -> p.AddCircle(c.X, c.Y, r)
        | GEllipse(c, rx, ry) -> p.AddOval(SKRect(c.X - rx, c.Y - ry, c.X + rx, c.Y + ry))
        | GPolygon vs ->
            if vs.Length > 0 then
                p.MoveTo(vs.[0].X, vs.[0].Y)
                for i in 1 .. vs.Length - 1 do p.LineTo(vs.[i].X, vs.[i].Y)
                p.Close()
        | GLine(a, b, _)      -> p.AddRect(SKRect(MathF.Min(a.X, b.X), MathF.Min(a.Y, b.Y), MathF.Max(a.X, b.X), MathF.Max(a.Y, b.Y)))
        | GOpaque b           -> p.AddRect(rect b)
        p

    /// Skia versions differ on the scale of the colour matrix's translate column (0..255 vs 0..1).
    /// Measure it once instead of guessing.
    let translateScale : Lazy<float32> =
        lazy (
            try
                use bmp = new SKBitmap(1, 1, SKColorType.Rgba8888, SKAlphaType.Premul)
                use cv = new SKCanvas(bmp)
                cv.Clear(SKColors.Black)
                use f = SKColorFilter.CreateColorMatrix([| 0.0f; 0.0f; 0.0f; 0.0f; 1.0f
                                                           0.0f; 0.0f; 0.0f; 0.0f; 0.0f
                                                           0.0f; 0.0f; 0.0f; 0.0f; 0.0f
                                                           0.0f; 0.0f; 0.0f; 1.0f; 0.0f |])
                use p = new SKPaint(ColorFilter = f)
                cv.DrawRect(SKRect(0.0f, 0.0f, 1.0f, 1.0f), p)
                if bmp.GetPixel(0, 0).Red >= 128uy then 1.0f else 255.0f
            with _ -> 1.0f)

/// GPU/raster canvas backend (SkiaSharp). Same contract as the software renderer:
/// decode the command stream, issue the equivalent Skia calls.
type SkiaSink(canvas: SKCanvas, fonts: FontStore, clearColor: SKColor) =
    let paint = new SKPaint(IsAntialias = true)
    let typefaces = Dictionary<uint64, SKTypeface>()
    let mutable baseSave = 0

    let typefaceFor (FontId id as fid) : SKTypeface =
        match typefaces.TryGetValue id with
        | true, t -> t
        | _ ->
            let tf =
                match fonts.TryBytes fid with
                | Some bytes -> SKTypeface.FromData(SKData.CreateCopy bytes)
                | None -> SKTypeface.Default
            typefaces.[id] <- tf
            tf

    let resetPaint () =
        paint.Style <- SKPaintStyle.Fill
        paint.Shader <- null
        paint.MaskFilter <- null
        paint.ImageFilter <- null
        paint.ColorFilter <- null
        paint.StrokeWidth <- 0.0f
        paint.StrokeCap <- SKStrokeCap.Butt
        paint.StrokeJoin <- SKStrokeJoin.Miter
        paint.StrokeMiter <- 4.0f
        paint.Color <- SKColors.Black

    /// Configure `paint` for a Paint; returns the shader (if any) so the caller can dispose it.
    let applyPaint (p: Paint) : IDisposable =
        let build (stops: GradientStop array) =
            let ss = stops |> Array.sortBy (fun s -> s.Offset)
            ss |> Array.map (fun s -> Conv.color s.Color), ss |> Array.map (fun s -> s.Offset)
        match p with
        | Solid c ->
            paint.Shader <- null
            paint.Color <- Conv.color c
            null
        | LinearGradient(a, b, stops) when stops.Length > 0 ->
            let colors, pos = build stops
            let sh = SKShader.CreateLinearGradient(SKPoint(a.X, a.Y), SKPoint(b.X, b.Y), colors, pos, SKShaderTileMode.Clamp)
            paint.Color <- SKColors.White
            paint.Shader <- sh
            sh :> IDisposable
        | RadialGradient(c, r, stops) when stops.Length > 0 ->
            let colors, pos = build stops
            let sh = SKShader.CreateRadialGradient(SKPoint(c.X, c.Y), MathF.Max(r, 1e-6f), colors, pos, SKShaderTileMode.Clamp)
            paint.Color <- SKColors.White
            paint.Shader <- sh
            sh :> IDisposable
        | _ ->
            paint.Shader <- null
            paint.Color <- SKColor(200uy, 200uy, 200uy, 255uy)   // images / empty gradients: neutral grey (same as software backend)
            null

    let drawGeom (g: ShapeGeometry) =
        match g with
        | GRect(b, r) ->
            if r.TopLeft <= 0.0f && r.TopRight <= 0.0f && r.BottomRight <= 0.0f && r.BottomLeft <= 0.0f then
                canvas.DrawRect(Conv.rect b, paint)
            else
                use rr = Conv.roundRect b r
                canvas.DrawRoundRect(rr, paint)
        | GCircle(c, r)       -> canvas.DrawCircle(c.X, c.Y, r, paint)
        | GEllipse(c, rx, ry) -> canvas.DrawOval(c.X, c.Y, rx, ry, paint)
        | GLine(a, b, t) ->
            // a capsule: stroke the segment with round caps
            let style, width, cap = paint.Style, paint.StrokeWidth, paint.StrokeCap
            paint.Style <- SKPaintStyle.Stroke
            paint.StrokeWidth <- t
            paint.StrokeCap <- SKStrokeCap.Round
            canvas.DrawLine(a.X, a.Y, b.X, b.Y, paint)
            paint.Style <- style; paint.StrokeWidth <- width; paint.StrokeCap <- cap
        | GPolygon _ | GOpaque _ ->
            use path = Conv.toPath g
            canvas.DrawPath(path, paint)

    interface IRenderSink with
        member _.BeginFrame(_, _) =
            baseSave <- canvas.SaveCount
            canvas.Clear clearColor

        member _.EndFrame() = canvas.RestoreToCount baseSave

        member _.Fill(g, p) =
            resetPaint ()
            use sh = applyPaint p
            drawGeom g

        member _.Stroke(g, st) =
            resetPaint ()
            use sh = applyPaint st.Paint
            paint.Style <- SKPaintStyle.Stroke
            paint.StrokeWidth <- st.Width
            paint.StrokeCap <- (match st.Cap with Butt -> SKStrokeCap.Butt | Round -> SKStrokeCap.Round | Square -> SKStrokeCap.Square)
            paint.StrokeJoin <- (match st.Join with Miter -> SKStrokeJoin.Miter | RoundJoin -> SKStrokeJoin.Round | Bevel -> SKStrokeJoin.Bevel)
            paint.StrokeMiter <- st.MiterLimit
            drawGeom g

        member _.Shadow(g, sh) =
            resetPaint ()
            let sigma = MathF.Max(sh.Blur * 0.5f, 0.01f)        // CSS: sigma = blur radius / 2
            paint.Color <- Conv.color sh.Color
            use blur = SKMaskFilter.CreateBlur(SKBlurStyle.Normal, sigma)
            paint.MaskFilter <- blur
            if not sh.Inner then
                canvas.Save() |> ignore
                canvas.Translate(sh.Offset.X, sh.Offset.Y)
                drawGeom (Conv.inflate g sh.Spread)
                canvas.Restore()
            else
                // blurred complement of the offset/eroded shape, clipped to the shape
                use shape = Conv.toPath g
                use hole = Conv.toPath (Conv.inflate g (-sh.Spread))
                let margin = 4.0f * sigma + MathF.Abs sh.Spread + MathF.Abs sh.Offset.X + MathF.Abs sh.Offset.Y + 8.0f
                let b = shape.Bounds
                use ring = new SKPath(FillType = SKPathFillType.EvenOdd)
                ring.AddRect(SKRect(b.Left - margin, b.Top - margin, b.Right + margin, b.Bottom + margin))
                hole.Offset(sh.Offset.X, sh.Offset.Y)
                ring.AddPath hole
                canvas.Save() |> ignore
                canvas.ClipPath(shape, SKClipOperation.Intersect, true)
                canvas.DrawPath(ring, paint)
                canvas.Restore()

        member _.Text(fid, size, color, glyphs) =
            if glyphs.Length > 0 then
                resetPaint ()
                paint.Color <- Conv.color color
                use font = new SKFont(typefaceFor fid, size)
                font.Edging <- SKFontEdging.SubpixelAntialias
                font.Subpixel <- true
                font.Hinting <- SKFontHinting.None               // unhinted: positions match the shaper and the software glyphs
                use builder = new SKTextBlobBuilder()
                let run = builder.AllocatePositionedRun(font, glyphs.Length)
                let gs = run.GetGlyphSpan()
                let ps = run.GetPositionSpan()
                for i in 0 .. glyphs.Length - 1 do
                    gs.[i] <- uint16 glyphs.[i].GlyphId
                    ps.[i] <- SKPoint(glyphs.[i].X, glyphs.[i].Y)
                use blob = builder.Build()
                canvas.DrawText(blob, 0.0f, 0.0f, paint)

        member _.Image(_, r, opacity) =
            resetPaint ()
            paint.Color <- SKColor(200uy, 200uy, 200uy, byte (MathF.Min(1.0f, MathF.Max(0.0f, opacity)) * 255.0f))
            canvas.DrawRect(Conv.rect r, paint)

        member _.PushClip(r) =
            canvas.Save() |> ignore
            canvas.ClipRect(Conv.rect r, SKClipOperation.Intersect, true)

        member _.PopClip() = canvas.Restore()

        member _.PushTransform(m) =
            canvas.Save() |> ignore
            let mutable mat = SKMatrix.CreateIdentity()
            mat.ScaleX <- m.M11
            mat.SkewY  <- m.M12
            mat.SkewX  <- m.M21
            mat.ScaleY <- m.M22
            mat.TransX <- m.M31
            mat.TransY <- m.M32
            canvas.Concat(&mat)

        member _.PopTransform() = canvas.Restore()

        member _.PushLayer(lp) =
            use p = new SKPaint()
            p.Color <- SKColors.White.WithAlpha(byte (MathF.Min(1.0f, MathF.Max(0.0f, lp.Alpha)) * 255.0f))
            p.BlendMode <- Conv.blend lp.Blend
            if lp.BlurSigma > 0.0f then
                use f = SKImageFilter.CreateBlur(lp.BlurSigma, lp.BlurSigma)
                p.ImageFilter <- f
            if lp.Filter.Length = 20 then
                let m = Array.copy lp.Filter
                let k = Conv.translateScale.Value
                for i in [ 4; 9; 14; 19 ] do m.[i] <- m.[i] * k
                use f = SKColorFilter.CreateColorMatrix m
                p.ColorFilter <- f
            canvas.SaveLayer p |> ignore

        member _.PopLayer() = canvas.Restore()

    interface IDisposable with
        member _.Dispose() =
            paint.Dispose()
            for t in typefaces.Values do t.Dispose()
            typefaces.Clear()
