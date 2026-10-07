namespace FSharp.Graphics.Host

open System
open System.Collections.Generic
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives

/// Premultiplied float RGBA surface (float keeps layered compositing exact; 8-bit only at export).
type Surface(w: int, h: int) =
    member _.W = w
    member _.H = h
    member val Data : float32[] = Array.zeroCreate (w * h * 4)

module internal RasterMath =
    let inline clamp01 (x: float32) = if x < 0.0f then 0.0f elif x > 1.0f then 1.0f else x

    /// Complementary error function (Abramowitz-Stegun 7.1.26, |err| < 1.5e-7).
    let erfc (x: float32) : float32 =
        let z = MathF.Abs x
        let t = 1.0f / (1.0f + 0.3275911f * z)
        let poly = t * (0.254829592f + t * (-0.284496736f + t * (1.421413741f + t * (-1.453152027f + t * 1.061405429f))))
        let y = poly * MathF.Exp(-z * z)
        if x >= 0.0f then y else 2.0f - y

    let sdfOf (g: ShapeGeometry) : ISdfShape =
        match g with
        | GRect(b, r)        -> RectSdf(b, r) :> ISdfShape
        | GCircle(c, r)      -> CircleSdf(c, r) :> ISdfShape
        | GEllipse(c, rx, ry)-> EllipseSdf(c, rx, ry) :> ISdfShape
        | GPolygon vs        -> PolygonSdf vs :> ISdfShape
        | GLine(a, b, t)     -> LineSdf(a, b, t) :> ISdfShape
        | GOpaque b          -> RectSdf(b, CornerRadius.zero) :> ISdfShape

    type Rgba = (struct (float32 * float32 * float32 * float32))

    let private ofColor (c: Color) : Rgba =
        struct (float32 c.R / 255.0f, float32 c.G / 255.0f, float32 c.B / 255.0f, float32 c.A / 255.0f)

    /// Gradient lookup. Interpolates straight (non-premultiplied) RGBA in gamma space, like
    /// Skia's default and Canvas2D, so backends agree.
    let private ramp (stops: GradientStop array) : float32 -> Rgba =
        if stops.Length = 0 then fun _ -> struct (0.0f, 0.0f, 0.0f, 0.0f)
        else
            let ss = stops |> Array.sortBy (fun s -> s.Offset)
            fun t ->
                if t <= ss.[0].Offset then ofColor ss.[0].Color
                elif t >= ss.[ss.Length - 1].Offset then ofColor ss.[ss.Length - 1].Color
                else
                    let mutable i = 1
                    while ss.[i].Offset < t do i <- i + 1
                    let a = ss.[i - 1]
                    let b = ss.[i]
                    let u = (t - a.Offset) / MathF.Max(b.Offset - a.Offset, 1e-9f)
                    let struct (ar, ag, ab, aa) = ofColor a.Color
                    let struct (br, bg, bb, ba) = ofColor b.Color
                    struct (ar + (br - ar) * u, ag + (bg - ag) * u, ab + (bb - ab) * u, aa + (ba - aa) * u)

    let samplerOf (p: Paint) : Vec2 -> Rgba =
        match p with
        | Solid c -> let v = ofColor c in fun _ -> v
        | LinearGradient(a, b, stops) ->
            let d = Vec2.sub b a
            let inv = 1.0f / MathF.Max(Vec2.dot d d, 1e-12f)
            let r = ramp stops
            fun pt -> r (Vec2.dot (Vec2.sub pt a) d * inv)
        | RadialGradient(c, rad, stops) ->
            let r = ramp stops
            let inv = 1.0f / MathF.Max(rad, 1e-6f)
            fun pt -> r (Vec2.distance pt c * inv)
        | ImageFill _ -> fun _ -> struct (0.78f, 0.78f, 0.78f, 1.0f)   // no image store yet: neutral grey

    /// W3C separable blend function B(cb, cs) on straight colour channels.
    let blendCh (mode: Blend) (cb: float32) (cs: float32) : float32 =
        let screen a b = a + b - a * b
        let hard b s = if s <= 0.5f then 2.0f * b * s else screen b (2.0f * s - 1.0f)
        match mode with
        | Normal -> cs
        | Multiply -> cb * cs
        | Screen -> screen cb cs
        | Overlay -> hard cs cb
        | Darken -> MathF.Min(cb, cs)
        | Lighten -> MathF.Max(cb, cs)
        | ColorDodge -> if cb = 0.0f then 0.0f elif cs >= 1.0f then 1.0f else MathF.Min(1.0f, cb / (1.0f - cs))
        | ColorBurn -> if cb >= 1.0f then 1.0f elif cs <= 0.0f then 0.0f else 1.0f - MathF.Min(1.0f, (1.0f - cb) / cs)
        | HardLight -> hard cb cs
        | SoftLight ->
            if cs <= 0.5f then cb - (1.0f - 2.0f * cs) * cb * (1.0f - cb)
            else
                let d = if cb <= 0.25f then ((16.0f * cb - 12.0f) * cb + 4.0f) * cb else MathF.Sqrt cb
                cb + (2.0f * cs - 1.0f) * (d - cb)
        | Difference -> MathF.Abs(cb - cs)
        | Exclusion -> cb + cs - 2.0f * cb * cs

    /// Three successive box blurs approximate a gaussian (O(n) per pass, any sigma).
    let gaussianBlur (data: float32[]) (w: int) (h: int) (sigma: float32) : unit =
        if sigma > 0.3f then
            let n = 3.0f
            let ideal = MathF.Sqrt(12.0f * sigma * sigma / n + 1.0f)
            let mutable wl = int (MathF.Floor ideal)
            if wl % 2 = 0 then wl <- wl - 1
            let wu = wl + 2
            let m = MathF.Round((12.0f * sigma * sigma - n * float32 (wl * wl) - 4.0f * n * float32 wl - 3.0f * n) / (-4.0f * float32 wl - 4.0f))
            let tmp = Array.zeroCreate<float32> data.Length
            let pass (src: float32[]) (dst: float32[]) (len: int) (count: int) (stride: int) (step: int) (r: int) =
                // blur `count` lines of `len` samples; sample k of line j at j*stride + k*step
                let inv = 1.0f / float32 (2 * r + 1)
                for line in 0 .. count - 1 do
                    for c in 0 .. 3 do
                        let b = line * stride + c
                        let mutable acc = 0.0f
                        for k in 0 .. min r (len - 1) do acc <- acc + src.[b + k * step]
                        for k in 0 .. len - 1 do
                            dst.[b + k * step] <- acc * inv
                            let add = k + r + 1
                            let sub = k - r
                            if add < len then acc <- acc + src.[b + add * step]
                            if sub >= 0 then acc <- acc - src.[b + sub * step]
            for i in 0 .. 2 do
                let size = if float32 i < m then wl else wu
                let r = max 0 ((size - 1) / 2)
                pass data tmp w h (w * 4) 4 r      // horizontal: lines are rows
                pass tmp data h w 4 (w * 4) r      // vertical:   lines are columns

    let applyMatrix (data: float32[]) (m: float32[]) : unit =
        let mutable i = 0
        while i < data.Length do
            let a = data.[i + 3]
            if a > 0.0f then
                let r = data.[i] / a
                let g = data.[i + 1] / a
                let b = data.[i + 2] / a
                let row k = clamp01 (m.[k] * r + m.[k + 1] * g + m.[k + 2] * b + m.[k + 3] * a + m.[k + 4])
                let nr = row 0
                let ng = row 5
                let nb = row 10
                let na = row 15
                data.[i] <- nr * na
                data.[i + 1] <- ng * na
                data.[i + 2] <- nb * na
                data.[i + 3] <- na
            i <- i + 4

    /// Signed-area accumulation of one polygon edge (font-rs style exact-coverage rasteriser).
    let accumulateLine (a: float32[]) (stride: int) (h: int) (px0: float32) (py0: float32) (px1: float32) (py1: float32) =
        if py0 <> py1 then
            let dir, x0, y0, x1, y1 =
                if py0 < py1 then 1.0f, px0, py0, px1, py1 else -1.0f, px1, py1, px0, py0
            let dxdy = (x1 - x0) / (y1 - y0)
            let mutable x = if y0 < 0.0f then x0 - y0 * dxdy else x0
            let ys = if y0 < 0.0f then 0 else int y0
            let ye = min h (int (MathF.Ceiling y1))
            for y in ys .. ye - 1 do
                let ls = y * stride
                let dy = MathF.Min(float32 (y + 1), y1) - MathF.Max(float32 y, y0)
                let xnext = x + dxdy * dy
                let d = dy * dir
                let xa, xb = if x < xnext then x, xnext else xnext, x
                let x0floor = MathF.Floor xa
                let x0i = int x0floor
                let x1ceil = MathF.Ceiling xb
                let x1i = int x1ceil
                if x1i <= x0i + 1 then
                    let xmf = 0.5f * (x + xnext) - x0floor
                    a.[ls + x0i] <- a.[ls + x0i] + d - d * xmf
                    a.[ls + x0i + 1] <- a.[ls + x0i + 1] + d * xmf
                else
                    let s = 1.0f / (xb - xa)
                    let x0f = xa - x0floor
                    let a0 = 0.5f * s * (1.0f - x0f) * (1.0f - x0f)
                    let x1f = xb - x1ceil + 1.0f
                    let am = 0.5f * s * x1f * x1f
                    a.[ls + x0i] <- a.[ls + x0i] + d * a0
                    if x1i = x0i + 2 then
                        a.[ls + x0i + 1] <- a.[ls + x0i + 1] + d * (1.0f - a0 - am)
                    else
                        let a1 = s * (1.5f - x0f)
                        a.[ls + x0i + 1] <- a.[ls + x0i + 1] + d * (a1 - a0)
                        for xi in x0i + 2 .. x1i - 2 do a.[ls + xi] <- a.[ls + xi] + d * s
                        let a2 = a1 + float32 (x1i - x0i - 3) * s
                        a.[ls + x1i - 1] <- a.[ls + x1i - 1] + d * (1.0f - a2 - am)
                    a.[ls + x1i] <- a.[ls + x1i] + d * am
                x <- xnext

open RasterMath

[<Struct>]
type internal ClipEntry = { Inv: Mat3x2; Box: Rect; Scale: float32 }

type internal LayerEntry = { Target: Surface; Parent: Surface; Params: LayerParams; Scale: float32 }

/// Pure-F# reference backend. It is the ground truth other backends are compared against and the
/// fallback that works everywhere (no GPU, no native libs). Quality over speed.
type SoftwareRenderer(width: int, height: int, fonts: FontStore) =
    let root = Surface(width, height)
    let mutable cur = root
    let layers = Stack<LayerEntry>()
    let mutable ctm = Mat3x2.identity
    let mutable inv = Mat3x2.identity
    let mutable scale = 1.0f
    let ctmStack = Stack<struct (Mat3x2 * Mat3x2 * float32)>()
    let mutable clips : ClipEntry list = []
    let mutable bx0 = 0
    let mutable by0 = 0
    let mutable bx1 = width
    let mutable by1 = height
    let clipStack = Stack<struct (ClipEntry list * int * int * int * int)>()

    let setCtm (m: Mat3x2) =
        ctm <- m
        inv <- (match Mat3x2.invert m with Some i -> i | None -> Mat3x2.identity)
        scale <- MathF.Sqrt(MathF.Abs(m.M11 * m.M22 - m.M12 * m.M21))

    let deviceBox (r: Rect) : struct (int * int * int * int) =
        let d = Mat3x2.transformRect ctm r
        struct (max bx0 (int (MathF.Floor d.X)),
                max by0 (int (MathF.Floor d.Y)),
                min bx1 (int (MathF.Ceiling(d.X + d.W))),
                min by1 (int (MathF.Ceiling(d.Y + d.H))))

    /// Coverage (0..1) of the active clip stack at a device-space point. Rotated clips stay exact.
    let clipCoverage (x: float32) (y: float32) : float32 =
        let mutable c = 1.0f
        let mutable l = clips
        while not l.IsEmpty && c > 0.0f do
            let e = l.Head
            let q = Mat3x2.transformPoint e.Inv (Vec2.create x y)
            let dx = MathF.Abs(q.X - (e.Box.X + e.Box.W * 0.5f)) - e.Box.W * 0.5f
            let dy = MathF.Abs(q.Y - (e.Box.Y + e.Box.H * 0.5f)) - e.Box.H * 0.5f
            let ox = MathF.Max(dx, 0.0f)
            let oy = MathF.Max(dy, 0.0f)
            let d = MathF.Sqrt(ox * ox + oy * oy) + MathF.Min(MathF.Max(dx, dy), 0.0f)
            c <- c * clamp01 (0.5f - d * e.Scale)
            l <- l.Tail
        c

    /// Source-over of a straight colour with effective alpha `a` onto the current surface.
    let put (x: int) (y: int) (r: float32) (g: float32) (b: float32) (a: float32) =
        let d = cur.Data
        let i = (y * width + x) * 4
        let ia = 1.0f - a
        d.[i]     <- r * a + d.[i] * ia
        d.[i + 1] <- g * a + d.[i + 1] * ia
        d.[i + 2] <- b * a + d.[i + 2] * ia
        d.[i + 3] <- a + d.[i + 3] * ia

    /// Evaluate `cover` / `color` in local space for every device pixel in `box`.
    let drawLocal (struct (x0, y0, x1, y1): struct (int * int * int * int))
                  (cover: Vec2 -> float32) (color: Vec2 -> Rgba) =
        for y in y0 .. y1 - 1 do
            for x in x0 .. x1 - 1 do
                let fx = float32 x + 0.5f
                let fy = float32 y + 0.5f
                let p = Mat3x2.transformPoint inv (Vec2.create fx fy)
                let c = cover p
                if c > 0.0f then
                    let a = if clips.IsEmpty then c else c * clipCoverage fx fy
                    if a > 0.0f then
                        let struct (r, g, b, al) = color p
                        put x y r g b (al * a)

    let coverage (sdf: ISdfShape) (p: Vec2) = clamp01 (0.5f - sdf.Sdf p * scale)

    let fill (g: ShapeGeometry) (paint: Paint) =
        let sdf = sdfOf g
        drawLocal (deviceBox (Rect.expand 1.0f sdf.Bounds)) (coverage sdf) (samplerOf paint)

    let stroke (g: ShapeGeometry) (st: Stroke) =
        let hw = st.Width * 0.5f
        let bounds = (sdfOf g).Bounds
        let cover : Vec2 -> float32 =
            match g with
            | GRect(b, r) ->
                // exact ring between two rounded rects: keeps mitred corners sharp
                let outerR (x: float32) = if x > 0.0f then x + hw else (match st.Join with Miter -> 0.0f | _ -> hw)
                let innerR (x: float32) = MathF.Max(x - hw, 0.0f)
                let outer = RectSdf(Rect.expand hw b,
                                    { TopLeft = outerR r.TopLeft; TopRight = outerR r.TopRight
                                      BottomRight = outerR r.BottomRight; BottomLeft = outerR r.BottomLeft })
                let innerEmpty = b.W <= st.Width || b.H <= st.Width
                let inner = RectSdf(Rect.expand (-hw) b,
                                    { TopLeft = innerR r.TopLeft; TopRight = innerR r.TopRight
                                      BottomRight = innerR r.BottomRight; BottomLeft = innerR r.BottomLeft })
                fun p -> coverage outer p * (if innerEmpty then 1.0f else 1.0f - coverage inner p)
            | _ ->
                let sdf = sdfOf g
                fun p -> clamp01 (0.5f - (MathF.Abs(sdf.Sdf p) - hw) * scale)
        drawLocal (deviceBox (Rect.expand (hw + 1.0f) bounds)) cover (samplerOf st.Paint)

    let shadow (g: ShapeGeometry) (sh: Shadow) =
        let sdf = sdfOf g
        let sigma = MathF.Max(sh.Blur * 0.5f, 0.01f)       // CSS: sigma = blur radius / 2
        let k = 1.0f / (sigma * 1.41421356f)
        let off = sh.Offset
        let colorFn = samplerOf (Solid sh.Color)
        if not sh.Inner then
            // gaussian-blurred half-plane at signed distance d: 0.5 * erfc(d / (sigma*sqrt 2))
            let box = deviceBox (Rect.expand (MathF.Abs sh.Spread + 3.0f * sigma + 1.0f) (Rect.offset off.X off.Y sdf.Bounds))
            drawLocal box (fun p -> 0.5f * erfc ((sdf.Sdf (Vec2.sub p off) - sh.Spread) * k)) colorFn
        else
            // blurred complement of the (offset, eroded) shape, clipped to the shape itself
            let box = deviceBox (Rect.expand 1.0f sdf.Bounds)
            drawLocal box
                (fun p ->
                    let inside = coverage sdf p
                    if inside <= 0.0f then 0.0f
                    else inside * 0.5f * erfc (-(sdf.Sdf (Vec2.sub p off) + sh.Spread) * k))
                colorFn

    let text (fid: FontId) (size: float32) (color: Color) (glyphs: GlyphPosition array) =
        match fonts.TryFont fid with
        | None -> ()
        | Some f ->
            let s = size / float32 f.UnitsPerEm
            let struct (cr, cg, cb, ca) = (samplerOf (Solid color)) Vec2.zero
            for gl in glyphs do
                let outline = f.Outline(int gl.GlyphId)
                if outline.Length > 0 then
                    let polys =
                        outline |> Array.map (fun c ->
                            Array.init (c.Length / 2) (fun i ->
                                Mat3x2.transformPoint ctm
                                    (Vec2.create (gl.X + c.[2 * i] * s) (gl.Y - c.[2 * i + 1] * s))))
                    let mutable minX = Single.MaxValue
                    let mutable minY = Single.MaxValue
                    let mutable maxX = Single.MinValue
                    let mutable maxY = Single.MinValue
                    for poly in polys do
                        for p in poly do
                            minX <- MathF.Min(minX, p.X); maxX <- MathF.Max(maxX, p.X)
                            minY <- MathF.Min(minY, p.Y); maxY <- MathF.Max(maxY, p.Y)
                    let ox = int (MathF.Floor minX)
                    let oy = int (MathF.Floor minY)
                    let gw = int (MathF.Ceiling maxX) - ox + 1
                    let gh = int (MathF.Ceiling maxY) - oy + 1
                    if gw > 0 && gh > 0 && gw * gh < 4_000_000
                       && ox < bx1 && oy < by1 && ox + gw > bx0 && oy + gh > by0 then
                        let stride = gw + 3
                        let acc = Array.zeroCreate<float32> (stride * gh)
                        for poly in polys do
                            for i in 0 .. poly.Length - 1 do
                                let a = poly.[i]
                                let b = poly.[(i + 1) % poly.Length]
                                accumulateLine acc stride gh (a.X - float32 ox) (a.Y - float32 oy) (b.X - float32 ox) (b.Y - float32 oy)
                        for y in 0 .. gh - 1 do
                            let mutable sum = 0.0f
                            for x in 0 .. gw - 1 do
                                sum <- sum + acc.[y * stride + x]
                                let dx = ox + x
                                let dy = oy + y
                                if dx >= bx0 && dx < bx1 && dy >= by0 && dy < by1 then
                                    let cov = MathF.Min(1.0f, MathF.Abs sum)
                                    if cov > 0.0f then
                                        let cc = if clips.IsEmpty then 1.0f else clipCoverage (float32 dx + 0.5f) (float32 dy + 0.5f)
                                        put dx dy cr cg cb (ca * cov * cc)

    let popLayer () =
        if layers.Count > 0 then
            let e = layers.Pop()
            cur <- e.Parent
            let t = e.Target.Data
            if e.Params.BlurSigma > 0.0f then gaussianBlur t width height (e.Params.BlurSigma * e.Scale)
            if e.Params.Filter.Length = 20 then applyMatrix t e.Params.Filter
            let d = cur.Data
            let alpha = e.Params.Alpha
            let mode = e.Params.Blend
            let mutable i = 0
            while i < t.Length do
                let sa = t.[i + 3] * alpha
                if sa > 0.0f then
                    let ba = d.[i + 3]
                    if mode = Normal || ba <= 0.0f then
                        let ia = 1.0f - sa
                        d.[i]     <- t.[i] * alpha + d.[i] * ia
                        d.[i + 1] <- t.[i + 1] * alpha + d.[i + 1] * ia
                        d.[i + 2] <- t.[i + 2] * alpha + d.[i + 2] * ia
                        d.[i + 3] <- sa + ba * ia
                    else
                        let inva = 1.0f / t.[i + 3]
                        for c in 0 .. 2 do
                            let cs = t.[i + c] * inva
                            let cb = d.[i + c] / ba
                            d.[i + c] <-
                                sa * (1.0f - ba) * cs + sa * ba * blendCh mode cb cs + (1.0f - sa) * ba * cb
                        d.[i + 3] <- sa + ba * (1.0f - sa)
                i <- i + 4

    member _.Surface = root
    member _.Width = width
    member _.Height = height

    /// Export as straight-alpha RGBA8, composited over `background`.
    member _.ToRgba8(background: Color) : byte[] =
        let out = Array.zeroCreate<byte> (width * height * 4)
        let br = float32 background.R / 255.0f
        let bg = float32 background.G / 255.0f
        let bb = float32 background.B / 255.0f
        let ba = float32 background.A / 255.0f
        let d = root.Data
        for p in 0 .. width * height - 1 do
            let i = p * 4
            let a = d.[i + 3]
            let ia = 1.0f - a
            let oa = a + ba * ia
            let q (premulSrc: float32) (bgc: float32) =
                let v = if oa > 0.0f then (premulSrc + bgc * ba * ia) / oa else 0.0f
                byte (MathF.Round(clamp01 v * 255.0f))
            out.[i]     <- q d.[i] br
            out.[i + 1] <- q d.[i + 1] bg
            out.[i + 2] <- q d.[i + 2] bb
            out.[i + 3] <- byte (MathF.Round(clamp01 oa * 255.0f))
        out

    interface IRenderSink with
        member _.BeginFrame(_, _) =
            Array.Clear(root.Data, 0, root.Data.Length)
            cur <- root
            layers.Clear(); ctmStack.Clear(); clipStack.Clear()
            clips <- []
            bx0 <- 0; by0 <- 0; bx1 <- width; by1 <- height
            setCtm Mat3x2.identity
        member _.EndFrame() = while layers.Count > 0 do popLayer ()
        member _.Fill(g, p) = fill g p
        member _.Stroke(g, s) = stroke g s
        member _.Shadow(g, s) = shadow g s
        member _.Text(f, size, c, gs) = text f size c gs
        member _.Image(_, r, opacity) =
            fill (GRect(r, CornerRadius.zero)) (Solid (Color.rgba 200uy 200uy 200uy (byte (clamp01 opacity * 255.0f))))
        member _.PushClip(r) =
            clipStack.Push(struct (clips, bx0, by0, bx1, by1))
            clips <- { Inv = inv; Box = r; Scale = scale } :: clips
            let struct (a, b, c, d) = deviceBox r
            bx0 <- a; by0 <- b; bx1 <- max a c; by1 <- max b d
        member _.PopClip() =
            if clipStack.Count > 0 then
                let struct (c, a, b, x1, y1) = clipStack.Pop()
                clips <- c; bx0 <- a; by0 <- b; bx1 <- x1; by1 <- y1
        member _.PushTransform(m) =
            ctmStack.Push(struct (ctm, inv, scale))
            setCtm (Mat3x2.multiply m ctm)
        member _.PopTransform() =
            if ctmStack.Count > 0 then
                let struct (c, i, s) = ctmStack.Pop()
                ctm <- c; inv <- i; scale <- s
        member _.PushLayer(p) =
            let s = Surface(width, height)
            layers.Push { Target = s; Parent = cur; Params = p; Scale = scale }
            cur <- s
        member _.PopLayer() = popLayer ()
