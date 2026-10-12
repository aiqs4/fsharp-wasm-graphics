namespace FSharp.Graphics.Host

open System
open FSharp.Graphics.Core
open GlyphAura

/// ITextEngine backed by GlyphAura (line breaking, kerning/GPOS) + our own vertical metrics.
/// GlyphAura supplies glyph ids and x positions; this adapter owns line height, letter spacing
/// and alignment so the result is identical on every host.
type GlyphAuraTextEngine(fonts: FontStore) =
    let loaded = Collections.Generic.Dictionary<byte[], FontTables * FontMetrics>(HashIdentity.Reference)

    let tablesFor (bytes: byte[]) =
        match loaded.TryGetValue bytes with
        | true, t -> Some t
        | _ ->
            match FontProvider.load (ReadOnlyMemory<byte> bytes) with
            | Ok t -> loaded.[bytes] <- t; Some t
            | Error _ -> None

    let rectShape (w: float32) (h: float32) : Shapes.ISdfShape =
        { new Shapes.ISdfShape with
            member _.GetDistance(p) =
                let dx = max (0.0f - p.X) (p.X - w)
                let dy = max (0.0f - p.Y) (p.Y - h)
                max dx dy
            member _.GetInterval(y) =
                if y >= 0.0f && y < h then ValueSome(struct (0.0f, w)) else ValueNone
            member _.BoundingBox = { Left = 0.0f; Top = 0.0f; Right = w; Bottom = h } }

    member private this.LayoutParagraph(text: string, style: TextStyle, maxWidth: float32) : TextLayout =
            let empty = { Glyphs = [||]; Width = 0.0f; Height = style.SizePx * style.LineHeight }
            match fonts.TryBytes style.FontId, fonts.TryFont style.FontId with
            | Some bytes, Some ttf when text.Length > 0 ->
                match tablesFor bytes with
                | None -> empty
                | Some (tables, metrics) ->
                    let bounded = maxWidth > 0.0f && maxWidth < 1.0e7f
                    let boxW = if bounded then maxWidth else 1.0e6f
                    let req : LayoutEngine.LayoutRequest =
                        { Text = text.AsMemory()
                          Shape = rectShape boxW 1.0e6f
                          Font = tables
                          Metrics = metrics
                          FontSizePx = style.SizePx
                          Mode = LayoutEngine.Flow
                          Alignment = LayoutEngine.TextAlignment.Left }
                    let res = LayoutEngine.layout req
                    let n = res.Glyphs.Length
                    let src = res.Glyphs.Span.ToArray()
                    let scale = style.SizePx / float32 ttf.UnitsPerEm
                    let natural = float32 (ttf.Ascender - ttf.Descender) * scale
                    let lh = style.SizePx * style.LineHeight
                    let ascent = float32 ttf.Ascender * scale
                    let firstBaseline = (lh - natural) * 0.5f + ascent
                    // group glyphs into lines by GlyphAura's baseline Y
                    let lineOf = Array.zeroCreate<int> n
                    let mutable line = 0
                    for i in 1 .. n - 1 do
                        if src.[i].Y > src.[i - 1].Y + 0.5f then line <- line + 1
                        lineOf.[i] <- line
                    let lines = if n = 0 then 0 else line + 1
                    let x0 = Array.create lines Single.MaxValue
                    let x1 = Array.create lines 0.0f
                    let ls = style.LetterSpacing
                    let xs = Array.init n (fun i ->
                        // letter spacing accumulates within a line
                        let first = (let mutable j = i in (while j > 0 && lineOf.[j - 1] = lineOf.[i] do j <- j - 1); j)
                        src.[i].X + ls * float32 (i - first))
                    let adv = Array.init n (fun i -> float32 (ttf.Advance(int src.[i].GlyphId)) * scale)
                    for i in 0 .. n - 1 do
                        let l = lineOf.[i]
                        x0.[l] <- min x0.[l] xs.[i]
                        x1.[l] <- max x1.[l] (xs.[i] + adv.[i])
                    let contentW = if lines = 0 then 0.0f else (Array.max x1)
                    let width = if bounded && style.Align <> TextAlign.Left then maxWidth else contentW
                    let glyphs =
                        Array.init n (fun i ->
                            let l = lineOf.[i]
                            let lineW = x1.[l]
                            let shift =
                                match style.Align with
                                | TextAlign.Center -> (width - lineW) * 0.5f
                                | TextAlign.Right  -> width - lineW
                                | _ -> 0.0f
                            { GlyphId = uint32 src.[i].GlyphId
                              X = xs.[i] + shift
                              Y = firstBaseline + float32 l * lh
                              AdvanceX = adv.[i] })
                    { Glyphs = glyphs; Width = width; Height = float32 (max 1 lines) * lh }
            | _ -> empty

    interface ITextEngine with
        /// Splits on '\n' (hard line breaks) and stacks the paragraphs.
        member this.Layout(text, style, maxWidth) =
            if text.IndexOf '\n' < 0 then this.LayoutParagraph(text, style, maxWidth)
            else
                let lh = style.SizePx * style.LineHeight
                let mutable y = 0.0f
                let mutable w = 0.0f
                let glyphs = ResizeArray<GlyphPosition>()
                for para in text.Split '\n' do
                    let l = this.LayoutParagraph(para, style, maxWidth)
                    for g in l.Glyphs do glyphs.Add { g with Y = g.Y + y }
                    w <- max w l.Width
                    y <- y + (if para.Length = 0 then lh else l.Height)
                { Glyphs = glyphs.ToArray(); Width = w; Height = y }
