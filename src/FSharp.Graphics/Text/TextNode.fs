/// Minimal self-contained text subsystem (GlyphAuro compatibility layer).
/// In a full deployment this would be replaced by the fsharp-glyph-aura
/// package; the API surface is intentionally identical.
namespace GlyphAuro

open FSharp.Graphics
open System.Collections.Generic

// ─────────────────────────────────────────────────────────────────────────────
// Font / Glyph types
// ─────────────────────────────────────────────────────────────────────────────

/// Opaque font identifier (host-provided or synthesised by the text engine).
[<Struct>]
type FontId = FontId of uint64

/// Metrics for a single glyph.
[<Struct>]
type GlyphMetrics =
    { GlyphId   : uint32
      AdvanceX  : float32
      BearingX  : float32
      BearingY  : float32
      Width     : float32
      Height    : float32 }

/// Per-character layout result produced by TextNode.layout.
[<Struct>]
type TextGlyphPosition =
    { GlyphId   : uint32
      X         : float32   // baseline-relative pen X
      Y         : float32   // baseline-relative pen Y (positive = up in text coords)
      AdvanceX  : float32
      Width     : float32
      Height    : float32
      CharIndex : int        // index into original string
    }

// ─────────────────────────────────────────────────────────────────────────────
// FontFace – bitmap-free minimal description
// ─────────────────────────────────────────────────────────────────────────────

/// A simplified font description that synthesises metrics from a proportional
/// scale factor.  Real font data would come from a TrueType loader; for the
/// WASM stub the host supplies actual outlines at render time.
type FontFace =
    { Id         : FontId
      Family     : string
      Size       : float32   // point size
      /// Ascender above baseline (positive).
      Ascender   : float32
      /// Descender below baseline (negative).
      Descender  : float32
      LineGap    : float32 }

    member f.LineHeight = f.Ascender - f.Descender + f.LineGap

    /// Synthesise a minimal fixed-pitch font face for testing / fallback.
    static member Monospace(size: float32) =
        { Id        = FontId(hash ("mono", size) |> uint64)
          Family    = "monospace"
          Size      = size
          Ascender  =  size * 0.8f
          Descender = -size * 0.2f
          LineGap   =  size * 0.1f }

    /// Synthesise a proportional font for testing / fallback.
    static member SansSerif(size: float32) =
        { Id        = FontId(hash ("sans", size) |> uint64)
          Family    = "sans-serif"
          Size      = size
          Ascender  =  size * 0.75f
          Descender = -size * 0.25f
          LineGap   =  size * 0.12f }

// ─────────────────────────────────────────────────────────────────────────────
// TextNode
// ─────────────────────────────────────────────────────────────────────────────

/// Text alignment along the main axis.
type TextAlign = Left | Center | Right

/// A styled text run that can be laid out and queried for cursor/hit positions.
[<NoComparison>]
type TextNode =
    { Text      : string
      Font      : FontFace
      Color     : Color
      Align     : TextAlign
      MaxWidth  : float32 voption   // wrap width; ValueNone = no wrap
      MaxHeight : float32 voption   // clipping height; ValueNone = no clip
      LetterSpacing : float32       // extra spacing between glyphs
      LineSpacing   : float32       // multiplier on LineHeight (default 1.0)
    }

module TextNode =

    // ── Glyph metric synthesis ───────────────────────────────────────────────
    // In production these would come from actual font tables.

    /// Synthesise a glyph advance width for a character.
    /// Average proportional to the font size; tab = 4 spaces.
    let private glyphAdvance (font: FontFace) (ch: char) =
        match ch with
        | ' '  -> font.Size * 0.28f
        | '\t' -> font.Size * 0.28f * 4.0f
        | _    ->
            // Latin characters: scale by a typical width factor.
            let baseScale =
                if System.Char.IsUpper ch  then 0.65f
                elif System.Char.IsLetter ch then 0.55f
                elif System.Char.IsDigit ch  then 0.60f
                else 0.40f
            font.Size * baseScale

    // ── Layout ───────────────────────────────────────────────────────────────

    /// Lay out the text in `node` inside optional max dimensions.
    /// Returns the list of glyph positions and the tight bounding rect.
    let layout (node: TextNode) : TextGlyphPosition array * Rect =
        if node.Text.Length = 0 then [||], Rect.Zero
        else
        let text      = node.Text
        let font      = node.Font
        let lineH     = font.LineHeight * node.LineSpacing
        let maxW      = node.MaxWidth |> ValueOption.defaultValue System.Single.PositiveInfinity

        // Break into visual lines.
        let mutable lines : string list = []
        let mutable lineStart = 0
        let mutable penX = 0.0f

        for i = 0 to text.Length - 1 do
            let ch = text.[i]
            if ch = '\n' then
                lines <- text.[lineStart..i-1] :: lines
                lineStart <- i + 1
                penX <- 0.0f
            else
                penX <- penX + glyphAdvance font ch + node.LetterSpacing
                if penX > maxW && i > lineStart then
                    // Soft-wrap: find previous space.
                    let mutable wrapAt = i
                    let mutable k = i - 1
                    while k > lineStart && text.[k] <> ' ' do k <- k - 1
                    if k > lineStart then wrapAt <- k + 1
                    lines <- text.[lineStart..wrapAt-1] :: lines
                    lineStart <- wrapAt
                    penX <- 0.0f

        lines <- text.[lineStart..] :: lines
        let lines = List.rev lines |> List.toArray

        // Build glyph positions.
        let glyphs = ResizeArray<TextGlyphPosition>()
        let mutable maxLineW = 0.0f
        let mutable charIdx = 0

        for lineIdx = 0 to lines.Length - 1 do
            let line = lines.[lineIdx]
            // Measure line width for alignment.
            let mutable lineW = 0.0f
            for ch in line do
                lineW <- lineW + glyphAdvance font ch + node.LetterSpacing
            if lineIdx < lines.Length - 1 then lineW <- lineW - node.LetterSpacing // no trailing
            if lineW > maxLineW then maxLineW <- lineW

            let offsetX =
                match node.Align with
                | Left   -> 0.0f
                | Center -> (maxW - lineW) * 0.5f |> max 0.0f
                | Right  -> max 0.0f (maxW - lineW)

            let mutable penX = offsetX
            let baseY = float32 lineIdx * lineH

            for chIdx = 0 to line.Length - 1 do
                let ch = line.[chIdx]
                let adv = glyphAdvance font ch + node.LetterSpacing
                glyphs.Add {
                    GlyphId   = uint32 ch
                    X         = penX + font.Size * 0.05f   // bearing approximation
                    Y         = baseY
                    AdvanceX  = adv
                    Width     = adv - node.LetterSpacing - font.Size * 0.1f
                    Height    = font.Size
                    CharIndex = charIdx + chIdx
                }
                penX <- penX + adv

            // Account for newline char in original index (if not last line).
            charIdx <- charIdx + line.Length + (if lineIdx < lines.Length - 1 then 1 else 0)

        let totalH = float32 lines.Length * lineH
        let bounds = { X = 0.0f; Y = 0.0f; W = maxLineW; H = totalH }
        glyphs.ToArray(), bounds

    // ── Cursor / hit-test ────────────────────────────────────────────────────

    /// Map a point in local text coordinates to the nearest character index.
    let hitTest (node: TextNode) (point: Vec2) : int =
        let glyphs, bounds = layout node
        if glyphs.Length = 0 then 0
        else
        let lineH = node.Font.LineHeight * node.LineSpacing
        // Find the line.
        let lineIdx = int (point.Y / lineH) |> max 0 |> min (int bounds.H / max 1 (int lineH))
        // Find the closest glyph on that line.
        let lineGlyphs =
            glyphs
            |> Array.filter (fun g -> int (g.Y / lineH) = lineIdx)
        if lineGlyphs.Length = 0 then glyphs.Length
        else
            let mutable best = lineGlyphs.[0].CharIndex
            let mutable bestDist = abs (lineGlyphs.[0].X - point.X)
            for g in lineGlyphs do
                let mid = g.X + g.AdvanceX * 0.5f
                let dist = abs (mid - point.X)
                if dist < bestDist then
                    bestDist <- dist
                    best <- g.CharIndex
            best

    /// Return the pixel rectangle of the character at `charIndex` (for cursors).
    let caretPosition (node: TextNode) (charIndex: int) : Rect =
        let glyphs, _ = layout node
        let lineH = node.Font.LineHeight * node.LineSpacing
        match glyphs |> Array.tryFind (fun g -> g.CharIndex = charIndex) with
        | Some g -> { X = g.X; Y = g.Y; W = 2.0f; H = lineH }
        | None   ->
            // Caret at end.
            if glyphs.Length > 0 then
                let last = glyphs.[glyphs.Length - 1]
                { X = last.X + last.AdvanceX; Y = last.Y; W = 2.0f; H = lineH }
            else
                { X = 0.0f; Y = 0.0f; W = 2.0f; H = lineH }

    /// Return the sequence of highlight rects for [startIndex, endIndex).
    let selectionRects (node: TextNode) (startIndex: int) (endIndex: int) : Rect array =
        let glyphs, _ = layout node
        let lineH = node.Font.LineHeight * node.LineSpacing
        glyphs
        |> Array.filter (fun g -> g.CharIndex >= startIndex && g.CharIndex < endIndex)
        |> Array.map (fun g -> { X = g.X; Y = g.Y; W = g.AdvanceX; H = lineH })

    // ── DrawList production ──────────────────────────────────────────────────

    /// Produce the DrawList for a text node, refreshing ShapeCache as needed.
    let update (node: TextNode) (cache: ShapeCache) : DrawList =
        let (FontId fontIdVal) = node.Font.Id
        let alignInt =
            match node.Align with
            | TextAlign.Left   -> 0
            | TextAlign.Center -> 1
            | TextAlign.Right  -> 2
        let key =
            sprintf "text:%s:%d:%.1f:%d" node.Text (int fontIdVal) node.Font.Size alignInt
        match cache.TryGet key with
        | ValueSome dl -> dl
        | ValueNone ->
            let glyphs, _ = layout node
            let positions : GlyphPosition array =
                glyphs |> Array.map (fun g ->
                    { GlyphId  = g.GlyphId
                      X        = g.X
                      Y        = g.Y
                      AdvanceX = g.AdvanceX })
            let dl : DrawList = [| DrawTextBatch(positions, fontIdVal, node.Font.Size, node.Color) |]
            cache.Put(key, dl)
            dl
