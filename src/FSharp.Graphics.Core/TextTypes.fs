namespace FSharp.Graphics.Core

open FSharp.Math

/// Font weight
type FontWeight =
    | Thin | ExtraLight | Light | Regular | Medium
    | SemiBold | Bold | ExtraBold | Black

/// Text alignment
[<RequireQualifiedAccess>]
type TextAlign = Left | Center | Right | Justify

/// Text overflow handling
[<RequireQualifiedAccess>]
type TextOverflow = Clip | Ellipsis | Visible

/// Text style configuration
type TextStyle = {
    FontId       : FontId
    SizePx       : float32
    Color        : Color
    Weight       : FontWeight
    Align        : TextAlign
    Wrap         : bool
    Overflow     : TextOverflow
    LetterSpacing: float32
    LineHeight   : float32
}

module TextStyle =
    let defaultStyle fontId =
        { FontId = fontId; SizePx = 16.0f; Color = Color.Black; Weight = Regular
          Align = TextAlign.Left; Wrap = true; Overflow = TextOverflow.Clip; LetterSpacing = 0.0f; LineHeight = 1.2f }

/// Positioned glyph for text rendering
[<Struct>]
type GlyphPosition = {
    GlyphId  : uint32
    X        : float32
    Y        : float32
    AdvanceX : float32
}

/// Result of laying out one text run. Glyph (X, Y) = pen position on the baseline,
/// relative to the top-left of the text box.
type TextLayout = {
    Glyphs : GlyphPosition array
    Width  : float32
    Height : float32
}

/// Host-provided text services. Injected (not referenced) so the core stays BCL-minimal and
/// WASM-clean: desktop plugs in GlyphAura, the browser host may plug in its own shaper.
type ITextEngine =
    /// Lay out `text`, wrapping at `maxWidth` (<= 0 or infinity = unbounded).
    abstract Layout : text: string * style: TextStyle * maxWidth: float32 -> TextLayout

module TextEngine =
    /// Font-less fallback: deterministic fixed-pitch estimate, emits no glyphs.
    let approximate : ITextEngine =
        { new ITextEngine with
            member _.Layout(text, style, _maxWidth) =
                { Glyphs = [||]
                  Width  = float32 text.Length * style.SizePx * 0.55f
                  Height = style.SizePx * style.LineHeight } }
