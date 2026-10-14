namespace FSharp.Graphics.Core

open FSharp.Math

/// Font weight
type FontWeight =
    | Thin | ExtraLight | Light | Regular | Medium
    | SemiBold | Bold | ExtraBold | Black

/// Text alignment
type TextAlign = Left | Center | Right | Justify

/// Text overflow handling
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
          Align = Left; Wrap = true; Overflow = Clip; LetterSpacing = 0.0f; LineHeight = 1.2f }

/// Positioned glyph for text rendering
[<Struct>]
type GlyphPosition = {
    GlyphId  : uint32
    X        : float32
    Y        : float32
    AdvanceX : float32
}
