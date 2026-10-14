/// Production text-rendering bridge between FSharp.Graphics and GlyphAura.
///
/// GlyphAura (https://github.com/fsharp-wasm/fsharp-glyph-aura) is a
/// zero-alloc OpenType shaping & SDF layout engine.  Until NuGet packages
/// are published, this module wraps the in-tree GlyphAuro compatibility
/// shim so that the public API is identical to what the real GlyphAura
/// package will provide.  When the NuGet package lands, only this file
/// needs to change — all consumer code stays the same.
///
/// Usage
/// ─────
///   1. Create a text node:
///        let node = TextFactory.create (FontFace.SansSerif 16.0f) 200.0f 60.0f "Hello" 16.0f
///   2. Insert it into the scene:
///        Scene.RichText(node, Vec2.Create(10.0f, 10.0f))
namespace FSharp.Graphics.Text

open FSharp.Graphics
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// Text node factory  (wraps GlyphAuro until real GlyphAura NuGet arrives)
// ─────────────────────────────────────────────────────────────────────────────

module TextFactory =

    /// Create a GlyphAuro TextNode with a maximum layout width of `w`.
    /// Position is controlled by the `Vec2` companion in `Scene.RichText`.
    let create
            (font   : FontFace)
            (w      : float32)
            (_h     : float32)
            (text   : string)
            (sizePx : float32) : TextNode =
        { Text          = text
          Font          = { font with Size = sizePx }
          Color         = Color.Black
          Align         = TextAlign.Left
          MaxWidth      = if w > 0.0f then ValueSome w else ValueNone
          MaxHeight     = ValueNone
          LetterSpacing = 0.0f
          LineSpacing   = 1.0f }

    /// Set the text content of a node, returning a new node.
    let setText (node: TextNode) (text: string) : TextNode =
        { node with Text = text }

    /// Set the font size, returning a new node.
    let setFontSize (node: TextNode) (sizePx: float32) : TextNode =
        { node with Font = { node.Font with Size = sizePx } }

    /// Resize the text container width.
    let resize (node: TextNode) (w: float32) (_h: float32) : TextNode =
        { node with MaxWidth = if w > 0.0f then ValueSome w else ValueNone }

    /// Perform layout and return the glyph positions and bounds.
    let update (node: TextNode) (cache: ShapeCache) : DrawList =
        TextNode.update node cache

// ─────────────────────────────────────────────────────────────────────────────
// DrawList adapter
// ─────────────────────────────────────────────────────────────────────────────

module DrawListAdapter =

    /// Convert a GlyphAuro layout result to the FSharp.Graphics `GlyphPosition` array
    /// used by the encoder's `DrawTextBatch` command.
    let toGlyphPositions (glyphs: TextGlyphPosition array) : GlyphPosition array =
        glyphs |> Array.map (fun g ->
            { GlyphId  = g.GlyphId
              X        = g.X
              Y        = g.Y
              AdvanceX = g.AdvanceX })
