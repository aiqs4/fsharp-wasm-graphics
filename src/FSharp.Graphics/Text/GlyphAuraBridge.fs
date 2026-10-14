/// Production text-rendering bridge between FSharp.Graphics and GlyphAura.
///
/// GlyphAura is a zero-alloc OpenType shaping & SDF layout engine that loads
/// real .ttf/.otf files and produces pixel-perfect glyph positions.  This
/// module wraps its public API so that consumer code never has to open the
/// GlyphAura namespace directly.
///
/// Usage
/// ─────
///   1. Load a font:
///        let font = FontLoader.fromBytes (File.ReadAllBytes "MyFont.ttf") 1UL
///   2. Create a text node:
///        let node = font |> Result.map (fun f -> TextFactory.create f 200.0f 60.0f "Hello" 16.0f)
///   3. Insert it into the scene:
///        Scene.RichText(node, Vec2.Create(10.0f, 10.0f))
namespace FSharp.Graphics.Text

open System
open FSharp.Graphics
open GlyphAura
open GlyphAura.Shapes
open GlyphAura.LayoutEngine

// ─────────────────────────────────────────────────────────────────────────────
// Font handle
// ─────────────────────────────────────────────────────────────────────────────

/// A loaded font ready for GlyphAura text layout.
type LoadedFont =
    { Tables  : FontTables
      Metrics : FontMetrics
      /// Caller-supplied opaque identifier (used for cache keying).
      FontId  : uint64 }

// ─────────────────────────────────────────────────────────────────────────────
// Internal SDF shape helpers
// ─────────────────────────────────────────────────────────────────────────────

module internal SdfHelpers =

    /// Create an axis-aligned rectangle as a GlyphAura ISdfShape.
    /// GlyphAura's built-in primitives are circle/diamond/triangle/parallelogram;
    /// this adds the rectangle needed for normal paragraph text layout.
    let rectangleShape (x: float32) (y: float32) (w: float32) (h: float32) : ISdfShape =
        let bbox : GlyphAura.Shapes.Rectangle = { Left = x; Top = y; Right = x + w; Bottom = y + h }
        { new ISdfShape with
            member _.GetDistance(p) =
                let inside =
                    p.X >= bbox.Left && p.X <= bbox.Right &&
                    p.Y >= bbox.Top  && p.Y <= bbox.Bottom
                if inside then
                    -(min (p.X - bbox.Left)
                          (min (bbox.Right - p.X)
                               (min (p.Y - bbox.Top) (bbox.Bottom - p.Y))))
                else
                    let dx = max 0.0f (max (bbox.Left - p.X) (p.X - bbox.Right))
                    let dy = max 0.0f (max (bbox.Top  - p.Y) (p.Y - bbox.Bottom))
                    sqrt (dx * dx + dy * dy)
            member _.GetInterval(scanY) =
                if scanY >= bbox.Top && scanY <= bbox.Bottom then
                    ValueSome(struct (bbox.Left, bbox.Right))
                else
                    ValueNone
            member _.BoundingBox = bbox }

// ─────────────────────────────────────────────────────────────────────────────
// Font loader
// ─────────────────────────────────────────────────────────────────────────────

module FontLoader =

    /// Load a TrueType / OpenType font from a raw byte array.
    /// Returns `Ok LoadedFont` on success or `Error message` on failure.
    let fromBytes (bytes: byte[]) (fontId: uint64) : Result<LoadedFont, string> =
        let mem = ReadOnlyMemory bytes
        match FontProvider.load mem with
        | Ok (tables, metrics) -> Ok { Tables = tables; Metrics = metrics; FontId = fontId }
        | Error msg            -> Error msg

    /// Convenience overload: load from a file path.
    let fromFile (path: string) (fontId: uint64) : Result<LoadedFont, string> =
        try
            let bytes = IO.File.ReadAllBytes path
            fromBytes bytes fontId
        with ex ->
            Error (sprintf "FontLoader.fromFile: %s" ex.Message)

// ─────────────────────────────────────────────────────────────────────────────
// GlyphAura text node factory
// ─────────────────────────────────────────────────────────────────────────────

module TextFactory =

    /// Create a GlyphAura TextNode with a rectangular layout region of size (w × h).
    /// Position is controlled by the `Vec2` companion in `Scene.RichText`.
    let create
            (font   : LoadedFont)
            (w      : float32)
            (h      : float32)
            (text   : string)
            (sizePx : float32) : GlyphAura.TextNode.TextNode =
        let shape = SdfHelpers.rectangleShape 0.0f 0.0f w h
        GlyphAura.TextNode.create font.Tables font.Metrics font.FontId shape text sizePx

    /// Set the text content of a node, marking it dirty for re-layout.
    let setText (node: GlyphAura.TextNode.TextNode) (text: string) : unit =
        GlyphAura.TextNode.setText node text

    /// Set the font size, marking the node dirty.
    let setFontSize (node: GlyphAura.TextNode.TextNode) (sizePx: float32) : unit =
        GlyphAura.TextNode.setFontSize node sizePx

    /// Resize the text container, replacing the underlying SDF shape.
    let resize (node: GlyphAura.TextNode.TextNode) (w: float32) (h: float32) : unit =
        GlyphAura.TextNode.setShape node (SdfHelpers.rectangleShape 0.0f 0.0f w h)

    /// Perform layout (or serve from cache) and return the GlyphAura DrawList.
    let update
            (node  : GlyphAura.TextNode.TextNode)
            (cache : GlyphAura.ShapeCache.ShapeCache) : GlyphAura.DrawList.DrawList =
        GlyphAura.TextNode.update node cache

// ─────────────────────────────────────────────────────────────────────────────
// DrawList adapter
// ─────────────────────────────────────────────────────────────────────────────

module DrawListAdapter =

    /// Convert a GlyphAura DrawList to the FSharp.Graphics `GlyphPosition` array
    /// used by the encoder's `DrawTextBatch` command.
    let toGlyphPositions (dl: GlyphAura.DrawList.DrawList) : FSharp.Graphics.GlyphPosition array =
        let count = dl.GlyphCount
        if count = 0 then Array.empty
        else
            Array.init count (fun i ->
                { FSharp.Graphics.GlyphPosition.GlyphId  = uint32 dl.GlyphIds.[i]
                  FSharp.Graphics.GlyphPosition.X         = dl.XPositions.[i]
                  FSharp.Graphics.GlyphPosition.Y         = dl.YPositions.[i]
                  FSharp.Graphics.GlyphPosition.AdvanceX  = 0.0f })
