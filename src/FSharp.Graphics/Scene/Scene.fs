/// Public immutable Scene graph.
/// A Scene value is a pure description of the UI – it contains no mutable state.
namespace FSharp.Graphics.Scene

open FSharp.Graphics
open FSharp.Graphics.Primitives
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// Layout helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Direction for a Stack container.
type Direction =
    | Horizontal
    | Vertical
    | ZStack        // children overlaid at the same position

/// Cross-axis alignment inside a Stack.
type Alignment =
    | Start
    | Center
    | End
    | Stretch

// ─────────────────────────────────────────────────────────────────────────────
// Animation / Transition
// ─────────────────────────────────────────────────────────────────────────────

/// Easing function identifier.
type Easing = Linear | EaseIn | EaseOut | EaseInOut | Spring

/// Animation transition descriptor.
[<Struct>]
type Transition =
    { Duration : float32   // seconds
      Delay    : float32   // seconds
      Easing   : Easing }

    static member inline Instant   = { Duration = 0.0f; Delay = 0.0f; Easing = Linear }
    static member inline Default   = { Duration = 0.2f; Delay = 0.0f; Easing = EaseOut }
    static member inline Create(d, e) = { Duration = d; Delay = 0.0f; Easing = e }

// ─────────────────────────────────────────────────────────────────────────────
// Input events (forwarded to Reactive nodes)
// ─────────────────────────────────────────────────────────────────────────────

type InputEvent =
    | MouseMove  of pos: Vec2
    | MouseDown  of pos: Vec2 * button: int
    | MouseUp    of pos: Vec2 * button: int
    | KeyDown    of key: string
    | KeyUp      of key: string
    | TextInput  of text: string
    | Wheel      of delta: float32
    | Focus
    | Blur

// ─────────────────────────────────────────────────────────────────────────────
// Scene  (the public immutable scene graph DU)
// ─────────────────────────────────────────────────────────────────────────────

/// Immutable scene graph.  A Scene value fully describes a frame; the runtime
/// diffs consecutive frames to produce a minimal draw-list update.
[<NoComparison>]
type Scene =
    // ── Leaf nodes ────────────────────────────────────────────────────────────
    /// Vector shape (rectangle, circle, ellipse, or arbitrary SDF path).
    | Shape    of primitive: PrimitiveShape
    /// Text run styled by a TextNode (GlyphAuro synthetic font — no .ttf required).
    | Text     of node: TextNode
    /// Production-quality text node backed by GlyphAura's OpenType engine.
    /// Requires a loaded font (see FSharp.Graphics.Text.FontLoader).
    /// The node carries its own retained-mode cache and dirty flag.
    | RichText of node: GlyphAura.TextNode.TextNode * position: Vec2
    /// Bitmap or decoded image.
    | Image    of imageId: ImageId * destRect: Rect * opacity: float32

    // ── Container nodes ───────────────────────────────────────────────────────
    /// Unordered group – children are rendered in order with no layout.
    | Group    of children: Scene list
    /// Linear (or Z) stack with optional gap and cross-axis alignment.
    | Stack    of direction: Direction * gap: float32 * alignment: Alignment * children: Scene list
    /// Clip children to a rectangular region.
    | Clip     of region: Rect * child: Scene
    /// Add padding around a child.
    | Padding  of insets: Insets * child: Scene
    /// Apply a 2-D affine transform.
    | Transform of matrix: Matrix3x2 * child: Scene
    /// Multiply child opacity.
    | Opacity  of alpha: float32 * child: Scene
    /// Attach input event handlers (hit-test is against child bounds).
    | Reactive of handler: (InputEvent -> bool) * child: Scene
    /// Animate a child with a transition (applied to its position/opacity).
    | Animated of transition: Transition * child: Scene

    // ── Empty / utility ───────────────────────────────────────────────────────
    /// Renders nothing; useful as a placeholder.
    | Empty

// ─────────────────────────────────────────────────────────────────────────────
// Scene helpers
// ─────────────────────────────────────────────────────────────────────────────

module Scene =

    let hstack gap children = Stack(Horizontal, gap, Start, children)
    let vstack gap children = Stack(Vertical,   gap, Start, children)
    let zstack children     = Stack(ZStack,      0.0f, Start, children)

    let pad all child = Padding(Insets.All all, child)
    let padEdges top right bottom left child =
        Padding(Insets.Create(top, right, bottom, left), child)

    let translate dx dy child =
        Transform(Matrix3x2.Translation(dx, dy), child)

    let scale sx sy child =
        Transform(Matrix3x2.Scale(sx, sy), child)

    let rotate angle child =
        Transform(Matrix3x2.Rotation(angle), child)

    let opacity alpha child = Opacity(alpha, child)

    let clip rect child = Clip(rect, child)

    let onClick handler child =
        Reactive((fun e ->
            match e with
            | MouseUp _ -> handler (); true
            | _         -> false), child)
