/// Public primitives API – CornerRadius, PrimitiveShape DU, and Shapes module.
namespace FSharp.Graphics.Primitives

open FSharp.Graphics

// ─────────────────────────────────────────────────────────────────────────────
// CornerRadius
// ─────────────────────────────────────────────────────────────────────────────

/// Per-corner border radius (TL = top-left, TR = top-right, …).
[<Struct>]
type CornerRadius =
    { TL: float32; TR: float32; BR: float32; BL: float32 }

    static member inline All r      = { TL = r;  TR = r;  BR = r;  BL = r  }
    static member inline Zero       = { TL = 0.0f; TR = 0.0f; BR = 0.0f; BL = 0.0f }
    static member inline Create(tl, tr, br, bl) = { TL = tl; TR = tr; BR = br; BL = bl }
    member inline r.IsZero = r.TL = 0.0f && r.TR = 0.0f && r.BR = 0.0f && r.BL = 0.0f

// ─────────────────────────────────────────────────────────────────────────────
// PrimitiveShape  (the unified SDF primitive DU)
// ─────────────────────────────────────────────────────────────────────────────

/// All vector primitives expressed as a discriminated union.
/// Each case maps 1-to-1 to a concrete ISdfShape implementation.
[<NoComparison>]
type PrimitiveShape =
    /// Axis-aligned (optionally rounded) rectangle.
    | Rectangle of bounds: Rect * radius: CornerRadius * fill: Fill * stroke: Stroke option * shadow: Shadow option
    /// Circle defined by centre and radius.
    | Circle    of center: Vec2 * radius: float32 * fill: Fill * stroke: Stroke option * shadow: Shadow option
    /// Axis-aligned ellipse.
    | Ellipse   of center: Vec2 * rx: float32 * ry: float32 * fill: Fill * stroke: Stroke option * shadow: Shadow option
    /// Arbitrary SDF path (plug-in point for Bézier curves, polygons, etc.).
    | Path      of sdf: ISdfShape * fill: Fill * stroke: Stroke option * shadow: Shadow option

    /// Extract the underlying ISdfShape for this primitive.
    member p.ToSdfShape() : ISdfShape =
        match p with
        | Rectangle(bounds, radius, _, _, _) ->
            RectSdf(bounds, radius.TL, radius.TR, radius.BR, radius.BL) :> ISdfShape
        | Circle(center, r, _, _, _) ->
            CircleSdf(center, r) :> ISdfShape
        | Ellipse(center, rx, ry, _, _, _) ->
            EllipseSdf(center, rx, ry) :> ISdfShape
        | Path(sdf, _, _, _) -> sdf

    member p.Fill =
        match p with
        | Rectangle(_, _, f, _, _) | Circle(_, _, f, _, _)
        | Ellipse(_, _, _, f, _, _) | Path(_, f, _, _) -> f

    member p.Stroke =
        match p with
        | Rectangle(_, _, _, s, _) | Circle(_, _, _, s, _)
        | Ellipse(_, _, _, _, s, _) | Path(_, _, s, _) -> s

    member p.Shadow =
        match p with
        | Rectangle(_, _, _, _, sh) | Circle(_, _, _, _, sh)
        | Ellipse(_, _, _, _, _, sh) | Path(_, _, _, sh) -> sh

    member p.Bounds = p.ToSdfShape().Bounds

// ─────────────────────────────────────────────────────────────────────────────
// Shapes  (convenience factory module)
// ─────────────────────────────────────────────────────────────────────────────

/// Idiomatic constructors for PrimitiveShape.  These are the recommended way
/// to create shapes; they mirror Flutter/Clutter conventions.
module Shapes =

    // ── Rectangle ────────────────────────────────────────────────────────────

    let rect x y w h radius fill stroke shadow =
        Rectangle(Rect.Create(x, y, w, h), radius, fill, stroke, shadow)

    let rectSimple x y w h color =
        Rectangle(Rect.Create(x, y, w, h), CornerRadius.Zero, Solid color, None, None)

    let roundedRect x y w h r color =
        Rectangle(Rect.Create(x, y, w, h), CornerRadius.All r, Solid color, None, None)

    // ── Circle ───────────────────────────────────────────────────────────────

    let circle cx cy r fill stroke shadow =
        Circle(Vec2.Create(cx, cy), r, fill, stroke, shadow)

    let circleSimple cx cy r color =
        Circle(Vec2.Create(cx, cy), r, Solid color, None, None)

    // ── Ellipse ──────────────────────────────────────────────────────────────

    let ellipse cx cy rx ry fill stroke shadow =
        Ellipse(Vec2.Create(cx, cy), rx, ry, fill, stroke, shadow)

    let ellipseSimple cx cy rx ry color =
        Ellipse(Vec2.Create(cx, cy), rx, ry, Solid color, None, None)

    // ── Path ─────────────────────────────────────────────────────────────────

    let path sdf fill stroke shadow =
        Path(sdf, fill, stroke, shadow)

    let polygon (vertices: Vec2 array) color =
        Path(PolygonSdf vertices :> ISdfShape, Solid color, None, None)

    // ── With stroke/shadow helpers ────────────────────────────────────────────

    let withStroke (stroke: Stroke) (p: PrimitiveShape) =
        match p with
        | Rectangle(b, r, f, _, sh) -> Rectangle(b, r, f, Some stroke, sh)
        | Circle(c, r, f, _, sh)    -> Circle(c, r, f, Some stroke, sh)
        | Ellipse(c, rx, ry, f, _, sh) -> Ellipse(c, rx, ry, f, Some stroke, sh)
        | Path(s, f, _, sh)         -> Path(s, f, Some stroke, sh)

    let withShadow (shadow: Shadow) (p: PrimitiveShape) =
        match p with
        | Rectangle(b, r, f, st, _) -> Rectangle(b, r, f, st, Some shadow)
        | Circle(c, r, f, st, _)    -> Circle(c, r, f, st, Some shadow)
        | Ellipse(c, rx, ry, f, st, _) -> Ellipse(c, rx, ry, f, st, Some shadow)
        | Path(s, f, st, _)         -> Path(s, f, st, Some shadow)
