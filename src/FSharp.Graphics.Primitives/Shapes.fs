namespace FSharp.Graphics.Primitives

open FSharp.Math
open FSharp.Graphics.Core

/// Style for a rectangle shape
type RectStyle = {
    Bounds  : Rect
    Radius  : CornerRadius
    Fill    : Paint
    Stroke  : Stroke option
    Shadow  : Shadow option
}

/// Style for a circle shape
type CircleStyle = {
    Center  : Vec2
    Radius  : float32
    Fill    : Paint
    Stroke  : Stroke option
    Shadow  : Shadow option
}

/// Style for an ellipse shape
type EllipseStyle = {
    Center  : Vec2
    RadiusX : float32
    RadiusY : float32
    Fill    : Paint
    Stroke  : Stroke option
    Shadow  : Shadow option
}

/// A renderable primitive shape
type PrimitiveShape =
    | Rectangle of RectStyle
    | Circle    of CircleStyle
    | Ellipse   of EllipseStyle
    | Path      of sdf: ISdfShape * fill: Paint * stroke: Stroke option * shadow: Shadow option

/// SDF math helpers
module internal SdfMath =
    let inline clamp lo hi x = Scalar.clamp lo hi x
    let inline len2 (x: float32) (y: float32) = Scalar.sqrt (x * x + y * y)

/// Rounded rectangle SDF (exact, per-corner radii; Quilez 2D box)
type RectSdf(bounds: Rect, radius: CornerRadius) =
    let cx = bounds.X + bounds.W * 0.5f
    let cy = bounds.Y + bounds.H * 0.5f
    let hw = bounds.W * 0.5f
    let hh = bounds.H * 0.5f
    let maxR = Scalar.min' hw hh

    interface ISdfShape with
        member _.Sdf(p) =
            let px = p.X - cx
            let py = p.Y - cy
            let r0 =
                if px >= 0.0f && py >= 0.0f then radius.BottomRight
                elif px < 0.0f && py >= 0.0f then radius.BottomLeft
                elif px >= 0.0f then radius.TopRight
                else radius.TopLeft
            let r  = Scalar.clamp 0.0f maxR r0
            let qx = abs px - hw + r
            let qy = abs py - hh + r
            // interior term is min(max(qx,qy),0). The old min(qx,0)+min(qy,0) is only right at
            // the corners: along straight edges it put the zero-contour in the wrong place
            // (half-transparent fills, edge-less strokes, octagonal shadows).
            Scalar.min' (Scalar.max' qx qy) 0.0f
            + SdfMath.len2 (Scalar.max' qx 0.0f) (Scalar.max' qy 0.0f) - r

        member _.Bounds = bounds
        member _.CacheKey = hash (bounds, radius)
        member _.Geometry = GRect(bounds, radius)

/// Circle SDF
type CircleSdf(center: Vec2, radius: float32) =
    interface ISdfShape with
        member _.Sdf(p) =
            let dx = p.X - center.X
            let dy = p.Y - center.Y
            SdfMath.len2 dx dy - radius

        member _.Bounds =
            Rect.create (center.X - radius) (center.Y - radius) (radius * 2.0f) (radius * 2.0f)

        member _.CacheKey = hash (center, radius)
        member _.Geometry = GCircle(center, radius)

/// Ellipse SDF (first-order distance estimate: exact on the boundary, accurate nearby, sqrt only).
/// The old (|p/r|-1)*min(rx,ry) had correct zero-contour but a wrong gradient => wrong AA/stroke width.
type EllipseSdf(center: Vec2, rx: float32, ry: float32) =
    interface ISdfShape with
        member _.Sdf(p) =
            let ax = abs (p.X - center.X)
            let ay = abs (p.Y - center.Y)
            let k0 = SdfMath.len2 (ax / rx) (ay / ry)
            let k1 = SdfMath.len2 (ax / (rx * rx)) (ay / (ry * ry))
            if k1 < 1e-9f then -(Scalar.min' rx ry) else k0 * (k0 - 1.0f) / k1

        member _.Bounds =
            Rect.create (center.X - rx) (center.Y - ry) (rx * 2.0f) (ry * 2.0f)

        member _.CacheKey = hash (center, rx, ry)
        member _.Geometry = GEllipse(center, rx, ry)

/// Line segment SDF
type LineSdf(a: Vec2, b: Vec2, thickness: float32) =
    let halfThick = thickness * 0.5f

    interface ISdfShape with
        member _.Sdf(p) =
            let ab = Vec2.sub b a
            let ap = Vec2.sub p a
            let t = Scalar.clamp 0.0f 1.0f (Vec2.dot ap ab / Vec2.dot ab ab)
            let proj = Vec2.add a (Vec2.scale t ab)
            Vec2.distance p proj - halfThick

        member _.Bounds =
            let minX = Scalar.min' a.X b.X - halfThick
            let minY = Scalar.min' a.Y b.Y - halfThick
            let maxX = Scalar.max' a.X b.X + halfThick
            let maxY = Scalar.max' a.Y b.Y + halfThick
            Rect.create minX minY (maxX - minX) (maxY - minY)

        member _.CacheKey = hash (a, b, thickness)
        member _.Geometry = GLine(a, b, thickness)

/// Simple polygon SDF (either winding)
type PolygonSdf(vertices: Vec2 array) =
    let bounds =
        if vertices.Length = 0 then Rect.zero
        else Rect.boundsOf (Array.toList vertices)

    interface ISdfShape with
        member _.Sdf(p) =
            if vertices.Length < 3 then
                1000.0f
            else
                let mutable d = Vec2.distanceSq p vertices.[0]
                let mutable s = 1.0f
                let n = vertices.Length

                for i in 0 .. n - 1 do
                    let j = (i + 1) % n
                    let e = Vec2.sub vertices.[j] vertices.[i]
                    let w = Vec2.sub p vertices.[i]
                    let b =
                        Vec2.sub w (Vec2.scale (Scalar.clamp 0.0f 1.0f (Vec2.dot w e / Vec2.dot e e)) e)
                    d <- Scalar.min' d (Vec2.dot b b)
                    let c1 = p.Y >= vertices.[i].Y
                    let c2 = p.Y < vertices.[j].Y
                    let c3 = e.X * w.Y > e.Y * w.X

                    if (c1 && c2 && c3) || (not c1 && not c2 && not c3) then
                        s <- -s

                Scalar.sqrt d * s

        member _.Bounds = bounds
        member _.CacheKey = hash vertices
        member _.Geometry = GPolygon vertices

/// Helpers for creating shapes
module Shapes =
    let rect bounds fill =
        Rectangle { Bounds = bounds; Radius = CornerRadius.zero; Fill = fill; Stroke = None; Shadow = None }

    let rectSimple x y w h fill =
        rect (Rect.create x y w h) fill

    let roundedRect bounds radius fill =
        Rectangle { Bounds = bounds; Radius = radius; Fill = fill; Stroke = None; Shadow = None }

    let circle center radius fill =
        Circle { Center = center; Radius = radius; Fill = fill; Stroke = None; Shadow = None }

    let ellipse center rx ry fill =
        Ellipse { Center = center; RadiusX = rx; RadiusY = ry; Fill = fill; Stroke = None; Shadow = None }

    let rectFull(bounds: Rect, radius: CornerRadius, fill: Paint, stroke: Stroke option, shadow: Shadow option) =
        Rectangle { Bounds = bounds; Radius = radius; Fill = fill; Stroke = stroke; Shadow = shadow }

    let circleFull(center: Vec2, radius: float32, fill: Paint, stroke: Stroke option, shadow: Shadow option) =
        Circle { Center = center; Radius = radius; Fill = fill; Stroke = stroke; Shadow = shadow }

    let ellipseFull(center: Vec2, rx: float32, ry: float32, fill: Paint, stroke: Stroke option, shadow: Shadow option) =
        Ellipse { Center = center; RadiusX = rx; RadiusY = ry; Fill = fill; Stroke = stroke; Shadow = shadow }

    let withStroke stroke shape =
        match shape with
        | Rectangle r -> Rectangle { r with Stroke = Some stroke }
        | Circle c    -> Circle { c with Stroke = Some stroke }
        | Ellipse e   -> Ellipse { e with Stroke = Some stroke }
        | Path (s, f, _, sh) -> Path (s, f, Some stroke, sh)

    let withShadow shadow shape =
        match shape with
        | Rectangle r -> Rectangle { r with Shadow = Some shadow }
        | Circle c    -> Circle { c with Shadow = Some shadow }
        | Ellipse e   -> Ellipse { e with Shadow = Some shadow }
        | Path (s, f, st, _) -> Path (s, f, st, Some shadow)

    /// Get the SDF for any primitive shape
    let toSdf (shape: PrimitiveShape) : ISdfShape =
        match shape with
        | Rectangle r -> RectSdf(r.Bounds, r.Radius) :> ISdfShape
        | Circle c    -> CircleSdf(c.Center, c.Radius) :> ISdfShape
        | Ellipse e   -> EllipseSdf(e.Center, e.RadiusX, e.RadiusY) :> ISdfShape
        | Path (s, _, _, _) -> s

    /// Get fill paint for any shape
    let fill (shape: PrimitiveShape) : Paint =
        match shape with
        | Rectangle r -> r.Fill
        | Circle c    -> c.Fill
        | Ellipse e   -> e.Fill
        | Path (_, f, _, _) -> f

    /// Get bounds for any shape
    let bounds (shape: PrimitiveShape) : Rect =
        (toSdf shape).Bounds

/// Convenience accessors
type PrimitiveShape with
    member this.Fill = Shapes.fill this
    member this.ToSdfShape() = Shapes.toSdf this
    member this.Stroke =
        match this with
        | Rectangle r -> r.Stroke
        | Circle c    -> c.Stroke
        | Ellipse e   -> e.Stroke
        | Path (_, _, s, _) -> s
    member this.Shadow =
        match this with
        | Rectangle r -> r.Shadow
        | Circle c    -> c.Shadow
        | Ellipse e   -> e.Shadow
        | Path (_, _, _, s) -> s

