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

/// Rounded rectangle SDF (Inigo Quilez 2D Box SDF)
type RectSdf(bounds: Rect, radius: CornerRadius) =
    let cx = bounds.X + bounds.W * 0.5f
    let cy = bounds.Y + bounds.H * 0.5f
    let hw = bounds.W * 0.5f
    let hh = bounds.H * 0.5f

    interface ISdfShape with
        member _.Sdf(p) =
            let px = p.X - cx
            let py = p.Y - cy
            let qx = abs px
            let qy = abs py
            let r =
                if px >= 0.0f && py >= 0.0f then radius.BottomRight
                elif px < 0.0f && py >= 0.0f then radius.BottomLeft
                elif px >= 0.0f then radius.TopRight
                else radius.TopLeft

            let qx' = qx - hw + r
            let qy' = qy - hh + r
            let dx = Scalar.max' qx' 0.0f
            let dy = Scalar.max' qy' 0.0f
            let outsideDist = SdfMath.len2 dx dy
            let insideDist = Scalar.min' (Scalar.max' qx' qy') 0.0f
            outsideDist + insideDist - r

        member _.Bounds = bounds
        member _.CacheKey = hash (bounds, radius)

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

/// Ellipse SDF
type EllipseSdf(center: Vec2, rx: float32, ry: float32) =
    interface ISdfShape with
        member _.Sdf(p) =
            let px = (p.X - center.X) / rx
            let py = (p.Y - center.Y) / ry
            (SdfMath.len2 px py - 1.0f) * Scalar.min' rx ry

        member _.Bounds =
            Rect.create (center.X - rx) (center.Y - ry) (rx * 2.0f) (ry * 2.0f)

        member _.CacheKey = hash (center, rx, ry)

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

/// Convex polygon SDF
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

/// Helpers for creating shapes
module Shapes =
    let rect bounds fill =
        Rectangle { Bounds = bounds; Radius = CornerRadius.zero; Fill = fill; Stroke = None; Shadow = None }

    let roundedRect bounds radius fill =
        Rectangle { Bounds = bounds; Radius = radius; Fill = fill; Stroke = None; Shadow = None }

    let circle center radius fill =
        Circle { Center = center; Radius = radius; Fill = fill; Stroke = None; Shadow = None }

    let ellipse center rx ry fill =
        Ellipse { Center = center; RadiusX = rx; RadiusY = ry; Fill = fill; Stroke = None; Shadow = None }

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
