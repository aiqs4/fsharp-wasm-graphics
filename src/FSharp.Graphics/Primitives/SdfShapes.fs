/// Concrete SDF shape implementations.
/// Each type implements ISdfShape with an analytic signed-distance function.
namespace FSharp.Graphics.Primitives

open FSharp.Graphics

// ─────────────────────────────────────────────────────────────────────────────
// Helper – inline SDF maths
// ─────────────────────────────────────────────────────────────────────────────

[<AutoOpen>]
module private SdfMath =

    /// Clamp x to [lo, hi].
    let inline clamp (lo: float32) (hi: float32) (x: float32) =
        if x < lo then lo elif x > hi then hi else x

    /// Length of a 2-D vector.
    let inline len2 (x: float32) (y: float32) = sqrt (x * x + y * y)

    /// Component-wise max of a Vec2 and zero.
    let inline vec2max0 (v: Vec2) = Vec2.Create(max 0.0f v.X, max 0.0f v.Y)

// ─────────────────────────────────────────────────────────────────────────────
// RectSdf  (rounded rectangle – r = 0 gives a sharp rectangle)
// ─────────────────────────────────────────────────────────────────────────────

/// Analytic SDF for a (optionally rounded) rectangle.
type RectSdf(bounds: Rect, tl: float32, tr: float32, br: float32, bl: float32) =

    let cx = bounds.X + bounds.W * 0.5f
    let cy = bounds.Y + bounds.H * 0.5f
    let hw = bounds.W * 0.5f
    let hh = bounds.H * 0.5f
    let cacheKey =
        sprintf "rect:%.3f,%.3f,%.3f,%.3f,r:%.2f,%.2f,%.2f,%.2f"
                bounds.X bounds.Y bounds.W bounds.H tl tr br bl

    /// Sharp rectangle (no rounding).
    new(bounds: Rect) = RectSdf(bounds, 0.0f, 0.0f, 0.0f, 0.0f)

    interface ISdfShape with
        member _.CacheKey = cacheKey
        member _.Bounds   = bounds

        member _.Sdf(p: Vec2) =
            // Map to local coordinates centred at the rectangle.
            let qx = abs (p.X - cx) - hw
            let qy = abs (p.Y - cy) - hh

            // Select per-quadrant corner radius.
            let rx = p.X - cx
            let ry = p.Y - cy
            let r =
                if   rx >= 0.0f && ry <= 0.0f then tr   // top-right
                elif rx >= 0.0f && ry >  0.0f then br   // bottom-right
                elif rx <  0.0f && ry >  0.0f then bl   // bottom-left
                else                                tl   // top-left
            let r = clamp 0.0f (min hw hh) r

            let qx' = qx + r
            let qy' = qy + r
            let inside = min qx' 0.0f + min qy' 0.0f
            let outside = len2 (max qx' 0.0f) (max qy' 0.0f)
            inside + outside - r

// ─────────────────────────────────────────────────────────────────────────────
// CircleSdf
// ─────────────────────────────────────────────────────────────────────────────

/// Analytic SDF for a circle.
type CircleSdf(center: Vec2, radius: float32) =
    let cacheKey = sprintf "circle:%.3f,%.3f,r:%.3f" center.X center.Y radius

    interface ISdfShape with
        member _.CacheKey = cacheKey
        member _.Bounds   =
            { X = center.X - radius; Y = center.Y - radius
              W = radius * 2.0f;     H = radius * 2.0f }

        member _.Sdf(p: Vec2) =
            len2 (p.X - center.X) (p.Y - center.Y) - radius

// ─────────────────────────────────────────────────────────────────────────────
// EllipseSdf
// ─────────────────────────────────────────────────────────────────────────────

/// Analytic SDF for an axis-aligned ellipse (Quilez approximation).
type EllipseSdf(center: Vec2, rx: float32, ry: float32) =
    let cacheKey = sprintf "ellipse:%.3f,%.3f,rx:%.3f,ry:%.3f" center.X center.Y rx ry

    interface ISdfShape with
        member _.CacheKey = cacheKey
        member _.Bounds   =
            { X = center.X - rx; Y = center.Y - ry; W = rx * 2.0f; H = ry * 2.0f }

        member _.Sdf(p: Vec2) =
            // Inigo Quilez – exact ellipse SDF
            let ax = abs (p.X - center.X)
            let ay = abs (p.Y - center.Y)
            let rx2 = rx * rx
            let ry2 = ry * ry
            // f(t) = (ax·ry·cos(t))² + (ay·rx·sin(t))² – (rx·ry)²
            // Iterate Newton to find closest point on ellipse.
            let mutable tx = 0.70711f
            let mutable ty = 0.70711f
            for _ = 0 to 2 do
                let x = rx * tx
                let y = ry * ty
                let ex = (rx2 - ry2) * tx * tx * tx / rx
                let ey = (ry2 - rx2) * ty * ty * ty / ry
                let rx' = ax - ex
                let ry' = ay - ey
                let qx = x - ex
                let qy = y - ey
                let r  = len2 qx qy
                let s  = len2 rx' ry'
                tx <- clamp 0.0f 1.0f ((qx * s / r + ex) / rx)
                ty <- clamp 0.0f 1.0f ((qy * s / r + ey) / ry)
                let nlen = len2 tx ty
                tx <- tx / nlen
                ty <- ty / nlen
            let dxSign = if ax > rx * tx then 1.0f else -1.0f
            len2 (ax - rx * tx) (ay - ry * ty) * dxSign

// ─────────────────────────────────────────────────────────────────────────────
// LineSdf  (thick line segment – convenience for strokes)
// ─────────────────────────────────────────────────────────────────────────────

/// Analytic SDF for a thick line segment (capsule shape).
type LineSdf(a: Vec2, b: Vec2, halfThickness: float32) =
    let dir = b - a
    let len = dir.Length
    let cacheKey =
        sprintf "line:%.3f,%.3f-%.3f,%.3f,t:%.3f" a.X a.Y b.X b.Y halfThickness

    interface ISdfShape with
        member _.CacheKey = cacheKey
        member _.Bounds   =
            let pad = halfThickness
            let minX = min a.X b.X - pad
            let minY = min a.Y b.Y - pad
            let maxX = max a.X b.X + pad
            let maxY = max a.Y b.Y + pad
            { X = minX; Y = minY; W = maxX - minX; H = maxY - minY }

        member _.Sdf(p: Vec2) =
            if len < 1e-7f then
                len2 (p.X - a.X) (p.Y - a.Y) - halfThickness
            else
                let t = clamp 0.0f 1.0f (Vec2.Dot(p - a, dir) / (len * len))
                let proj = a + dir * t
                len2 (p.X - proj.X) (p.Y - proj.Y) - halfThickness

// ─────────────────────────────────────────────────────────────────────────────
// PolygonSdf  (convex polygon – exact SDF)
// ─────────────────────────────────────────────────────────────────────────────

/// Exact signed-distance to an arbitrary (convex or concave) polygon.
type PolygonSdf(vertices: Vec2 array) =
    let n = vertices.Length
    let cacheKey =
        vertices
        |> Array.map (fun v -> sprintf "%.2f,%.2f" v.X v.Y)
        |> String.concat ";"
        |> sprintf "poly:[%s]"
    let bounds = BoundingBox.ofPoints vertices

    interface ISdfShape with
        member _.CacheKey = cacheKey
        member _.Bounds   = bounds

        member _.Sdf(p: Vec2) =
            if n = 0 then System.Single.PositiveInfinity
            else
                let mutable d  = Vec2.Dot(p - vertices.[0], p - vertices.[0])
                let mutable s  = 1.0f
                let mutable j  = n - 1
                for i = 0 to n - 1 do
                    let vi = vertices.[i]
                    let vj = vertices.[j]
                    let e  = vj - vi
                    let w  = p  - vi
                    let b  = w - e * (clamp 0.0f 1.0f (Vec2.Dot(w, e) / Vec2.Dot(e, e)))
                    let d' = Vec2.Dot(b, b)
                    if d' < d then d <- d'
                    let c1 = p.Y >= vi.Y
                    let c2 = p.Y <  vj.Y
                    let c3 = e.X * w.Y > e.Y * w.X
                    if (c1 && c2 && c3) || (not c1 && not c2 && not c3) then s <- -s
                    j <- i
                s * sqrt d
