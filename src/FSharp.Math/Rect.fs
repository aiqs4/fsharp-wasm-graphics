namespace FSharp.Math

/// Axis-aligned bounding box
[<Struct>]
type Rect = { X: float32; Y: float32; W: float32; H: float32 }

module Rect =
    let inline create (x: float32) (y: float32) (w: float32) (h: float32) : Rect = { X=x; Y=y; W=w; H=h }
    let zero = create 0.0f 0.0f 0.0f 0.0f
    
    let inline right  (r: Rect) = r.X + r.W
    let inline bottom (r: Rect) = r.Y + r.H
    let inline left   (r: Rect) = r.X
    let inline top    (r: Rect) = r.Y
    let inline width  (r: Rect) = r.W
    let inline height (r: Rect) = r.H
    let inline center (r: Rect) = Vec2.create (r.X + r.W * 0.5f) (r.Y + r.H * 0.5f)
    let inline size   (r: Rect) = Vec2.create r.W r.H
    
    let inline contains (pt: Vec2) (r: Rect) =
        pt.X >= r.X && pt.X <= right r && pt.Y >= r.Y && pt.Y <= bottom r
    
    let inline containsRect (inner: Rect) (outer: Rect) =
        inner.X >= outer.X && right inner <= right outer &&
        inner.Y >= outer.Y && bottom inner <= bottom outer
    
    let inline intersect (a: Rect) (b: Rect) =
        let x = Scalar.max' a.X b.X
        let y = Scalar.max' a.Y b.Y
        let r = Scalar.min' (right a) (right b)
        let bt = Scalar.min' (bottom a) (bottom b)
        create x y (Scalar.max' 0.0f (r - x)) (Scalar.max' 0.0f (bt - y))
    
    let inline union (a: Rect) (b: Rect) =
        let x = Scalar.min' a.X b.X
        let y = Scalar.min' a.Y b.Y
        create x y (Scalar.max' (right a) (right b) - x) (Scalar.max' (bottom a) (bottom b) - y)
    
    let inline overlaps (a: Rect) (b: Rect) =
        a.X < right b && right a > b.X && a.Y < bottom b && bottom a > b.Y
    
    let inline expand (amount: float32) (r: Rect) =
        create (r.X - amount) (r.Y - amount) (r.W + amount * 2.0f) (r.H + amount * 2.0f)
    
    let inline offset (dx: float32) (dy: float32) (r: Rect) =
        create (r.X + dx) (r.Y + dy) r.W r.H
    
    let inline isEmpty (r: Rect) = r.W <= 0.0f || r.H <= 0.0f
    
    let inline area (r: Rect) = r.W * r.H
    
    /// Compute bounding rect of a list of points
    let boundsOf (points: Vec2 list) : Rect =
        match points with
        | [] -> zero
        | p :: rest ->
            let mutable minX = p.X
            let mutable minY = p.Y
            let mutable maxX = p.X
            let mutable maxY = p.Y
            for pt in rest do
                if pt.X < minX then minX <- pt.X
                if pt.Y < minY then minY <- pt.Y
                if pt.X > maxX then maxX <- pt.X
                if pt.Y > maxY then maxY <- pt.Y
            create minX minY (maxX - minX) (maxY - minY)
    
    let inline approxEq (eps: float32) (a: Rect) (b: Rect) =
        Scalar.approxEq eps a.X b.X && Scalar.approxEq eps a.Y b.Y &&
        Scalar.approxEq eps a.W b.W && Scalar.approxEq eps a.H b.H
