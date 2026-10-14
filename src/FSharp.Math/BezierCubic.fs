namespace FSharp.Math

/// Cubic bezier curve (2D)
[<Struct>]
type BezierCubic = { P0: Vec2; P1: Vec2; P2: Vec2; P3: Vec2 }

module BezierCubic =
    let inline create p0 p1 p2 p3 : BezierCubic = { P0 = p0; P1 = p1; P2 = p2; P3 = p3 }
    
    /// Evaluate point at parameter t ∈ [0,1]
    let inline evaluate (t: float32) (b: BezierCubic) : Vec2 =
        let t2 = t * t
        let t3 = t2 * t
        let mt = 1.0f - t
        let mt2 = mt * mt
        let mt3 = mt2 * mt
        Vec2.add
            (Vec2.add (Vec2.scale mt3 b.P0) (Vec2.scale (3.0f * mt2 * t) b.P1))
            (Vec2.add (Vec2.scale (3.0f * mt * t2) b.P2) (Vec2.scale t3 b.P3))
    
    /// Evaluate tangent at parameter t
    let inline tangent (t: float32) (b: BezierCubic) : Vec2 =
        let mt = 1.0f - t
        let d01 = Vec2.sub b.P1 b.P0
        let d12 = Vec2.sub b.P2 b.P1
        let d23 = Vec2.sub b.P3 b.P2
        let d0112 = Vec2.add (Vec2.scale mt d01) (Vec2.scale t d12)
        let d1223 = Vec2.add (Vec2.scale mt d12) (Vec2.scale t d23)
        Vec2.scale 3.0f (Vec2.add (Vec2.scale mt d0112) (Vec2.scale t d1223))
    
    /// Split curve at parameter t (de Casteljau)
    let split (t: float32) (b: BezierCubic) : BezierCubic * BezierCubic =
        let p01 = Vec2.lerp b.P0 b.P1 t
        let p12 = Vec2.lerp b.P1 b.P2 t
        let p23 = Vec2.lerp b.P2 b.P3 t
        let p012 = Vec2.lerp p01 p12 t
        let p123 = Vec2.lerp p12 p23 t
        let p0123 = Vec2.lerp p012 p123 t
        (create b.P0 p01 p012 p0123, create p0123 p123 p23 b.P3)
    
    /// Approximate bounding box
    let bounds (b: BezierCubic) : Rect =
        Rect.boundsOf [ b.P0; b.P1; b.P2; b.P3 ]
    
    /// Sample N+1 points along the curve for polyline approximation
    let sample (n: int) (b: BezierCubic) : Vec2 array =
        Array.init (n + 1) (fun i ->
            let t = float32 i / float32 n
            evaluate t b)
