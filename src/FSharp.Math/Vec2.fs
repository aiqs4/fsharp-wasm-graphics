namespace FSharp.Math

/// 2D vector with float32 components
[<Struct>]
type Vec2 = { X: float32; Y: float32 }

module Vec2 =
    let inline create x y : Vec2 = { X = x; Y = y }
    let zero  = create 0.0f 0.0f
    let one   = create 1.0f 1.0f
    let unitX = create 1.0f 0.0f
    let unitY = create 0.0f 1.0f
    let inline add (a: Vec2) (b: Vec2)  = create (a.X + b.X) (a.Y + b.Y)
    let inline sub (a: Vec2) (b: Vec2)  = create (a.X - b.X) (a.Y - b.Y)
    let inline scale (s: float32) (v: Vec2) = create (v.X * s) (v.Y * s)
    let inline mul (a: Vec2) (b: Vec2)  = create (a.X * b.X) (a.Y * b.Y)
    let inline negate (v: Vec2) = create (-v.X) (-v.Y)
    let inline dot (a: Vec2) (b: Vec2)  = a.X * b.X + a.Y * b.Y
    let inline cross (a: Vec2) (b: Vec2) = a.X * b.Y - a.Y * b.X
    let inline lenSq (v: Vec2) = dot v v
    let inline len (v: Vec2) = Scalar.sqrt (lenSq v)
    let inline normalize (v: Vec2) = 
        let l = len v
        if l > Scalar.Eps then scale (1.0f / l) v else zero
    let inline perp (v: Vec2) = create (-v.Y) v.X
    let inline lerp (a: Vec2) (b: Vec2) (t: float32) = create (Scalar.lerp a.X b.X t) (Scalar.lerp a.Y b.Y t)
    let inline distance (a: Vec2) (b: Vec2) = len (sub a b)
    let inline distanceSq (a: Vec2) (b: Vec2) = lenSq (sub a b)
    let inline reflect (v: Vec2) (n: Vec2) = sub v (scale (2.0f * dot v n) n)
    let inline min' (a: Vec2) (b: Vec2) = create (Scalar.min' a.X b.X) (Scalar.min' a.Y b.Y)
    let inline max' (a: Vec2) (b: Vec2) = create (Scalar.max' a.X b.X) (Scalar.max' a.Y b.Y)
    let inline approxEq (eps: float32) (a: Vec2) (b: Vec2) = 
        Scalar.approxEq eps a.X b.X && Scalar.approxEq eps a.Y b.Y
