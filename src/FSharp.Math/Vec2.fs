namespace FSharp.Math

/// 2D vector with float32 components
[<Struct>]
type Vec2 =
    { X: float32; Y: float32 }

    static member inline Create(x: float32, y: float32) = { X = x; Y = y }
    static member inline Zero  = { X = 0.0f; Y = 0.0f }
    static member inline One   = { X = 1.0f; Y = 1.0f }

    static member inline (+) (a: Vec2, b: Vec2) = { X = a.X + b.X; Y = a.Y + b.Y }
    static member inline (-) (a: Vec2, b: Vec2) = { X = a.X - b.X; Y = a.Y - b.Y }
    static member inline (*) (a: Vec2, s: float32) = { X = a.X * s;   Y = a.Y * s   }
    static member inline (*) (s: float32, a: Vec2) = { X = a.X * s;   Y = a.Y * s   }
    static member inline (/) (a: Vec2, s: float32) = { X = a.X / s;   Y = a.Y / s   }

    member inline v.LengthSquared = v.X * v.X + v.Y * v.Y
    member inline v.Length = sqrt v.LengthSquared

    member inline v.Normalize() =
        let len = v.Length
        if len > 1e-7f then { X = v.X / len; Y = v.Y / len }
        else Vec2.Zero

    static member inline Dot   (a: Vec2, b: Vec2) = a.X * b.X + a.Y * b.Y
    static member inline Lerp  (a: Vec2, b: Vec2, t: float32) = a + (b - a) * t
    static member inline Abs   (v: Vec2) = { X = abs v.X; Y = abs v.Y }
    static member inline Max   (a: Vec2, b: Vec2) = { X = max a.X b.X; Y = max a.Y b.Y }
    static member inline Min   (a: Vec2, b: Vec2) = { X = min a.X b.X; Y = min a.Y b.Y }

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
