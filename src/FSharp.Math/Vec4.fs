namespace FSharp.Math

[<Struct>]
type Vec4 = { X: float32; Y: float32; Z: float32; W: float32 }

module Vec4 =
    let inline create x y z w : Vec4 = { X = x; Y = y; Z = z; W = w }
    let zero = create 0.0f 0.0f 0.0f 0.0f
    let one  = create 1.0f 1.0f 1.0f 1.0f
    let inline add (a: Vec4) (b: Vec4) = create (a.X+b.X) (a.Y+b.Y) (a.Z+b.Z) (a.W+b.W)
    let inline sub (a: Vec4) (b: Vec4) = create (a.X-b.X) (a.Y-b.Y) (a.Z-b.Z) (a.W-b.W)
    let inline scale (s: float32) (v: Vec4) = create (v.X*s) (v.Y*s) (v.Z*s) (v.W*s)
    let inline dot (a: Vec4) (b: Vec4) = a.X*b.X + a.Y*b.Y + a.Z*b.Z + a.W*b.W
    let inline lenSq (v: Vec4) = dot v v
    let inline len (v: Vec4) = Scalar.sqrt (lenSq v)
    let inline normalize (v: Vec4) =
        let l = len v
        if l > Scalar.Eps then scale (1.0f/l) v else zero
    let inline lerp (a: Vec4) (b: Vec4) (t: float32) =
        create (Scalar.lerp a.X b.X t) (Scalar.lerp a.Y b.Y t)
               (Scalar.lerp a.Z b.Z t) (Scalar.lerp a.W b.W t)
    let inline approxEq (eps: float32) (a: Vec4) (b: Vec4) =
        Scalar.approxEq eps a.X b.X && Scalar.approxEq eps a.Y b.Y &&
        Scalar.approxEq eps a.Z b.Z && Scalar.approxEq eps a.W b.W
