namespace FSharp.Math

[<Struct>]
type Vec3 = { X: float32; Y: float32; Z: float32 }

module Vec3 =
    let inline create x y z : Vec3 = { X = x; Y = y; Z = z }
    let zero = create 0.0f 0.0f 0.0f
    let one  = create 1.0f 1.0f 1.0f
    let inline add (a: Vec3) (b: Vec3) = create (a.X+b.X) (a.Y+b.Y) (a.Z+b.Z)
    let inline sub (a: Vec3) (b: Vec3) = create (a.X-b.X) (a.Y-b.Y) (a.Z-b.Z)
    let inline scale (s: float32) (v: Vec3) = create (v.X*s) (v.Y*s) (v.Z*s)
    let inline dot (a: Vec3) (b: Vec3) = a.X*b.X + a.Y*b.Y + a.Z*b.Z
    let inline cross (a: Vec3) (b: Vec3) = 
        create (a.Y*b.Z - a.Z*b.Y) (a.Z*b.X - a.X*b.Z) (a.X*b.Y - a.Y*b.X)
    let inline lenSq (v: Vec3) = dot v v
    let inline len (v: Vec3) = Scalar.sqrt (lenSq v)
    let inline normalize (v: Vec3) =
        let l = len v
        if l > Scalar.Eps then scale (1.0f/l) v else zero
    let inline lerp (a: Vec3) (b: Vec3) (t: float32) =
        create (Scalar.lerp a.X b.X t) (Scalar.lerp a.Y b.Y t) (Scalar.lerp a.Z b.Z t)
    let inline approxEq (eps: float32) (a: Vec3) (b: Vec3) =
        Scalar.approxEq eps a.X b.X && Scalar.approxEq eps a.Y b.Y && Scalar.approxEq eps a.Z b.Z
