namespace FSharp.Math

/// 4×4 matrix for color transforms and projection
[<Struct>]
type Mat4x4 = {
    M11: float32; M12: float32; M13: float32; M14: float32
    M21: float32; M22: float32; M23: float32; M24: float32
    M31: float32; M32: float32; M33: float32; M34: float32
    M41: float32; M42: float32; M43: float32; M44: float32
}

module Mat4x4 =
    let identity : Mat4x4 = {
        M11=1.0f; M12=0.0f; M13=0.0f; M14=0.0f
        M21=0.0f; M22=1.0f; M23=0.0f; M24=0.0f
        M31=0.0f; M32=0.0f; M33=1.0f; M34=0.0f
        M41=0.0f; M42=0.0f; M43=0.0f; M44=1.0f
    }
    
    let inline transformVec4 (m: Mat4x4) (v: Vec4) : Vec4 =
        Vec4.create
            (v.X*m.M11 + v.Y*m.M21 + v.Z*m.M31 + v.W*m.M41)
            (v.X*m.M12 + v.Y*m.M22 + v.Z*m.M32 + v.W*m.M42)
            (v.X*m.M13 + v.Y*m.M23 + v.Z*m.M33 + v.W*m.M43)
            (v.X*m.M14 + v.Y*m.M24 + v.Z*m.M34 + v.W*m.M44)
    
    let multiply (a: Mat4x4) (b: Mat4x4) : Mat4x4 = {
        M11 = a.M11*b.M11 + a.M12*b.M21 + a.M13*b.M31 + a.M14*b.M41
        M12 = a.M11*b.M12 + a.M12*b.M22 + a.M13*b.M32 + a.M14*b.M42
        M13 = a.M11*b.M13 + a.M12*b.M23 + a.M13*b.M33 + a.M14*b.M43
        M14 = a.M11*b.M14 + a.M12*b.M24 + a.M13*b.M34 + a.M14*b.M44
        M21 = a.M21*b.M11 + a.M22*b.M21 + a.M23*b.M31 + a.M24*b.M41
        M22 = a.M21*b.M12 + a.M22*b.M22 + a.M23*b.M32 + a.M24*b.M42
        M23 = a.M21*b.M13 + a.M22*b.M23 + a.M23*b.M33 + a.M24*b.M43
        M24 = a.M21*b.M14 + a.M22*b.M24 + a.M23*b.M34 + a.M24*b.M44
        M31 = a.M31*b.M11 + a.M32*b.M21 + a.M33*b.M31 + a.M34*b.M41
        M32 = a.M31*b.M12 + a.M32*b.M22 + a.M33*b.M32 + a.M34*b.M42
        M33 = a.M31*b.M13 + a.M32*b.M23 + a.M33*b.M33 + a.M34*b.M43
        M34 = a.M31*b.M14 + a.M32*b.M24 + a.M33*b.M34 + a.M34*b.M44
        M41 = a.M41*b.M11 + a.M42*b.M21 + a.M43*b.M31 + a.M44*b.M41
        M42 = a.M41*b.M12 + a.M42*b.M22 + a.M43*b.M32 + a.M44*b.M42
        M43 = a.M41*b.M13 + a.M42*b.M23 + a.M43*b.M33 + a.M44*b.M43
        M44 = a.M41*b.M14 + a.M42*b.M24 + a.M43*b.M34 + a.M44*b.M44
    }
    
    let inline approxEq (eps: float32) (a: Mat4x4) (b: Mat4x4) : bool =
        Scalar.approxEq eps a.M11 b.M11 && Scalar.approxEq eps a.M12 b.M12 &&
        Scalar.approxEq eps a.M13 b.M13 && Scalar.approxEq eps a.M14 b.M14 &&
        Scalar.approxEq eps a.M21 b.M21 && Scalar.approxEq eps a.M22 b.M22 &&
        Scalar.approxEq eps a.M23 b.M23 && Scalar.approxEq eps a.M24 b.M24 &&
        Scalar.approxEq eps a.M31 b.M31 && Scalar.approxEq eps a.M32 b.M32 &&
        Scalar.approxEq eps a.M33 b.M33 && Scalar.approxEq eps a.M34 b.M34 &&
        Scalar.approxEq eps a.M41 b.M41 && Scalar.approxEq eps a.M42 b.M42 &&
        Scalar.approxEq eps a.M43 b.M43 && Scalar.approxEq eps a.M44 b.M44
