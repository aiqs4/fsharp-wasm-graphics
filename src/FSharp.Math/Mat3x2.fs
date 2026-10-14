namespace FSharp.Math

/// 2D affine transform stored as 3×2 matrix.
/// Layout: M11 M12 / M21 M22 / M31 M32 (where M31,M32 is translation)
[<Struct>]
type Mat3x2 = {
    M11: float32; M12: float32
    M21: float32; M22: float32
    M31: float32; M32: float32
}

module Mat3x2 =
    let identity : Mat3x2 = { M11=1.0f; M12=0.0f; M21=0.0f; M22=1.0f; M31=0.0f; M32=0.0f }
    
    let inline translation (dx: float32) (dy: float32) : Mat3x2 =
        { M11=1.0f; M12=0.0f; M21=0.0f; M22=1.0f; M31=dx; M32=dy }
    
    let inline scale (sx: float32) (sy: float32) : Mat3x2 =
        { M11=sx; M12=0.0f; M21=0.0f; M22=sy; M31=0.0f; M32=0.0f }
    
    let inline rotation (radians: float32) : Mat3x2 =
        let c = Scalar.cos radians
        let s = Scalar.sin radians
        { M11=c; M12=s; M21= -s; M22=c; M31=0.0f; M32=0.0f }
    
    let inline rotateAround (degrees: float32) (origin: Vec2) : Mat3x2 =
        let r = Scalar.toRadians degrees
        let c = Scalar.cos r
        let s = Scalar.sin r
        { M11=c; M12=s; M21= -s; M22=c
          M31=origin.X - origin.X*c + origin.Y*s
          M32=origin.Y - origin.X*s - origin.Y*c }
    
    /// Matrix multiplication: result applies a then b (row-vector convention)
    let inline multiply (a: Mat3x2) (b: Mat3x2) : Mat3x2 =
        { M11 = a.M11*b.M11 + a.M12*b.M21
          M12 = a.M11*b.M12 + a.M12*b.M22
          M21 = a.M21*b.M11 + a.M22*b.M21
          M22 = a.M21*b.M12 + a.M22*b.M22
          M31 = a.M31*b.M11 + a.M32*b.M21 + b.M31
          M32 = a.M31*b.M12 + a.M32*b.M22 + b.M32 }
    
    /// Transform a point by this matrix
    let inline transformPoint (m: Mat3x2) (p: Vec2) : Vec2 =
        Vec2.create (p.X*m.M11 + p.Y*m.M21 + m.M31)
                    (p.X*m.M12 + p.Y*m.M22 + m.M32)
    
    /// Transform a vector (direction, no translation)
    let inline transformVector (m: Mat3x2) (v: Vec2) : Vec2 =
        Vec2.create (v.X*m.M11 + v.Y*m.M21)
                    (v.X*m.M12 + v.Y*m.M22)
    
    /// Determinant of the 2×2 linear part
    let inline determinant (m: Mat3x2) : float32 =
        m.M11 * m.M22 - m.M12 * m.M21
    
    /// Invert the matrix. Returns None for singular matrices.
    let invert (m: Mat3x2) : Mat3x2 option =
        let det = determinant m
        if Scalar.approxEq Scalar.Eps det 0.0f then None
        else
            let invDet = 1.0f / det
            Some {
                M11 =  m.M22 * invDet
                M12 = -m.M12 * invDet
                M21 = -m.M21 * invDet
                M22 =  m.M11 * invDet
                M31 = (m.M21 * m.M32 - m.M22 * m.M31) * invDet
                M32 = (m.M12 * m.M31 - m.M11 * m.M32) * invDet
            }
    
    /// Transform an AABB rect, returning the bounding AABB of the transformed corners
    let transformRect (m: Mat3x2) (r: Rect) : Rect =
        let p0 = transformPoint m (Vec2.create r.X r.Y)
        let p1 = transformPoint m (Vec2.create (r.X + r.W) r.Y)
        let p2 = transformPoint m (Vec2.create r.X (r.Y + r.H))
        let p3 = transformPoint m (Vec2.create (r.X + r.W) (r.Y + r.H))
        let minP = Vec2.min' (Vec2.min' p0 p1) (Vec2.min' p2 p3)
        let maxP = Vec2.max' (Vec2.max' p0 p1) (Vec2.max' p2 p3)
        Rect.create minP.X minP.Y (maxP.X - minP.X) (maxP.Y - minP.Y)
    
    let inline approxEq (eps: float32) (a: Mat3x2) (b: Mat3x2) : bool =
        Scalar.approxEq eps a.M11 b.M11 && Scalar.approxEq eps a.M12 b.M12 &&
        Scalar.approxEq eps a.M21 b.M21 && Scalar.approxEq eps a.M22 b.M22 &&
        Scalar.approxEq eps a.M31 b.M31 && Scalar.approxEq eps a.M32 b.M32
