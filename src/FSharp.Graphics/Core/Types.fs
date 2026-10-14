/// Core geometric and colour primitives for FSharp.Graphics.
/// All types are immutable structs; float32 is used throughout for GPU compatibility.
namespace FSharp.Graphics

// ─────────────────────────────────────────────────────────────────────────────
// Vec2
// ─────────────────────────────────────────────────────────────────────────────

/// Immutable 2-D vector with single-precision components.
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

// ─────────────────────────────────────────────────────────────────────────────
// Rect
// ─────────────────────────────────────────────────────────────────────────────

/// Axis-aligned rectangle (origin + size, not two corners).
[<Struct>]
type Rect =
    { X: float32; Y: float32; W: float32; H: float32 }

    static member inline Create(x, y, w, h) = { X = x; Y = y; W = w; H = h }
    static member inline Zero = { X = 0.0f; Y = 0.0f; W = 0.0f; H = 0.0f }
    static member inline Infinite = { X = System.Single.NegativeInfinity; Y = System.Single.NegativeInfinity; W = System.Single.PositiveInfinity; H = System.Single.PositiveInfinity }

    member inline r.Left   = r.X
    member inline r.Top    = r.Y
    member inline r.Right  = r.X + r.W
    member inline r.Bottom = r.Y + r.H
    member inline r.Center = Vec2.Create(r.X + r.W * 0.5f, r.Y + r.H * 0.5f)
    member inline r.Size   = Vec2.Create(r.W, r.H)
    member inline r.IsEmpty = r.W <= 0.0f || r.H <= 0.0f

    member inline r.Contains(p: Vec2) =
        p.X >= r.X && p.X <= r.Right && p.Y >= r.Y && p.Y <= r.Bottom

    static member Intersect (a: Rect) (b: Rect) =
        let x1 = max a.Left  b.Left
        let y1 = max a.Top   b.Top
        let x2 = min a.Right b.Right
        let y2 = min a.Bottom b.Bottom
        { X = x1; Y = y1; W = max 0.0f (x2 - x1); H = max 0.0f (y2 - y1) }

    static member Union (a: Rect) (b: Rect) =
        let x1 = min a.Left  b.Left
        let y1 = min a.Top   b.Top
        let x2 = max a.Right b.Right
        let y2 = max a.Bottom b.Bottom
        { X = x1; Y = y1; W = x2 - x1; H = y2 - y1 }

    member r.Expand(margin: float32) =
        { X = r.X - margin; Y = r.Y - margin
          W = r.W + margin * 2.0f; H = r.H + margin * 2.0f }

    member r.Inset(top: float32, right: float32, bottom: float32, left: float32) =
        { X = r.X + left;  Y = r.Y + top
          W = r.W - left - right; H = r.H - top - bottom }

// ─────────────────────────────────────────────────────────────────────────────
// Color
// ─────────────────────────────────────────────────────────────────────────────

/// Linear RGBA colour, components in [0, 1].
[<Struct>]
type Color =
    { R: float32; G: float32; B: float32; A: float32 }

    static member inline Create(r, g, b, a) = { R = r; G = g; B = b; A = a }
    static member inline Opaque(r, g, b)    = { R = r; G = g; B = b; A = 1.0f }

    static member inline Transparent = { R = 0.0f; G = 0.0f; B = 0.0f; A = 0.0f }
    static member inline Black       = { R = 0.0f; G = 0.0f; B = 0.0f; A = 1.0f }
    static member inline White       = { R = 1.0f; G = 1.0f; B = 1.0f; A = 1.0f }
    static member inline Red         = { R = 1.0f; G = 0.0f; B = 0.0f; A = 1.0f }
    static member inline Green       = { R = 0.0f; G = 0.5f; B = 0.0f; A = 1.0f }
    static member inline Blue        = { R = 0.0f; G = 0.0f; B = 1.0f; A = 1.0f }
    static member inline Yellow      = { R = 1.0f; G = 1.0f; B = 0.0f; A = 1.0f }
    static member inline Cyan        = { R = 0.0f; G = 1.0f; B = 1.0f; A = 1.0f }
    static member inline Magenta     = { R = 1.0f; G = 0.0f; B = 1.0f; A = 1.0f }

    /// Multiply RGB by alpha (straight → premultiplied).
    member inline c.Premultiply() =
        { R = c.R * c.A; G = c.G * c.A; B = c.B * c.A; A = c.A }

    /// Alpha-composite fg over bg (Porter-Duff "over").
    static member Over (fg: Color) (bg: Color) =
        let outA = fg.A + bg.A * (1.0f - fg.A)
        if outA < 1e-7f then Color.Transparent
        else
            { R = (fg.R * fg.A + bg.R * bg.A * (1.0f - fg.A)) / outA
              G = (fg.G * fg.A + bg.G * bg.A * (1.0f - fg.A)) / outA
              B = (fg.B * fg.A + bg.B * bg.A * (1.0f - fg.A)) / outA
              A = outA }

    member c.WithAlpha(a: float32) = { c with A = a }

    /// Convert to packed 8-bit RGBA (sRGB approximation – direct clamp, no gamma).
    member c.ToRgba8() =
        let clamp x = byte (int (x * 255.0f + 0.5f) |> max 0 |> min 255)
        uint32 (clamp c.R) ||| (uint32 (clamp c.G) <<< 8)
        ||| (uint32 (clamp c.B) <<< 16) ||| (uint32 (clamp c.A) <<< 24)

    static member FromRgba8(v: uint32) =
        { R = float32 (v &&& 0xFFu)          / 255.0f
          G = float32 ((v >>> 8)  &&& 0xFFu) / 255.0f
          B = float32 ((v >>> 16) &&& 0xFFu) / 255.0f
          A = float32 ((v >>> 24) &&& 0xFFu) / 255.0f }

    /// Linearly interpolate between two colours.
    static member Lerp (a: Color, b: Color, t: float32) =
        let it = 1.0f - t
        { R = a.R * it + b.R * t
          G = a.G * it + b.G * t
          B = a.B * it + b.B * t
          A = a.A * it + b.A * t }

// ─────────────────────────────────────────────────────────────────────────────
// Insets
// ─────────────────────────────────────────────────────────────────────────────

/// Uniform or per-edge insets (padding / margin).
[<Struct>]
type Insets =
    { Top: float32; Right: float32; Bottom: float32; Left: float32 }

    static member inline All v    = { Top = v;   Right = v;     Bottom = v;      Left = v   }
    static member inline Zero     = { Top = 0.0f; Right = 0.0f; Bottom = 0.0f;   Left = 0.0f }
    static member inline Create(top, right, bottom, left) =
        { Top = top; Right = right; Bottom = bottom; Left = left }
    static member inline Symmetric(horizontal, vertical) =
        { Top = vertical; Right = horizontal; Bottom = vertical; Left = horizontal }

    member inline i.Horizontal = i.Left + i.Right
    member inline i.Vertical   = i.Top  + i.Bottom

// ─────────────────────────────────────────────────────────────────────────────
// Matrix3x2
// ─────────────────────────────────────────────────────────────────────────────

/// Column-major 2-D affine transform (no perspective).
/// Layout: | M11 M12 |   | M21 M22 |   | M31 M32 |
[<Struct>]
type Matrix3x2 =
    { M11: float32; M12: float32
      M21: float32; M22: float32
      M31: float32; M32: float32 }

    static member inline Identity =
        { M11 = 1.0f; M12 = 0.0f
          M21 = 0.0f; M22 = 1.0f
          M31 = 0.0f; M32 = 0.0f }

    static member Translation(dx: float32, dy: float32) =
        { M11 = 1.0f; M12 = 0.0f
          M21 = 0.0f; M22 = 1.0f
          M31 = dx;   M32 = dy   }

    static member Scale(sx: float32, sy: float32) =
        { M11 = sx;   M12 = 0.0f
          M21 = 0.0f; M22 = sy
          M31 = 0.0f; M32 = 0.0f }

    static member Rotation(angle: float32) =
        let c = cos angle
        let s = sin angle
        { M11 = c;  M12 = s
          M21 = -s; M22 = c
          M31 = 0.0f; M32 = 0.0f }

    static member (*) (a: Matrix3x2, b: Matrix3x2) =
        { M11 = a.M11 * b.M11 + a.M12 * b.M21
          M12 = a.M11 * b.M12 + a.M12 * b.M22
          M21 = a.M21 * b.M11 + a.M22 * b.M21
          M22 = a.M21 * b.M12 + a.M22 * b.M22
          M31 = a.M31 * b.M11 + a.M32 * b.M21 + b.M31
          M32 = a.M31 * b.M12 + a.M32 * b.M22 + b.M32 }

    member m.Transform(p: Vec2) =
        Vec2.Create(p.X * m.M11 + p.Y * m.M21 + m.M31,
                    p.X * m.M12 + p.Y * m.M22 + m.M32)

    member m.Determinant() = m.M11 * m.M22 - m.M12 * m.M21

    member m.Invert() =
        let det = m.Determinant()
        if abs det < 1e-7f then ValueNone
        else
            let invDet = 1.0f / det
            ValueSome {
                M11 =  m.M22 * invDet
                M12 = -m.M12 * invDet
                M21 = -m.M21 * invDet
                M22 =  m.M11 * invDet
                M31 = (m.M21 * m.M32 - m.M22 * m.M31) * invDet
                M32 = (m.M12 * m.M31 - m.M11 * m.M32) * invDet }
