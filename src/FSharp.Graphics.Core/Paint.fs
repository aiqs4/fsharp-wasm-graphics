namespace FSharp.Graphics.Core

open FSharp.Math

/// A gradient color stop
[<Struct>]
type GradientStop = { Offset: float32; Color: Color }

/// How a shape or region is filled.
/// Gradient geometry is explicit and lives in the same local space as the shape it fills,
/// so every backend resolves a gradient identically (no hidden dependency on shape bounds).
type Paint =
    | Solid          of Color
    /// t = 0 at `startPoint`, t = 1 at `endPoint`; clamped outside.
    | LinearGradient of startPoint: Vec2 * endPoint: Vec2 * stops: GradientStop array
    /// t = distance from `center` / `radius`; clamped outside.
    | RadialGradient of center: Vec2 * radius: float32 * stops: GradientStop array
    | ImageFill      of ImageId * opacity: float32

module Paint =
    let solid c = Solid c

    /// Linear gradient between two explicit points.
    let linear (a: Vec2) (b: Vec2) stops = LinearGradient(a, b, stops)

    /// Top -> bottom across `r`.
    let linearV (r: Rect) stops =
        LinearGradient(Vec2.create (r.X + r.W * 0.5f) r.Y, Vec2.create (r.X + r.W * 0.5f) (r.Y + r.H), stops)

    /// Left -> right across `r`.
    let linearH (r: Rect) stops =
        LinearGradient(Vec2.create r.X (r.Y + r.H * 0.5f), Vec2.create (r.X + r.W) (r.Y + r.H * 0.5f), stops)

    /// CSS-style angled gradient across `r` (0deg = left->right, 90deg = top->bottom, y down).
    /// The gradient line is sized so the corners of `r` land exactly on t = 0 and t = 1.
    let linearAngle (r: Rect) (angleDeg: float32) stops =
        let a  = Scalar.toRadians angleDeg
        let dx = Scalar.cos a
        let dy = Scalar.sin a
        let half = (abs (r.W * dx) + abs (r.H * dy)) * 0.5f
        let c = Vec2.create (r.X + r.W * 0.5f) (r.Y + r.H * 0.5f)
        LinearGradient(Vec2.create (c.X - dx * half) (c.Y - dy * half),
                       Vec2.create (c.X + dx * half) (c.Y + dy * half), stops)

    let radial (center: Vec2) (radius: float32) stops = RadialGradient(center, radius, stops)

    /// Radial gradient that exactly fills `r` (centre of `r`, radius = half the larger side).
    let radialIn (r: Rect) stops =
        RadialGradient(Vec2.create (r.X + r.W * 0.5f) (r.Y + r.H * 0.5f), Scalar.max' r.W r.H * 0.5f, stops)

    let image id              = ImageFill(id, 1.0f)
    let imageWithOpacity id a = ImageFill(id, a)
