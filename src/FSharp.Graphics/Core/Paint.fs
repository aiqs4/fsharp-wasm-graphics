/// Paint definitions – the "fill" of any shape or text run.
namespace FSharp.Graphics

// ─────────────────────────────────────────────────────────────────────────────
// ImageId
// ─────────────────────────────────────────────────────────────────────────────

/// Opaque host-managed image handle.
[<Struct>]
type ImageId = ImageId of uint64

// ─────────────────────────────────────────────────────────────────────────────
// GradientStop
// ─────────────────────────────────────────────────────────────────────────────

/// Single colour/offset stop in a gradient.
[<Struct>]
type GradientStop =
    { Offset: float32   // 0 = start, 1 = end
      Color:  Color }

    static member inline Create(offset, color) = { Offset = offset; Color = color }

// ─────────────────────────────────────────────────────────────────────────────
// Paint
// ─────────────────────────────────────────────────────────────────────────────

/// Describes how a shape or glyph is filled.
[<NoComparison>]
type Paint =
    /// Uniform colour fill.
    | Solid          of Color
    /// Two-point linear gradient.
    | LinearGradient of from': Vec2 * to': Vec2 * stops: GradientStop array
    /// Radial gradient expanding from a centre point.
    | RadialGradient of center: Vec2 * radius: float32 * stops: GradientStop array
    /// Image fill mapped to a destination rect with optional opacity.
    | ImagePaint     of imageId: ImageId * srcRect: Rect * opacity: float32

/// Convenience alias – Fill = Paint.
type Fill = Paint

// ─────────────────────────────────────────────────────────────────────────────
// Stroke
// ─────────────────────────────────────────────────────────────────────────────

[<NoComparison>]
type Stroke =
    { Paint:     Paint
      Thickness: float32 }

    static member inline Create(paint, thickness) = { Paint = paint; Thickness = thickness }
    static member inline Solid(color, thickness)  = { Paint = Paint.Solid color; Thickness = thickness }

// ─────────────────────────────────────────────────────────────────────────────
// Shadow
// ─────────────────────────────────────────────────────────────────────────────

/// Drop shadow descriptor.
[<Struct>]
type Shadow =
    { Offset: Vec2
      Blur:   float32
      Spread: float32
      Color:  Color }

    static member inline Create(offset, blur, spread, color) =
        { Offset = offset; Blur = blur; Spread = spread; Color = color }

    static member inline Simple(dx, dy, blur, color) =
        { Offset = Vec2.Create(dx, dy); Blur = blur; Spread = 0.0f; Color = color }
