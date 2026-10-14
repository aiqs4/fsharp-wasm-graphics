namespace FSharp.Graphics.Core

open FSharp.Math

/// A gradient color stop
[<Struct>]
type GradientStop = { Offset: float32; Color: Color }

/// How a shape or region is filled
type Paint =
    | Solid          of Color
    | LinearGradient of stops: GradientStop array * angleDeg: float32
    | RadialGradient of stops: GradientStop array * center: Vec2
    | ImageFill      of ImageId * opacity: float32

module Paint =
    let solid c              = Solid c
    let linearV stops        = LinearGradient(stops, 90.0f)
    let linearH stops        = LinearGradient(stops, 0.0f)
    let linear stops angle   = LinearGradient(stops, angle)
    let radial center stops  = RadialGradient(stops, center)
    let image id             = ImageFill(id, 1.0f)
    let imageWithOpacity id a = ImageFill(id, a)
