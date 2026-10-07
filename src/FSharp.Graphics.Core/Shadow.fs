namespace FSharp.Graphics.Core

open FSharp.Math

/// Shadow definition (drop or inner)
[<Struct>]
type Shadow = {
    Offset : Vec2
    Blur   : float32
    Spread : float32
    Color  : Color
    Inner  : bool
}

module Shadow =
    let drop offset blur color =
        { Offset = offset; Blur = blur; Spread = 0.0f; Color = color; Inner = false }
    let inner offset blur color =
        { Offset = offset; Blur = blur; Spread = 0.0f; Color = color; Inner = true }
    let dropSpread offset blur spread color =
        { Offset = offset; Blur = blur; Spread = spread; Color = color; Inner = false }
    let simple ox oy blur color =
        { Offset = Vec2.create ox oy; Blur = blur; Spread = 0.0f; Color = color; Inner = false }
    
    let Simple = simple
