namespace FSharp.Graphics.SceneDsl

open FSharp.Math
open FSharp.Graphics.Core

/// Numeric glue: every helper is `inline`, so `int`, `float32` and `float` literals are
/// converted at compile time at the call site - no boxing, no overloads, no runtime cost.
[<AutoOpen>]
module Num =
    /// Any numeric -> float32 (resolved statically).
    let inline f32 (x: ^a) : float32 = float32 x

    let inline v2 (x: ^a) (y: ^b) : Vec2 = Vec2.create (float32 x) (float32 y)
    let inline rect (x: ^a) (y: ^b) (w: ^c) (h: ^d) : Rect =
        Rect.create (float32 x) (float32 y) (float32 w) (float32 h)

    let inline rgb (r: ^a) (g: ^b) (b: ^c) : Color = Color.rgb (byte r) (byte g) (byte b)
    let inline rgba (r: ^a) (g: ^b) (b: ^c) (a: ^d) : Color = Color.rgba (byte r) (byte g) (byte b) (byte a)
    /// 0xRRGGBB
    let inline hex (h: int) : Color = Color.hex (uint32 h)

    /// Gradient stop: `0 ==> red`, `0.5f ==> green`, `1 ==> blue` (offset may be int or float).
    let inline (==>) (offset: ^a) (color: Color) : GradientStop = { Offset = float32 offset; Color = color }
