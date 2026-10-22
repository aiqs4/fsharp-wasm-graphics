namespace FSharp.Graphics.SceneDsl

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

/// Pipeline-style modifiers for finished scenes: `node |> shift 10 20 |> fade 0.5f`.
/// Numeric arguments are statically resolved (`int` or `float32`).
[<AutoOpen>]
module Ops =
    let inline shift (dx: ^a) (dy: ^b) (s: Scene) : Scene = Transform(Mat3x2.translation (float32 dx) (float32 dy), s)
    let inline scaleBy (k: ^a) (s: Scene) : Scene = Transform(Mat3x2.scale (float32 k) (float32 k), s)
    let inline rotateDeg (deg: ^a) (ox: ^b) (oy: ^c) (s: Scene) : Scene =
        Transform(Mat3x2.rotateAround (float32 deg) (Vec2.create (float32 ox) (float32 oy)), s)
    let inline fade (a: float32) (s: Scene) : Scene = Opacity(a, s)
    let inline padBy (p: ^a) (s: Scene) : Scene = Padding(Insets.all (float32 p), s)
    let inline padXY (x: ^a) (y: ^b) (s: Scene) : Scene = Padding(Insets.xy (float32 x) (float32 y), s)
    let inline clipTo (x: ^a) (y: ^b) (w: ^c) (h: ^d) (s: Scene) : Scene =
        Clip(Rect.create (float32 x) (float32 y) (float32 w) (float32 h), s)
    let onPress (f: unit -> unit) (s: Scene) : Scene =
        Reactive((fun e -> match e with PointerDown _ -> f (); true | _ -> false), s)
    let onEvent (h: InputEvent -> bool) (s: Scene) : Scene = Reactive(h, s)

    /// Convenience: one-line text label.
    let inline label (size: ^a) (color: Color) (s: string) : Scene =
        Text(s, { TextStyle.defaultStyle (FontId 0UL) with SizePx = float32 size; Color = color; Wrap = false })

    /// Regular polygon / star outline points (inner < outer gives a star).
    let starPoints (cx: float32) (cy: float32) (outer: float32) (inner: float32) (points: int) (rot: float32) : Vec2 list =
        [ for i in 0 .. points * 2 - 1 ->
            let a = rot + float32 i * MathF.PI / float32 points - MathF.PI / 2.0f
            let r = if i % 2 = 0 then outer else inner
            Vec2.create (cx + r * MathF.Cos a) (cy + r * MathF.Sin a) ]

    let polygon (pts: Vec2 list) (fill: Paint) : Scene =
        Shape(Path(PolygonSdf(Array.ofList pts), fill, None, None))

    /// The scene compiler pass - see `Optimize`.
    let compile (s: Scene) : Scene = Optimize.scene s
