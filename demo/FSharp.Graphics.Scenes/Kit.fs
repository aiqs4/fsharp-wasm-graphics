/// Tiny authoring vocabulary for the demo scenes (colours, boxes, labels, polygons).
module FSharp.Graphics.Scenes.Kit

open System
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

/// Everything a scene may depend on. Scenes are pure functions of this.
type Ctx = {
    /// Milliseconds since the scene was selected
    Time : float32
    Size : Vec2
    Font : FontId
}

type SceneDef = {
    Id          : string
    Title       : string
    /// What this scene exercises (shown in the HTML index / window title)
    Covers      : string
    Build       : Ctx -> Scene
    /// Animated scenes should be re-rendered every frame; static ones once
    Animated    : bool
}

let hex (h: int) = Color.hex (uint32 h)
let alpha (a: float32) (c: Color) = Color.withAlpha a c
let v x y = Vec2.create x y
let rc x y w h = Rect.create x y w h
let pi = MathF.PI

// palette
let bg       = hex 0x0F172A
let panel    = hex 0x1E293B
let panel2   = hex 0x334155
let ink      = hex 0xF1F5F9
let muted    = hex 0x94A3B8
let blue     = hex 0x3B82F6
let green    = hex 0x22C55E
let amber    = hex 0xF59E0B
let red      = hex 0xEF4444
let violet   = hex 0x8B5CF6
let pink     = hex 0xEC4899
let cyan     = hex 0x06B6D4

let shape s = Shape s
let box x y w h col = Shape (Shapes.rectSimple x y w h (Solid col))
let rbox x y w h r col = Shape (Shapes.roundedRect (rc x y w h) (CornerRadius.all r) (Solid col))
let dot cx cy r col = Shape (Shapes.circle (v cx cy) r (Solid col))
let ring cx cy r w col = Shape (Shapes.withStroke (Stroke.solid col w) (Shapes.circle (v cx cy) r (Solid Color.Transparent)))

let page (ctx: Ctx) = box 0.0f 0.0f ctx.Size.X ctx.Size.Y bg

let polygon (pts: Vec2 list) (fill: Paint) : PrimitiveShape =
    Path(PolygonSdf(Array.ofList pts), fill, None, None)

/// Regular n-gon / star (innerRatio < 1 gives a star)
let star (cx: float32) (cy: float32) (outer: float32) (inner: float32) (points: int) (rot: float32) =
    [ for i in 0 .. points * 2 - 1 ->
        let a = rot + float32 i * pi / float32 points - pi / 2.0f
        let r = if i % 2 = 0 then outer else inner
        v (cx + r * MathF.Cos a) (cy + r * MathF.Sin a) ]

let ngon cx cy r n rot = star cx cy r r n rot |> List.indexed |> List.filter (fun (i, _) -> i % 2 = 0) |> List.map snd

let label (ctx: Ctx) (size: float32) (col: Color) (s: string) =
    Scene.Text(s, { TextStyle.defaultStyle ctx.Font with SizePx = size; Color = col; Wrap = false })

/// Wrapping text in a column of the given width (a Clip region doubles as the text box)
let paragraph (ctx: Ctx) (size: float32) (col: Color) (width: float32) (align: TextAlign) (s: string) =
    Scene.Clip(rc 0.0f 0.0f width 2000.0f,
               Scene.Text(s, { TextStyle.defaultStyle ctx.Font with SizePx = size; Color = col; Wrap = true; Align = align }))

let at x y s = Scene.translate x y s

let title (ctx: Ctx) (t: string) (sub: string) =
    Group [ at 24.0f 18.0f (label ctx 22.0f ink t)
            at 24.0f 46.0f (label ctx 13.0f muted sub) ]

let hstackA gap align children =
    Stack({ Direction = Horizontal; Gap = gap; Alignment = align; MainSize = WrapContent }, children)
let vstackA gap align children =
    Stack({ Direction = Vertical; Gap = gap; Alignment = align; MainSize = WrapContent }, children)

/// small caption under a sample
let caption (ctx: Ctx) x y (s: string) = at x y (label ctx 11.0f muted s)

let stops (xs: (float32 * Color) list) : GradientStop array =
    xs |> List.map (fun (o, c) -> { Offset = o; Color = c }) |> Array.ofList
