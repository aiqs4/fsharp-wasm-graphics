namespace FSharp.Graphics.SceneDsl

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

/// How a shape is filled. Gradients are described *relative to the shape* and only turned into
/// concrete `Paint`s (with explicit geometry) when the builder's `Run` knows the final bounds.
type FillSpec =
    | NoFill
    | FSolid  of Color
    | FPaint  of Paint
    | FVGrad  of top: Color * bottom: Color
    | FHGrad  of left: Color * right: Color
    | FAngle  of degrees: float32 * stops: GradientStop array
    | FRadial of stops: GradientStop array

type ShapeKind = KRect | KCircle | KEllipse

/// Accumulated state of a `rect { }` / `circle { }` / `ellipse { }` block.
[<Struct>]
type ShapeState =
    { X: float32; Y: float32; W: float32; H: float32
      R: float32
      Radii: CornerRadius voption
      Fill: FillSpec
      Stroke: Stroke voption
      Shadow: Shadow voption
      Alpha: float32
      Rot: float32
      Dx: float32; Dy: float32 }

module internal Build =

    let resolveFill (b: Rect) (f: FillSpec) : Paint =
        match f with
        | NoFill -> Solid Color.Transparent
        | FSolid c -> Solid c
        | FPaint p -> p
        | FVGrad(a, c) -> Paint.linearV b [| { Offset = 0.0f; Color = a }; { Offset = 1.0f; Color = c } |]
        | FHGrad(a, c) -> Paint.linearH b [| { Offset = 0.0f; Color = a }; { Offset = 1.0f; Color = c } |]
        | FAngle(d, stops) -> Paint.linearAngle b d stops
        | FRadial stops -> Paint.radialIn b stops

    let finish (alpha: float32) (rot: float32) (dx: float32) (dy: float32) (center: Vec2) (s: Scene) : Scene =
        let s1 =
            if rot <> 0.0f then Transform(Mat3x2.rotateAround rot center, s) else s
        let s2 =
            if dx <> 0.0f || dy <> 0.0f then Transform(Mat3x2.translation dx dy, s1) else s1
        if alpha < 1.0f then Opacity(alpha, s2) else s2

    let shape (kind: ShapeKind) (s: ShapeState) : Scene =
        let stroke = ValueOption.toOption s.Stroke
        let shadow = ValueOption.toOption s.Shadow
        match kind with
        | KRect ->
            let b = Rect.create s.X s.Y s.W s.H
            let radius =
                match s.Radii with
                | ValueSome r -> r
                | ValueNone -> CornerRadius.all s.R
            let p = Shape(Shapes.rectFull(b, radius, resolveFill b s.Fill, stroke, shadow))
            finish s.Alpha s.Rot s.Dx s.Dy (Rect.center b) p
        | KCircle ->
            let cx, cy, r =
                if s.R > 0.0f then s.X, s.Y, s.R
                else s.X + s.W * 0.5f, s.Y + s.H * 0.5f, Scalar.min' s.W s.H * 0.5f
            let b = Rect.create (cx - r) (cy - r) (r * 2.0f) (r * 2.0f)
            let p = Shape(Shapes.circleFull(Vec2.create cx cy, r, resolveFill b s.Fill, stroke, shadow))
            finish s.Alpha s.Rot s.Dx s.Dy (Vec2.create cx cy) p
        | KEllipse ->
            let b = Rect.create s.X s.Y s.W s.H
            let p = Shape(Shapes.ellipseFull(Rect.center b, s.W * 0.5f, s.H * 0.5f, resolveFill b s.Fill, stroke, shadow))
            finish s.Alpha s.Rot s.Dx s.Dy (Rect.center b) p

/// `rect { ... }`, `circle { ... }`, `ellipse { ... }`.
///
/// For rectangles/ellipses `at`+`size` (or `bounds`) give the box. For circles `at` is the
/// *centre* and `radius` the radius (or give a box and the circle is inscribed in it).
/// All numeric operations accept `int` or `float32`; everything is `inline`, so the
/// expression compiles down to a handful of record updates.
type ShapeBuilder(kind: ShapeKind) =
    member inline _.Yield(()) : ShapeState =
        { X = 0.0f; Y = 0.0f; W = 0.0f; H = 0.0f; R = 0.0f; Radii = ValueNone
          Fill = NoFill; Stroke = ValueNone; Shadow = ValueNone
          Alpha = 1.0f; Rot = 0.0f; Dx = 0.0f; Dy = 0.0f }

    member _.Run(s: ShapeState) : Scene = Build.shape kind s

    // ---- geometry -------------------------------------------------------------------------
    [<CustomOperation("at")>]
    member inline _.At(s: ShapeState, x: float32, y: float32) = { s with X = x; Y = y }
    [<CustomOperation("at")>]
    member inline _.At(s: ShapeState, x: int, y: int) = { s with X = float32 x; Y = float32 y }

    [<CustomOperation("size")>]
    member inline _.Size(s: ShapeState, w: float32, h: float32) = { s with W = w; H = h }
    [<CustomOperation("size")>]
    member inline _.Size(s: ShapeState, w: int, h: int) = { s with W = float32 w; H = float32 h }

    [<CustomOperation("bounds")>]
    member inline _.Bounds(s: ShapeState, x: float32, y: float32, w: float32, h: float32) = { s with X = x; Y = y; W = w; H = h }
    [<CustomOperation("bounds")>]
    member inline _.Bounds(s: ShapeState, x: int, y: int, w: int, h: int) =
        { s with X = float32 x; Y = float32 y; W = float32 w; H = float32 h }
    [<CustomOperation("bounds")>]
    member inline _.Bounds(s: ShapeState, r: Rect) = { s with X = r.X; Y = r.Y; W = r.W; H = r.H }

    [<CustomOperation("radius")>]
    member inline _.Radius(s: ShapeState, r: float32) = { s with R = r }
    [<CustomOperation("radius")>]
    member inline _.Radius(s: ShapeState, r: int) = { s with R = float32 r }

    [<CustomOperation("radii")>]
    member inline _.Radii(s: ShapeState, tl: float32, tr: float32, br: float32, bl: float32) =
        { s with Radii = ValueSome (CornerRadius.create tl tr br bl) }

    // ---- paint ----------------------------------------------------------------------------
    [<CustomOperation("fill")>]
    member inline _.Fill(s: ShapeState, c: Color) = { s with Fill = FSolid c }
    [<CustomOperation("paint")>]
    member inline _.Paint(s: ShapeState, p: Paint) = { s with Fill = FPaint p }
    /// Top-to-bottom gradient across the shape's own bounds.
    [<CustomOperation("vgradient")>]
    member inline _.VGradient(s: ShapeState, top: Color, bottom: Color) = { s with Fill = FVGrad(top, bottom) }
    [<CustomOperation("hgradient")>]
    member inline _.HGradient(s: ShapeState, left: Color, right: Color) = { s with Fill = FHGrad(left, right) }
    /// CSS-style angle (0 = left->right, 90 = top->bottom) with explicit stops.
    [<CustomOperation("angled")>]
    member inline _.Angled(s: ShapeState, degrees: float32, stops: GradientStop array) = { s with Fill = FAngle(degrees, stops) }
    [<CustomOperation("angled")>]
    member inline _.Angled(s: ShapeState, degrees: int, stops: GradientStop array) = { s with Fill = FAngle(float32 degrees, stops) }
    [<CustomOperation("radial")>]
    member inline _.Radial(s: ShapeState, stops: GradientStop array) = { s with Fill = FRadial stops }

    [<CustomOperation("stroke")>]
    member inline _.Stroke(s: ShapeState, c: Color, width: float32) = { s with Stroke = ValueSome (Stroke.solid c width) }
    [<CustomOperation("stroke")>]
    member inline _.Stroke(s: ShapeState, c: Color, width: int) = { s with Stroke = ValueSome (Stroke.solid c (float32 width)) }

    [<CustomOperation("shadow")>]
    member inline _.Shadow(s: ShapeState, ox: float32, oy: float32, blur: float32, c: Color) =
        { s with Shadow = ValueSome (Shadow.simple ox oy blur c) }
    [<CustomOperation("shadow")>]
    member inline _.Shadow(s: ShapeState, ox: int, oy: int, blur: int, c: Color) =
        { s with Shadow = ValueSome (Shadow.simple (float32 ox) (float32 oy) (float32 blur) c) }

    // ---- compositing ----------------------------------------------------------------------
    [<CustomOperation("opacity")>]
    member inline _.Opacity(s: ShapeState, a: float32) = { s with Alpha = a }
    /// Rotate (degrees) around the shape's centre.
    [<CustomOperation("rotate")>]
    member inline _.Rotate(s: ShapeState, deg: float32) = { s with Rot = deg }
    [<CustomOperation("rotate")>]
    member inline _.Rotate(s: ShapeState, deg: int) = { s with Rot = float32 deg }
    [<CustomOperation("translate")>]
    member inline _.Translate(s: ShapeState, dx: float32, dy: float32) = { s with Dx = dx; Dy = dy }
    [<CustomOperation("translate")>]
    member inline _.Translate(s: ShapeState, dx: int, dy: int) = { s with Dx = float32 dx; Dy = float32 dy }

// ---------------------------------------------------------------------------------------------

/// Accumulated state of a `text { }` block.
[<Struct>]
type TextState =
    { Content: string
      Size: float32
      Color: Color
      Weight: FontWeight
      Align: TextAlign
      Wrap: bool
      Font: FontId
      Spacing: float32
      LineHeight: float32
      MaxWidth: float32
      X: float32; Y: float32
      Alpha: float32 }

/// `text { content "Hi"; size 14; color white; bold; at 10 10 }`
type TextBuilder() =
    member inline _.Yield(()) : TextState =
        { Content = ""; Size = 16.0f; Color = Color.Black; Weight = Regular; Align = TextAlign.Left
          Wrap = false; Font = FontId 0UL; Spacing = 0.0f; LineHeight = 1.2f; MaxWidth = 0.0f
          X = 0.0f; Y = 0.0f; Alpha = 1.0f }

    member _.Run(s: TextState) : Scene =
        let style : TextStyle =
            { FontId = s.Font; SizePx = s.Size; Color = s.Color; Weight = s.Weight; Align = s.Align
              Wrap = s.Wrap || s.MaxWidth > 0.0f; Overflow = TextOverflow.Clip
              LetterSpacing = s.Spacing; LineHeight = s.LineHeight }
        let t = Text(s.Content, style)
        // a Clip region doubles as the text box that wrapping text wraps inside
        let boxed = if s.MaxWidth > 0.0f then Clip(Rect.create 0.0f 0.0f s.MaxWidth 4000.0f, t) else t
        let placed = if s.X <> 0.0f || s.Y <> 0.0f then Transform(Mat3x2.translation s.X s.Y, boxed) else boxed
        if s.Alpha < 1.0f then Opacity(s.Alpha, placed) else placed

    [<CustomOperation("content")>]
    member inline _.Content(s: TextState, t: string) = { s with Content = t }
    [<CustomOperation("size")>]
    member inline _.Size(s: TextState, px: float32) = { s with Size = px }
    [<CustomOperation("size")>]
    member inline _.Size(s: TextState, px: int) = { s with Size = float32 px }
    [<CustomOperation("color")>]
    member inline _.Color(s: TextState, c: Color) = { s with Color = c }
    [<CustomOperation("weight")>]
    member inline _.Weight(s: TextState, w: FontWeight) = { s with Weight = w }
    [<CustomOperation("bold")>]
    member inline _.Bold(s: TextState) = { s with Weight = Bold }
    [<CustomOperation("align")>]
    member inline _.Align(s: TextState, a: TextAlign) = { s with Align = a }
    [<CustomOperation("font")>]
    member inline _.Font(s: TextState, f: FontId) = { s with Font = f }
    [<CustomOperation("spacing")>]
    member inline _.Spacing(s: TextState, v: float32) = { s with Spacing = v }
    [<CustomOperation("lineHeight")>]
    member inline _.LineHeight(s: TextState, v: float32) = { s with LineHeight = v }
    /// Wrap inside a box of this width.
    [<CustomOperation("maxWidth")>]
    member inline _.MaxWidth(s: TextState, w: float32) = { s with MaxWidth = w }
    [<CustomOperation("maxWidth")>]
    member inline _.MaxWidth(s: TextState, w: int) = { s with MaxWidth = float32 w }
    [<CustomOperation("at")>]
    member inline _.At(s: TextState, x: float32, y: float32) = { s with X = x; Y = y }
    [<CustomOperation("at")>]
    member inline _.At(s: TextState, x: int, y: int) = { s with X = float32 x; Y = float32 y }
    [<CustomOperation("opacity")>]
    member inline _.Opacity(s: TextState, a: float32) = { s with Alpha = a }

// ---------------------------------------------------------------------------------------------

/// Accumulated state of a `column { }` / `row { }` / `layers { }` block.
type StackState =
    { Gap: float32
      Align: CrossAxisAlignment
      Pad: Insets
      Items: Scene list          // reversed while building
      X: float32; Y: float32
      Handler: (InputEvent -> bool) voption
      Alpha: float32 }

/// `column { gap 8; align Center; pad 12; child a; child b; children others; onClick save }`
type StackBuilder(dir: Direction) =
    member inline _.Yield(()) : StackState =
        { Gap = 0.0f; Align = Start; Pad = Insets.zero; Items = []; X = 0.0f; Y = 0.0f
          Handler = ValueNone; Alpha = 1.0f }

    member _.Run(s: StackState) : Scene =
        let body = Stack({ Direction = dir; Gap = s.Gap; Alignment = s.Align; MainSize = WrapContent }, List.rev s.Items)
        let padded = if s.Pad = Insets.zero then body else Padding(s.Pad, body)
        let reactive =
            match s.Handler with
            | ValueSome h -> Reactive(h, padded)
            | ValueNone -> padded
        let placed = if s.X <> 0.0f || s.Y <> 0.0f then Transform(Mat3x2.translation s.X s.Y, reactive) else reactive
        if s.Alpha < 1.0f then Opacity(s.Alpha, placed) else placed

    [<CustomOperation("gap")>]
    member inline _.Gap(s: StackState, g: float32) = { s with Gap = g }
    [<CustomOperation("gap")>]
    member inline _.Gap(s: StackState, g: int) = { s with Gap = float32 g }
    [<CustomOperation("align")>]
    member inline _.Align(s: StackState, a: CrossAxisAlignment) = { s with Align = a }
    [<CustomOperation("pad")>]
    member inline _.Pad(s: StackState, p: float32) = { s with Pad = Insets.all p }
    [<CustomOperation("pad")>]
    member inline _.Pad(s: StackState, p: int) = { s with Pad = Insets.all (float32 p) }
    [<CustomOperation("padXY")>]
    member inline _.PadXY(s: StackState, x: float32, y: float32) = { s with Pad = Insets.xy x y }
    [<CustomOperation("padXY")>]
    member inline _.PadXY(s: StackState, x: int, y: int) = { s with Pad = Insets.xy (float32 x) (float32 y) }
    [<CustomOperation("at")>]
    member inline _.At(s: StackState, x: float32, y: float32) = { s with X = x; Y = y }
    [<CustomOperation("at")>]
    member inline _.At(s: StackState, x: int, y: int) = { s with X = float32 x; Y = float32 y }
    [<CustomOperation("opacity")>]
    member inline _.Opacity(s: StackState, a: float32) = { s with Alpha = a }
    [<CustomOperation("child")>]
    member inline _.Child(s: StackState, c: Scene) = { s with Items = c :: s.Items }
    /// Add `c` only when `cond` holds.
    [<CustomOperation("childIf")>]
    member inline _.ChildIf(s: StackState, cond: bool, c: Scene) = if cond then { s with Items = c :: s.Items } else s
    [<CustomOperation("children")>]
    member _.Children(s: StackState, cs: Scene seq) = { s with Items = Seq.fold (fun acc c -> c :: acc) s.Items cs }
    [<CustomOperation("onClick")>]
    member inline _.OnClick(s: StackState, f: unit -> unit) =
        { s with Handler = ValueSome (fun e -> match e with PointerDown _ -> f (); true | _ -> false) }
    [<CustomOperation("onEvent")>]
    member inline _.OnEvent(s: StackState, h: InputEvent -> bool) = { s with Handler = ValueSome h }

/// The builders, as ready-made values: `open FSharp.Graphics.SceneDsl`, then `rect { ... }`.
[<AutoOpen>]
module Builders =
    let rectangle = ShapeBuilder KRect
    let circle    = ShapeBuilder KCircle
    let ellipse   = ShapeBuilder KEllipse
    let text      = TextBuilder()
    let column    = StackBuilder Vertical
    let row       = StackBuilder Horizontal
    let layers    = StackBuilder ZStack
