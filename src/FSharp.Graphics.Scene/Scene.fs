namespace FSharp.Graphics.Scene

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives

/// Input event types for reactive regions
type InputEvent =
    | PointerDown  of position: Vec2 * button: int
    | PointerUp    of position: Vec2 * button: int
    | PointerMove  of position: Vec2
    | PointerEnter of position: Vec2
    | PointerLeave of position: Vec2
    | KeyDown      of key: string
    | KeyUp        of key: string
    | Scroll       of delta: Vec2

/// Stack layout direction
type Direction = Horizontal | Vertical | ZStack

/// Cross-axis alignment
type CrossAxisAlignment = Stretch | Start | Center | End_

/// Main axis size mode
type SizeMode = FillParent | WrapContent | Fixed of float32

/// Stack layout properties
[<Struct>]
type StackProps = {
    Direction  : Direction
    Gap        : float32
    Alignment  : CrossAxisAlignment
    MainSize   : SizeMode
}

/// Edge insets
[<Struct>]
type Insets = { Top: float32; Right: float32; Bottom: float32; Left: float32 }

module Insets =
    let zero = { Top = 0.0f; Right = 0.0f; Bottom = 0.0f; Left = 0.0f }
    let all p = { Top = p; Right = p; Bottom = p; Left = p }
    let xy x y = { Top = y; Right = x; Bottom = y; Left = x }
    let create t r b l = { Top = t; Right = r; Bottom = b; Left = l }

/// Blend modes for compositing
type Blend =
    | Normal | Multiply | Screen | Overlay
    | Darken | Lighten | ColorDodge | ColorBurn
    | HardLight | SoftLight | Difference | Exclusion

/// Color matrix for filters (4×5 RGBA matrix)
type ColorMatrix = float32 array

/// The immutable Scene tree
type Scene =
    // Leaf primitives
    | Shape     of PrimitiveShape
    | Text      of text: string * style: TextStyle
    | Image     of ImageId * Rect * opacity: float32
    | Empty

    // Structural compositing
    | Group     of children: Scene list
    | Stack     of StackProps * children: Scene list
    | Clip      of region: Rect * child: Scene
    | Padding   of Insets * child: Scene

    // Transformation
    | Transform of Mat3x2 * child: Scene
    | Opacity   of alpha: float32 * child: Scene

    // Interaction
    | Reactive  of handler: (InputEvent -> bool) * child: Scene

    // Animation
    | Animated  of Transition * child: Scene

    // Effects
    | Blur      of sigma: float32 * child: Scene
    | ColorFilter of ColorMatrix * child: Scene
    | BlendMode of Blend * child: Scene
    | Mask      of mask: Scene * child: Scene

/// Pipeline combinators for Scene manipulation
module Scene =
    let translate dx dy s       = Transform(Mat3x2.translation dx dy, s)
    let scale sx sy s           = Transform(Mat3x2.scale sx sy, s)
    let rotate deg origin s     = Transform(Mat3x2.rotateAround deg origin, s)
    let opacity a s             = Opacity(a, s)
    let clip r s                = Clip(r, s)
    let pad i s                 = Padding(i, s)
    let padAll p s              = Padding(Insets.all p, s)
    let padXY x y s             = Padding(Insets.xy x y, s)
    let animated t s            = Animated(t, s)
    let reactive h s            = Reactive(h, s)
    let blur sigma s            = Blur(sigma, s)
    let colorFilter m s         = ColorFilter(m, s)
    let blendMode b s           = BlendMode(b, s)
    let mask m s                = Mask(m, s)

    let hstack gap children     = Stack({ Direction = Horizontal; Gap = gap; Alignment = Start; MainSize = WrapContent }, children)
    let vstack gap children     = Stack({ Direction = Vertical;   Gap = gap; Alignment = Start; MainSize = WrapContent }, children)
    let zstack children         = Stack({ Direction = ZStack;     Gap = 0.0f; Alignment = Start; MainSize = WrapContent }, children)

    let group children          = Group children
    let empty                   = Empty

    let onClick handler s       = Reactive((fun evt -> match evt with PointerDown _ -> handler(); true | _ -> false), s)
    let onHover enter leave s   = Reactive((fun evt -> match evt with PointerEnter _ -> enter(); true | PointerLeave _ -> leave(); true | _ -> false), s)
