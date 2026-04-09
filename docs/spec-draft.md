# FSharp.Graphics — Complete Architecture & Engineering Specification

**Version:** 1.0  
**Date:** April 2026  
**Status:** Authoritative Engineering Specification  
**Scope:** Pure F# graphics stack targeting WasmGC, zero .NET BCL dependencies

---

## 0. Motivation & North Star

We are building the world-class, most elegant industry standard for retained-mode 2D graphics on WasmGC. This is simultaneously:

- A **library specification** for a clean, composable F# graphics stack
- A **testing contract** that defines "done": coordinate-accurate pixel output verified against golden masters
- A **team contract** that aligns compiler work, math library work, and graphics library work into a coherent roadmap

The bar is Clutter + Flutter-level quality, modernized through immutable-first F# with algebraic types. The "done" condition is not "it compiles" — it is "the geometry is correct, verified by automated pixel tests, and the CI gate is green."

---

## 1. Repository & Package Structure

```
fsharp-graphics/
├── src/
│   ├── FSharp.Math/                    ← Self-contained math BCL (no .NET deps)
│   │   ├── FSharp.Math.fsproj
│   │   ├── Scalar.fs                   ← float32 ops, constants, approx equality
│   │   ├── Vec2.fs
│   │   ├── Vec3.fs
│   │   ├── Vec4.fs
│   │   ├── Mat2x2.fs
│   │   ├── Mat3x2.fs                   ← Affine 2D transforms (primary workhorse)
│   │   ├── Mat4x4.fs
│   │   ├── Rect.fs                     ← AABB, intersection, union, contains
│   │   ├── BezierCubic.fs
│   │   └── Color.fs                    ← u8×4, f32×4, conversions, sRGB/linear
│   │
│   ├── FSharp.Graphics.Core/           ← Primitive types shared by all layers
│   │   ├── FSharp.Graphics.Core.fsproj
│   │   ├── Types.fs                    ← ImageId, FontId, CursorKind, opaque IDs
│   │   ├── Paint.fs                    ← Solid / LinearGradient / RadialGradient / Image
│   │   ├── Shadow.fs
│   │   ├── Stroke.fs
│   │   ├── CornerRadius.fs
│   │   └── TextTypes.fs                ← FontWeight, TextAlign, TextStyle, GlyphPosition
│   │
│   ├── FSharp.Graphics.Primitives/     ← SDF shape system
│   │   ├── FSharp.Graphics.Primitives.fsproj
│   │   ├── ISdfShape.fs
│   │   ├── Shapes.fs                   ← Rectangle, Circle, Ellipse, Path
│   │   ├── ShapeCache.fs               ← Mutable glyph/shape atlas cache
│   │   └── DrawList.fs                 ← Flat array of GPU-ready draw commands
│   │
│   ├── FSharp.Graphics.Scene/          ← Immutable Actor/Scene tree (user-facing)
│   │   ├── FSharp.Graphics.Scene.fsproj
│   │   ├── Scene.fs                    ← Scene DU (the full Actor model)
│   │   ├── Animation.fs                ← Transition, Easing, LoopMode
│   │   ├── Layout.fs                   ← Stack, Padding, Clip layout engine
│   │   ├── ActorBuilder.fs             ← CE builder for Scene trees
│   │   └── Combinators.fs              ← |> pipeline helpers
│   │
│   ├── FSharp.Graphics.Render/         ← Internal mutable cache + encoder
│   │   ├── FSharp.Graphics.Render.fsproj
│   │   ├── CachedNode.fs               ← Mutable dirty-flag node tree
│   │   ├── Diff.fs                     ← Scene → Scene → Delta
│   │   └── Encode.fs                   ← Delta → byte[] (linear memory command buffer)
│   │
│   ├── FSharp.Graphics.Tea/            ← TEA/MVU runtime
│   │   ├── FSharp.Graphics.Tea.fsproj
│   │   ├── Program.fs                  ← Program<'Model,'Msg>
│   │   ├── Cmd.fs                      ← Cmd<'Msg> DU + flatten + execution
│   │   └── Runtime.fs                  ← The single mutable app loop
│   │
│   └── GlyphAuro/                      ← Existing text engine (unchanged API)
│       └── (TextNode, LayoutEngine, ShapeCache, DrawList — as-is)
│
├── tests/
│   ├── FSharp.Math.Tests/
│   ├── FSharp.Graphics.Tests/          ← Unit + property-based tests (FsCheck + Expecto)
│   └── FSharp.Graphics.GoldenTests/    ← Visual regression (PNG/SVG golden masters)
│
├── golden/                             ← Committed reference images + JSON scene snapshots
│   ├── png/
│   │   ├── 001_rect_solid.png
│   │   ├── 002_circle_shadow.png
│   │   └── ...
│   └── json/
│       └── scenes/
│
├── demo/
│   └── FSharp.Graphics.Demo/           ← Renders golden PNGs via software rasterizer
│
└── .github/
    └── workflows/
        ├── ci.yml
        └── golden-update.yml
```

### Naming Rationale

| Namespace | Concern | Dependencies |
|-----------|---------|--------------|
| `FSharp.Math` | Self-hosted math primitives | None |
| `FSharp.Graphics.Core` | Shared types (Paint, Shadow, Color) | `FSharp.Math` |
| `FSharp.Graphics.Primitives` | SDF shapes, ShapeCache, DrawList | `FSharp.Graphics.Core` |
| `FSharp.Graphics.Scene` | Immutable Actor tree | `FSharp.Graphics.Primitives` |
| `FSharp.Graphics.Render` | Dirty cache + encoder | `FSharp.Graphics.Scene` + `GlyphAuro` |
| `FSharp.Graphics.Tea` | TEA runtime | `FSharp.Graphics.Render` |
| `GlyphAuro` | Text engine (existing) | `FSharp.Math` |

The dependency graph is a strict DAG. No circular dependencies. `FSharp.Math` has zero imports from any other package in this repo.

---

## 2. FSharp.Math — Self-Hosted Math Library

**Rationale:** The compiler team's BCL is not yet complete. We own this library entirely and write only what the graphics stack consumes. No speculative additions.

### 2.1 Design Rules

- All types are `[<Struct>]` where layout matters for WasmGC perf
- No `System.Math` — use hand-rolled constants and intrinsics
- `float32` throughout (not `double`); the GPU boundary is f32
- Provide `~=` approximate equality with configurable epsilon for tests
- All functions are pure; no mutation outside of known hot-path inline helpers

### 2.2 Scalar.fs

```fsharp
module FSharp.Math.Scalar

let [<Literal>] Pi    = 3.14159265358979323846f
let [<Literal>] TwoPi = 6.28318530717958647692f
let [<Literal>] Eps   = 1e-6f

let inline clamp (lo: float32) (hi: float32) (x: float32) = 
    if x < lo then lo elif x > hi then hi else x

let inline lerp (a: float32) (b: float32) (t: float32) = a + (b - a) * t

let inline smoothStep (lo: float32) (hi: float32) (x: float32) =
    let t = clamp 0.0f 1.0f ((x - lo) / (hi - lo))
    t * t * (3.0f - 2.0f * t)

let inline approxEq (eps: float32) (a: float32) (b: float32) = abs (a - b) < eps

// sin/cos/sqrt via platform intrinsic stubs — compiler team fills these in
// for the test/CI pipeline on .NET these delegate to System.MathF
```

### 2.3 Vec2.fs

```fsharp
[<Struct>]
type Vec2 = { X: float32; Y: float32 }

module Vec2 =
    let inline create x y : Vec2 = { X = x; Y = y }
    let zero  = create 0.0f 0.0f
    let one   = create 1.0f 1.0f
    let inline add (a: Vec2) (b: Vec2)  = create (a.X + b.X) (a.Y + b.Y)
    let inline sub (a: Vec2) (b: Vec2)  = create (a.X - b.X) (a.Y - b.Y)
    let inline scale (s: float32) (v: Vec2) = create (v.X * s) (v.Y * s)
    let inline dot (a: Vec2) (b: Vec2)  = a.X * b.X + a.Y * b.Y
    let inline lenSq (v: Vec2) = dot v v
    // len, normalize, perp, lerp — keep minimal, add when needed
```

### 2.4 Mat3x2.fs — The Affine Workhorse

The 2D affine transform is a 3×2 matrix (last row implicit `[0 0 1]`).
Stored column-major for GPU compatibility.

```fsharp
[<Struct>]
type Mat3x2 = {
    M11: float32; M12: float32
    M21: float32; M22: float32
    M31: float32; M32: float32   // translation
}

module Mat3x2 =
    let identity = { M11=1f; M12=0f; M21=0f; M22=1f; M31=0f; M32=0f }

    let inline translation dx dy = { identity with M31=dx; M32=dy }
    let inline scale sx sy       = { identity with M11=sx; M22=sy }
    // rotation, multiply, invert, transformPoint, transformRect
    // all pure, no allocation
```

**Testing law: invertibility**
```fsharp
// Property: T * invert(T) = identity (within Eps)
// Tested by FsCheck with arbitrary Mat3x2
```

### 2.5 Rect.fs

```fsharp
[<Struct>]
type Rect = { X: float32; Y: float32; W: float32; H: float32 }

module Rect =
    let inline create x y w h : Rect = { X=x; Y=y; W=w; H=h }
    let inline right  (r: Rect) = r.X + r.W
    let inline bottom (r: Rect) = r.Y + r.H
    let inline contains (pt: Vec2) (r: Rect) =
        pt.X >= r.X && pt.X <= right r && pt.Y >= r.Y && pt.Y <= bottom r
    let inline intersect (a: Rect) (b: Rect) =
        let x = max a.X b.X
        let y = max a.Y b.Y
        let r = min (right a) (right b)
        let b2 = min (bottom a) (bottom b)
        { X=x; Y=y; W=max 0.0f (r-x); H=max 0.0f (b2-y) }
    let inline union (a: Rect) (b: Rect) =
        let x = min a.X b.X
        let y = min a.Y b.Y
        { X=x; Y=y; W=max (right a) (right b) - x; H=max (bottom a) (bottom b) - y }
    let inline transform (m: Mat3x2) (r: Rect) = ...
```

### 2.6 Color.fs

```fsharp
[<Struct>]
type Color = { R: uint8; G: uint8; B: uint8; A: uint8 }

module Color =
    let inline rgb r g b : Color   = { R=r; G=g; B=b; A=255uy }
    let inline rgba r g b a : Color = { R=r; G=g; B=b; A=a }
    let inline hex (h: uint32) : Color =
        { R=uint8(h >>> 16); G=uint8(h >>> 8); B=uint8 h; A=255uy }
    let inline hexA (h: uint32) : Color =
        { R=uint8(h >>> 24); G=uint8(h >>> 16); B=uint8(h >>> 8); A=uint8 h }

    // Premultiplied alpha blending (correct for compositing)
    let inline blend (bg: Color) (fg: Color) : Color = ...

    // sRGB ↔ linear (for correct gradient interpolation)
    let inline toLinear  (c: Color) : Vec4 = ...
    let inline fromLinear (v: Vec4) : Color = ...

    // Named constants
    let Black       = rgb 0uy 0uy 0uy
    let White       = rgb 255uy 255uy 255uy
    let Transparent = rgba 0uy 0uy 0uy 0uy
    let Red         = rgb 255uy 0uy 0uy
    let Green       = rgb 0uy 255uy 0uy
    let Blue        = rgb 0uy 0uy 255uy
```

---

## 3. FSharp.Graphics.Core — Shared Domain Types

### 3.1 Paint.fs

```fsharp
type GradientStop = { Offset: float32; Color: Color }   // offset in [0, 1]

type Paint =
    | Solid          of Color
    | LinearGradient of stops: GradientStop array * angleDeg: float32
    | RadialGradient of stops: GradientStop array * center: Vec2
    | ImageFill      of ImageId * opacity: float32

// Helpers
module Paint =
    let solid c              = Solid c
    let linearV stops        = LinearGradient(stops, 90.0f)
    let linearH stops        = LinearGradient(stops, 0.0f)
    let radial center stops  = RadialGradient(stops, center)
```

### 3.2 Shadow.fs

```fsharp
[<Struct>]
type Shadow = {
    Offset : Vec2
    Blur   : float32   // Gaussian sigma in pixels
    Spread : float32   // Expand/contract the shadow shape
    Color  : Color
    Inner  : bool      // false = drop shadow; true = inner shadow (CSS inset)
}

module Shadow =
    let drop offset blur color = { Offset=offset; Blur=blur; Spread=0.0f; Color=color; Inner=false }
    let inner offset blur color = { Offset=offset; Blur=blur; Spread=0.0f; Color=color; Inner=true }
```

Shadow rendering algorithm: **Analytical SDF shadow** (no multipass blur needed).
For a rectangle SDF `d`, the shadow alpha at pixel `p` is:

```
alpha(p) = smoothStep(blur, -blur, sdf(p - offset)) * shadow.Color.A
```

This produces correct soft shadows in a single fragment shader pass. No ping-pong textures.

### 3.3 TextTypes.fs

```fsharp
type FontWeight = Thin | ExtraLight | Light | Regular | Medium | SemiBold | Bold | ExtraBold | Black
type TextAlign  = Left | Center | Right | Justify
type TextOverflow = Clip | Ellipsis | Visible

type TextStyle = {
    FontId       : uint64
    SizePx       : float32
    Color        : Color
    Weight       : FontWeight
    Align        : TextAlign
    Wrap         : bool
    Overflow     : TextOverflow
    LetterSpacing: float32
    LineHeight   : float32      // multiplier, default 1.2
}

[<Struct>]
type GlyphPosition = {
    GlyphId  : uint32
    X        : float32
    Y        : float32
    AdvanceX : float32
}
```

---

## 4. FSharp.Graphics.Scene — The Actor Model

This is the heart of the library. Every visual element is an `Actor`. Actors form a tree. The tree is rendered. This is Clutter's core idea, expressed in F# with algebraic types instead of GObject.

### 4.1 The Actor/Scene DU

```fsharp
// FSharp.Graphics.Scene.Scene

type Scene =
    // ── Leaf primitives ────────────────────────────────────────────────
    | Shape     of PrimitiveShape
    | Text      of TextNode                         // GlyphAuro integration point
    | Image     of ImageId * Rect * opacity: float32
    | Empty

    // ── Structural compositing ─────────────────────────────────────────
    | Group     of children: Scene list             // Z-ordered, no layout
    | Stack     of StackProps * children: Scene list
    | Clip      of region: Rect * child: Scene
    | Padding   of Insets * child: Scene

    // ── Transformation ─────────────────────────────────────────────────
    | Transform of Mat3x2 * child: Scene
    | Opacity   of alpha: float32 * child: Scene

    // ── Interaction ────────────────────────────────────────────────────
    | Reactive  of handler: (InputEvent -> bool) * child: Scene

    // ── Animation (declarative, Clutter-style implicit) ────────────────
    | Animated  of Transition * child: Scene

and [<Struct>] StackProps = {
    Direction  : Direction
    Gap        : float32
    Alignment  : CrossAxisAlignment
    MainSize   : SizeMode
}
and Direction = Horizontal | Vertical | ZStack
and CrossAxisAlignment = Stretch | Start | Center | End_
and SizeMode = FillParent | WrapContent | Fixed of float32

and [<Struct>] Insets = { Top: float32; Right: float32; Bottom: float32; Left: float32 }
with
    static member All p   = { Top=p; Right=p; Bottom=p; Left=p }
    static member XY x y  = { Top=y; Right=x; Bottom=y; Left=x }
    static member zero    = Insets.All 0.0f
```

### 4.2 PrimitiveShape

```fsharp
type PrimitiveShape =
    | Rectangle of RectStyle
    | Circle    of CircleStyle
    | Ellipse   of EllipseStyle
    | Path      of sdf: ISdfShape * fill: Paint * stroke: Stroke option * shadow: Shadow option

and RectStyle = {
    Bounds  : Rect
    Radius  : CornerRadius
    Fill    : Paint
    Stroke  : Stroke option
    Shadow  : Shadow option
}

and CircleStyle = {
    Center  : Vec2
    Radius  : float32
    Fill    : Paint
    Stroke  : Stroke option
    Shadow  : Shadow option
}
```

**Everything maps to SDF internally.** The SDF for a rounded rectangle is:

```
d = length(max(|p - center| - (halfSize - radius), 0)) - radius
```

This is analytically exact for arbitrary corner radii in a single expression.

### 4.3 Animation System

```fsharp
type Easing =
    | Linear
    | EaseIn | EaseOut | EaseInOut
    | CubicBezier of p1: Vec2 * p2: Vec2   // CSS cubic-bezier equivalent
    | Spring      of stiffness: float32 * damping: float32
    | Steps       of count: int * jump: StepJump

and StepJump = JumpStart | JumpEnd | JumpBoth | JumpNone

type Transition = {
    DurationMs : float32
    Easing     : Easing
    DelayMs    : float32
    Loop       : LoopMode
    FillMode   : FillMode    // none / forwards / backwards / both (CSS fill-mode)
}
and LoopMode  = Once | Repeat of count: int | Infinite | Alternate
and FillMode  = FillNone | FillForwards | FillBackwards | FillBoth

module Transition =
    let linear   ms = { DurationMs=float32 ms; Easing=Linear;      DelayMs=0.0f; Loop=Once; FillMode=FillNone }
    let easeOut  ms = { DurationMs=float32 ms; Easing=EaseOut;     DelayMs=0.0f; Loop=Once; FillMode=FillNone }
    let easeIn   ms = { DurationMs=float32 ms; Easing=EaseIn;      DelayMs=0.0f; Loop=Once; FillMode=FillNone }
    let spring k d  = { DurationMs=500.0f;     Easing=Spring(k,d); DelayMs=0.0f; Loop=Once; FillMode=FillForwards }
    let loop     t  = { t with Loop = Infinite }
    let delay   ms t = { t with DelayMs = float32 ms }
    let alternate t = { t with Loop = Alternate }
```

**Animation semantics:** The runtime compares `Animated(t, child)` between frames. If the wrapped `child` value changes, the runtime interpolates the changed properties from their previous value to their new value over `DurationMs`, using the specified easing. This is Clutter's implicit animation model — zero callbacks.

**CSS feature parity:** The Easing and FillMode DUs cover all CSS `animation-*` and `transition-*` properties. The `CubicBezier` case handles the full `cubic-bezier(p1x, p1y, p2x, p2y)` CSS syntax exactly.

### 4.4 Effects (CSS-equivalent)

Beyond shadow, we support the following compositing effects via Scene wrappers:

```fsharp
// Additional Scene cases for advanced effects
// (Milestone 3+ — not in initial implementation but reserved in the DU)

    | Blur         of sigma: float32 * child: Scene          // backdrop/content blur
    | ColorFilter  of ColorMatrix * child: Scene             // CSS filter: hue-rotate, saturate, etc.
    | BlendMode    of Blend * child: Scene                   // multiply, screen, overlay etc.
    | Mask         of mask: Scene * child: Scene             // alpha mask (CSS mask-image)

and ColorMatrix = float32 array    // 4×5 RGBA matrix (same as CSS filter matrix())
and Blend = Normal | Multiply | Screen | Overlay | Darken | Lighten | ColorDodge | ColorBurn | HardLight | SoftLight | Difference | Exclusion
```

Implementation strategy: **Render to offscreen texture, apply effect, composite.** For the software rasterizer (CI), these are CPU-computed. For the WASM host, these map to GPU operations.

### 4.5 ActorBuilder Computation Expression

```fsharp
type SceneBuilder() =
    member _.Yield(s: Scene)   = [s]
    member _.Yield(())         = []
    member _.Combine(a, b)     = a @ b
    member _.Delay(f)          = f()
    member _.Zero()            = []
    member _.For(seq, f)       = seq |> Seq.toList |> List.collect f
    member _.Run(children)     = Group children

let scene = SceneBuilder()

// Usage:
let sidebar model =
    scene {
        Shape(Rectangle { Bounds=Rect.create 0.0f 0.0f 200.0f 600.0f
                          Radius=CornerRadius.zero; Fill=Solid Color.White
                          Stroke=None; Shadow=None })
        for item in model.Items do
            navRow item
        if model.ShowFooter then
            footerWidget ()
    }
```

### 4.6 Pipeline Combinators

```fsharp
module Scene =
    let translate dx dy s    = Transform(Mat3x2.translation dx dy, s)
    let scale sx sy s        = Transform(Mat3x2.scale sx sy, s)
    let rotate deg origin s  = Transform(Mat3x2.rotateAround deg origin, s)
    let opacity a s          = Opacity(a, s)
    let clip r s             = Clip(r, s)
    let pad i s              = Padding(i, s)
    let padAll p s           = Padding(Insets.All p, s)
    let animated t s         = Animated(t, s)
    let reactive h s         = Reactive(h, s)
    let blur sigma s         = Blur(sigma, s)
```

---

## 5. FSharp.Graphics.Render — Internal Pipeline

This layer is not exposed to user code. It owns all mutation. The immutable `Scene` tree flows in; a `byte[]` command buffer flows out.

### 5.1 Diff.fs — Scene Delta

```fsharp
type Delta =
    | NoChange
    | Replace     of Scene
    | UpdateOpacity  of float32
    | UpdateTransform of Mat3x2
    | UpdateChildren of (int * Delta) list   // index * child delta
    | UpdateAnimation of Transition

// The diff is structural, using reference equality on immutable DU values.
// If prev === next (same reference), immediately returns NoChange.
// This is the fast path: any unchanged subtree is O(1).

val diff : prev: Scene -> next: Scene -> Delta
```

**v1:** Full structural comparison. **v2 optimization:** Tag each `Scene` value with a 64-bit hash at construction time (similar to React's fiber key). `diff` then does `if hash(prev) = hash(next) then NoChange` — O(1) per node.

### 5.2 Encode.fs — Linear Memory Command Buffer

**Format specification** (stable, versioned):

| Offset | Size | Field |
|--------|------|-------|
| 0 | 1 | Opcode |
| 1 | N | Payload (opcode-specific) |

All multi-byte integers: little-endian. All floats: IEEE 754 f32 LE.

| Opcode | Name | Payload bytes | Total |
|--------|------|---------------|-------|
| 0xFE | BeginFrame | 4 (frame seq u32) | 5 |
| 0xFF | EndFrame | 0 | 1 |
| 0x01 | DrawRect | 4×f32 bounds + 1 u8 radius_mode + 4×f32 radii + paint | ~50 |
| 0x02 | DrawCircle | 2×f32 center + f32 radius + paint | ~30 |
| 0x03 | DrawEllipse | 2×f32 center + 2×f32 radii + paint | ~32 |
| 0x04 | DrawPath | u32 sdf_id + paint | ~20 |
| 0x05 | DrawTextBatch | u64 font_id + f32 size + u32 glyph_count + u32×count glyph_ids + 2×f32×count positions + Color | variable |
| 0x06 | DrawImage | u32 image_id + 4×f32 bounds + f32 opacity | 25 |
| 0x10 | PushClip | 4×f32 rect | 17 |
| 0x11 | PopClip | 0 | 1 |
| 0x12 | PushTransform | 6×f32 Mat3x2 | 25 |
| 0x13 | PopTransform | 0 | 1 |
| 0x14 | SetOpacity | f32 | 5 |
| 0x15 | PushBlur | f32 sigma | 5 |
| 0x16 | PopBlur | 0 | 1 |
| 0x20 | DrawShadow | 4×f32 bounds + f32 blur + f32 spread + 2×f32 offset + Color | ~40 |

```fsharp
module Encode =
    // Pre-allocated, reused across frames — no GC pressure per-frame
    let private cmdBuf : byte[] = Array.zeroCreate (256 * 1024)  // 256 KB

    val encode     : DrawList -> (byte[] * int)        // returns (buf, len), uses internal buf
    val encodeInto : DrawList -> Span<byte> -> int     // zero-copy into caller-owned span
    
    // Opcode constants (public for tests)
    let [<Literal>] OpcodeBeginFrame  = 0xFEuy
    let [<Literal>] OpcodeEndFrame    = 0xFFuy
    let [<Literal>] OpcodeDrawRect    = 0x01uy
    // ...etc
```

### 5.3 CachedNode.fs — Retained Cache Layer

```fsharp
// Internal. User code never touches this.
type internal CachedNode =
    | CachedShape   of PrimitiveShape * mutable dirty: bool * mutable dl: DrawList voption
    | CachedText    of TextNode                                     // GlyphAuro owns it
    | CachedImage   of ImageId * Rect * float32
    | CachedGroup   of children: CachedNode array * mutable dirty: bool * mutable lastBounds: Rect
    | CachedStack   of StackProps * children: CachedNode array * mutable dirty: bool
    | CachedClip    of Rect * child: CachedNode * mutable dirty: bool
    | CachedTransform of Mat3x2 * child: CachedNode * mutable dirty: bool
    | CachedOpacity of float32 * child: CachedNode * mutable dirty: bool
    | CachedAnimated of Transition * AnimState * child: CachedNode * mutable dirty: bool
    | CachedEmpty

// Dirty propagation: bottom-up. When a leaf changes, mark all ancestors dirty.
// Only dirty nodes rebuild their DrawList.
// Cache hit rate target: > 95% for static/slowly-changing UIs.

module internal CachedNode =
    val build     : Scene -> CachedNode
    val applyDelta: Delta -> CachedNode -> CachedNode
    val getDrawList: CachedNode -> ShapeCache -> DrawList
```

**AnimState** tracks per-node animation state (start time, current t, interpolated value) without exposing it in the user-facing `Scene` type.

---

## 6. FSharp.Graphics.Tea — TEA/MVU Runtime

```fsharp
// FSharp.Graphics.Tea.Program

type Program<'Model, 'Msg> = {
    Init   : unit -> 'Model * Cmd<'Msg>
    Update : 'Model -> 'Msg -> 'Model * Cmd<'Msg>
    View   : 'Model -> Scene
}

type Cmd<'Msg> =
    | CmdNone
    | Batch       of Cmd<'Msg> list
    | Emit        of 'Msg
    | LoadImage   of url: string * ('Msg -> ImageId -> 'Msg)   // wrong, fix:
    | LoadImage   of url: string * (ImageId -> 'Msg)
    | LoadFont    of url: string * sizePx: float32 * (uint64 -> 'Msg)
    | Timer       of delayMs: int * 'Msg
    | Http        of url: string * (string -> 'Msg)

module Cmd =
    let none          = CmdNone
    let batch cmds    = Batch cmds
    let emit msg      = Emit msg
    let flatten (cmd: Cmd<'Msg>) : Cmd<'Msg> list = ... // flatten nested Batch
```

### 6.1 Runtime.fs

```fsharp
// The only impure module in the entire stack.
// Exported WASM functions are the only entry points from the host.

module Runtime =
    // Mutable state — the single source of truth
    let mutable private model    : obj = null
    let mutable private prevScene: Scene = Empty
    let mutable private cachedRoot: CachedNode = CachedEmpty
    let mutable private program  : obj = null   // erased Program<_,_>

    // Called by host once per animation frame
    [<WasmExport("render_frame")>]
    let renderFrame (timestampMs: float) : unit =
        let scene    = program.View model
        let delta    = Diff.diff prevScene scene
        let node'    = CachedNode.applyDelta delta cachedRoot
        let dl       = CachedNode.getDrawList node' ShapeCache.Shared
        let len      = Encode.encodeInto dl (Span<byte>(Encode.cmdBuf))
        Gfx.submitFrame (nativeint Encode.cmdBuf) len
        prevScene  <- scene
        cachedRoot <- node'
        processAnimations timestampMs

    // Called by host on input events (packed binary)
    [<WasmExport("handle_input")>]
    let handleInput (ptr: nativeint) (len: int) : unit =
        let evt     = Input.decode ptr len
        let msg     = dispatchInput evt cachedRoot
        let (m', c) = program.Update model msg
        model <- m'
        processCmds c

    // Entry point — sets up the program and begins the loop
    val run : Program<'Model, 'Msg> -> unit
```

### 6.2 Headless Runtime for Tests

```fsharp
module RuntimeHelpers =
    // Returns encoded command buffers for each input batch — no GPU, no WASM
    val runHeadless : Program<'Model,'Msg> -> 'Msg list list -> byte[] list

    // Returns a mutable handle for imperative test driving
    type HeadlessRuntime<'Model, 'Msg> = {
        Dispatch    : 'Msg -> unit
        RenderFrame : float -> unit
        Model       : 'Model           // readable for assertions
        LastBuffer  : byte[]           // last encoded frame
    }
    val start : Program<'Model,'Msg> -> HeadlessRuntime<'Model,'Msg>
```

---

## 7. Layout Engine

### 7.1 Algorithm Selection

We implement a **single-pass constraint-based layout** inspired by Flutter's box constraints. Each node receives `(minW, maxW, minH, maxH)` from its parent and returns `(measuredW, measuredH)`. Positions are then assigned in a second top-down pass.

This is faster than Clutter's multi-pass constraint expressions and simpler than CSS Flexbox's full spec. We cover the common 95% of UI layout needs.

**Skipped algorithms:**
- Full CSS Grid (too complex for v1; use nested Stacks instead)
- Baseline alignment across mixed content (deferred)
- Intrinsic size negotiation with aspect-ratio constraints (deferred)

### 7.2 Layout Function

```fsharp
// Pure, zero-allocation fast path for the common case
// Returns absolute (Scene * Rect) pairs for rendering
val layout : bounds: Rect -> scene: Scene -> (Scene * Rect) list
```

**Complexity target:** O(n) tree nodes for non-overlapping layouts. Clip and Clamp are O(1) per node.

### 7.3 Supported Layout Primitives

| Primitive | Behavior |
|-----------|----------|
| `Stack Horizontal` | Left-to-right, configurable gap and cross-axis alignment |
| `Stack Vertical` | Top-to-bottom, configurable gap and cross-axis alignment |
| `Stack ZStack` | All children at same origin (z-ordered) |
| `Padding` | Insets applied to child bounds |
| `Clip` | Intersects current bounds with given rect |
| `Transform` | Affine transform — layout still uses pre-transform bounds |
| `Reactive` | Transparent to layout |
| `Animated` | Transparent to layout (animation is visual only) |

---

## 8. Software Rasterizer — CI Testing Backend

**This is the critical enabling piece that makes everything testable before the WASM compiler is complete.**

The rasterizer is a pure F# software renderer that consumes the command buffer and produces a `byte[]` RGBA pixel array (which can then be encoded to PNG by the existing `FSharp.Graphics.Demo.Png` module).

### 8.1 Why a Software Rasterizer

- The WasmGC compiler is not yet complete — we cannot run the stack in a browser
- We need coordinate-accurate pixel tests in CI
- We need golden master images to define "correct output"
- The software rasterizer gives us a deterministic, platform-independent reference renderer

### 8.2 Rasterizer Architecture

```fsharp
// FSharp.Graphics.Software.Rasterizer (test/CI only — not shipped to WASM)

type Framebuffer = {
    Width  : int
    Height : int
    Pixels : uint32[]   // ARGB packed
}

module Rasterizer =
    // Parse command buffer and render to framebuffer — pure, deterministic
    val render : cmdBuffer: byte[] -> len: int -> width: int -> height: int -> Framebuffer

    // Blit a scene directly (skips encode/decode for tests)
    val renderScene : Scene -> width: int -> height: int -> Framebuffer
```

### 8.3 SDF Rasterization

Every shape is rendered using its analytical SDF evaluated per-pixel:

```
for each pixel p:
    d = sdf(shape, p)
    alpha = smoothStep(1.0, -1.0, d) * shape.fill.alpha
    pixel = blend(pixel, fill_color(p, shape), alpha)
```

This gives analytically correct anti-aliasing with sub-pixel precision. No MSAA needed for the software rasterizer.

Shadows use the same SDF with an offset and a Gaussian approximation:

```
shadow_d = sdf(shape, p - shadow.offset) + shadow.spread
shadow_alpha = gaussian_approx(shadow_d, shadow.blur) * shadow.color.A
pixel = blend(pixel, shadow.color, shadow_alpha)
```

Gaussian approximation: `exp(-d*d / (2 * sigma^2))` — exact for testing purposes.

### 8.4 PNG Output

The existing `FSharp.Graphics.Demo.Png` module (pure F# zlib stored-block encoder) is the canonical output format for golden tests. No external dependencies.

**Additionally, SVG output** is supported for debugging layout and hit-testing (coordinates readable without image diffing):

```fsharp
module Rasterizer.Svg =
    // Emit SVG from a Scene — not pixel-perfect but coordinate-accurate
    val renderSvg : Scene -> string   // SVG string
```

SVG output is used in the CI pipeline to validate coordinate assertions independently of pixel rendering.

---

## 9. Testing Strategy

### 9.1 Principles

The testing strategy has three tiers, each with a distinct failure mode:

| Tier | Tool | Tests what | Failure means |
|------|------|-----------|---------------|
| Math laws | FsCheck property tests | Algebraic correctness | Wrong matrix math, coordinate system bug |
| Behavioral | Expecto unit tests | Scene graph semantics | Layout bug, diff bug, encoding bug |
| Visual regression | PNG golden masters | Pixel-accurate output | Rendering regression or deliberate improvement |

**The golden master tier is the final gate.** A PR cannot merge if golden masters don't match. Intentional changes require running `golden-update` and committing new masters.

### 9.2 Math Laws (FsCheck — Property-Based)

```fsharp
// All in FSharp.Math.Tests/

// Law 1: Matrix invertibility
[<Property>]
let ``Mat3x2 inverse is correct`` (m: Mat3x2) =
    let inv = Mat3x2.invert m
    match inv with
    | None -> // singular matrix — ok, skip
        true
    | Some i ->
        let product = Mat3x2.multiply m i
        approxMatEq Mat3x2.identity product Scalar.Eps

// Law 2: Associativity
[<Property>]
let ``Mat3x2 multiply is associative`` (a: Mat3x2) (b: Mat3x2) (c: Mat3x2) =
    approxMatEq (Mat3x2.multiply (Mat3x2.multiply a b) c)
                (Mat3x2.multiply a (Mat3x2.multiply b c))
                Scalar.Eps

// Law 3: Local-to-world consistency
[<Property>]
let ``Nested transforms compose correctly`` (transforms: Mat3x2 list) (pt: Vec2) =
    let world = transforms |> List.fold Mat3x2.multiply Mat3x2.identity
    let sequential = transforms |> List.fold (fun p t -> Mat3x2.transformPoint t p) pt
    let combined = Mat3x2.transformPoint world pt
    Vec2.approxEq Scalar.Eps sequential combined

// Law 4: AABB containment after transform
[<Property>]
let ``Transformed AABB still contains original points`` (r: Rect) (m: Mat3x2) =
    let points = [Vec2.create r.X r.Y; Vec2.create (Rect.right r) r.Y
                  Vec2.create r.X (Rect.bottom r); Vec2.create (Rect.right r) (Rect.bottom r)]
    let transformed = points |> List.map (Mat3x2.transformPoint m)
    let aabb = Rect.boundsOf transformed
    transformed |> List.forall (fun p -> Rect.contains p aabb)

// Law 5: Rect intersection idempotent
[<Property>]
let ``Rect intersect with self is identity`` (r: Rect) =
    Rect.approxEq (Rect.intersect r r) r Scalar.Eps

// Law 6: Layout space conservation
[<Property>]
let ``Stack children total <= parent bounds`` (props: StackProps) (count: int) =
    let count = (abs count % 10) + 1
    let bounds = Rect.create 0.0f 0.0f 400.0f 300.0f
    let children = List.init count (fun _ -> Empty)   // zero-size leaves
    let pairs = layout bounds (Stack(props, children))
    let childBounds = pairs |> List.map snd
    childBounds |> List.forall (fun cb ->
        cb.W <= bounds.W + Scalar.Eps && cb.H <= bounds.H + Scalar.Eps)
```

### 9.3 Unit Tests (Expecto)

```fsharp
// FSharp.Graphics.Tests/

// Scene graph semantics
testCase "Group preserves child order" ...
testCase "Clip intersects bounds correctly" ...
testCase "Stack Horizontal distributes width" ...
testCase "Padding reduces child bounds" ...

// Diff
testCase "diff identical scenes = NoChange" ...
testCase "diff opacity change = UpdateOpacity" ...
testCase "diff group child change = UpdateChildren [index, delta]" ...

// Encoder
testCase "encode BeginFrame + EndFrame always present" ...
testCase "encoded buffer is parseable by decoder" ...
testCase "round-trip encode/decode preserves all fields" ...

// Hit testing
testCase "Reactive node captures events within bounds" ...
testCase "Scale(0) node never captures hit events" ...

// Animation
testCase "easeIn at t=0 = 0, at t=1 = 1" ...
testCase "spring converges to target" ...
testCase "linear interpolation is exact at midpoint" ...
```

### 9.4 Visual Regression — Golden Masters

This is the **definitive correctness criterion**. The pipeline:

```
F# Scene description
        │
        ▼
Software Rasterizer
        │
        ▼
Framebuffer (byte[])
        │
        ▼
PNG encoder
        │
        ▼ compare pixel-by-pixel
Golden master PNG ←── committed in /golden/png/
        │
        ▼
PASS (all pixels within tolerance) or FAIL
```

**Tolerance:** Per-pixel delta ≤ 2 (out of 255) for each channel. This accounts for floating-point rounding differences while catching real regressions.

**Golden test inventory** (must all pass before any milestone is declared complete):

| ID | Scene | Validates |
|----|-------|-----------|
| 001 | Solid-color rectangle | Basic rect, fill, bounds |
| 002 | Rounded rectangle | CornerRadius SDF |
| 003 | Circle with drop shadow | Shadow compositing |
| 004 | Linear gradient rect | Gradient interpolation |
| 005 | Radial gradient circle | Radial gradient |
| 006 | Nested Stack Vertical | Layout engine |
| 007 | Nested Stack Horizontal | Layout engine |
| 008 | ZStack with opacity layers | Opacity compositing |
| 009 | Clip region applied | Clip intersection |
| 010 | Affine transform (rotation 45°) | Mat3x2 transform pipeline |
| 011 | Text + shape composition | GlyphAuro integration |
| 012 | Inner shadow on rect | Inner shadow variant |
| 013 | Image fill on shape | ImageFill paint |
| 014 | Stroked circle | Stroke rendering |
| 015 | Complex nested scene (10+ nodes) | Full pipeline integration |
| 016 | Animation frame interpolation | Easing correctness (fixed timestamp) |
| 017 | Gaussian blur | Blur effect |
| 018 | Color filter (grayscale) | ColorMatrix effect |

**Scene → JSON snapshots** are also committed alongside PNGs. These validate the Scene DU structure and command buffer encoding independently of pixel output.

### 9.5 The "Done" Definition

**A feature is done if and only if:**

1. All relevant unit and property tests pass
2. The golden master test for the associated visual ID passes
3. CI is green (no tolerance failures)

Specifically for the graphics pipeline overall:

> **The graphics pipeline is done when all 18 golden master tests pass in CI on both Linux (software rasterizer) and macOS (software rasterizer), with zero pixel tolerance failures, across three consecutive clean builds.**

---

## 10. CI/CD Pipeline

### 10.1 GitHub Actions — ci.yml

```yaml
name: CI
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup .NET (for CI — uses .NET F# to run tests)
        uses: actions/setup-dotnet@v4
        with:
          dotnet-version: '9.0.x'
      
      - name: Build
        run: dotnet build fsharp-graphics.sln -c Release
      
      - name: Unit + Property Tests
        run: dotnet test tests/FSharp.Graphics.Tests --logger "console;verbosity=normal"
      
      - name: Math Tests
        run: dotnet test tests/FSharp.Math.Tests --logger "console;verbosity=normal"
      
      - name: Generate Actual PNGs (software rasterizer)
        run: dotnet run --project tests/FSharp.Graphics.GoldenTests -- --generate-actual
      
      - name: Compare against golden masters
        run: |
          dotnet run --project tests/FSharp.Graphics.GoldenTests -- --compare \
            --tolerance 2 \
            --golden-dir golden/png \
            --actual-dir .golden-actual
      
      - name: Upload actual images on failure
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: golden-failures
          path: .golden-actual/
```

### 10.2 Golden Update Workflow — golden-update.yml

```yaml
name: Update Golden Masters
on:
  workflow_dispatch:
    inputs:
      reason:
        description: 'Reason for golden update'
        required: true

jobs:
  update:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          token: ${{ secrets.GITHUB_TOKEN }}
      
      - name: Setup .NET
        uses: actions/setup-dotnet@v4
        with:
          dotnet-version: '9.0.x'
      
      - name: Regenerate all golden masters
        run: |
          dotnet run --project tests/FSharp.Graphics.GoldenTests -- --generate-golden \
            --output golden/png
      
      - name: Commit updated masters
        run: |
          git config user.name "golden-bot"
          git config user.email "golden@fsharp-graphics"
          git add golden/
          git commit -m "chore(golden): update masters — ${{ github.event.inputs.reason }}"
          git push
```

### 10.3 No BCL in CI

The test projects use .NET F# as the runner **only** because the WasmGC compiler is not yet complete. However, the library source files (`src/**`) must not import any .NET BCL type that is not available in our own BCL implementation.

**Enforcement:** A custom Roslyn analyzer (`FSharp.Graphics.Analyzers`) runs in CI and fails the build if any `src/**` file imports:
- `System.Math` (use `FSharp.Math.Scalar` instead)
- `System.Numerics.*` (use `FSharp.Math.*` instead)  
- `System.Runtime.InteropServices` (permitted only in `Encode.fs`)
- Any `System.Collections.*` other than `Array` and the approved subset

This ensures that when the WasmGC compiler is ready, the library compiles without modification.

---

## 11. Software Rasterizer Algorithms — Specification

### 11.1 Rendering Order

Per frame, render in this order:
1. Clear framebuffer to background color
2. Walk the flattened draw list (depth-first, back-to-front — painter's algorithm)
3. For each node: render shadow first, then fill, then stroke

### 11.2 Anti-Aliasing

All edges are analytically anti-aliased using the SDF distance. No MSAA. The alpha for a shape edge pixel is:

```
alpha = clamp(0, 1, 0.5 - d / fwidth)
```

where `fwidth` ≈ 1.0 pixel for the software rasterizer (fixed). This gives smooth sub-pixel edges.

### 11.3 Gradient Rasterization

Linear gradients use the correct sRGB-aware interpolation path:
1. Convert stop colors to linear RGB (gamma un-apply)
2. Interpolate in linear RGB
3. Convert back to sRGB for output

This matches CSS `color-interpolation: linearRGB` semantics and produces visually correct gradients without the dark-midpoint artifact of naive sRGB interpolation.

### 11.4 Text Rendering

In the CI rasterizer, text is rendered by the existing GlyphAuro `TextNode`. The glyph atlas is a software bitmap atlas. GlyphAuro's `DrawList` outputs `GlyphPosition` arrays which the rasterizer blits from the atlas with subpixel positioning.

### 11.5 Compositing

Premultiplied alpha compositing throughout:
```
out.rgb = src.rgb + dst.rgb * (1 - src.a)
out.a   = src.a  + dst.a   * (1 - src.a)
```

This is mathematically correct for partial transparency and matches WebGPU/GPU behavior exactly. Straight alpha (`color * alpha` at blend time) is explicitly not used.

---

## 12. wgpu/WebGPU Forward Path (Out of Scope for v1, Specified for Migration)

When the WasmGC compiler is complete and the WASM host is ready:

### 12.1 Bufferless Full-Screen Pass (Preferred Architecture)

From the team's research notes: render everything in a single full-screen triangle. The vertex shader generates coordinates procedurally from `gl_VertexIndex`. The fragment shader queries the command buffer (uploaded as a UBO or storage buffer) and evaluates SDFs per-pixel.

**Why this is the right choice:**
- Zero vertex buffer management — no `createBuffer`, no `vertexAttribPointer`
- Single draw call per frame regardless of UI complexity
- Pixel-perfect at any scale — no mesh rasterization artifacts
- Maps directly to our existing SDF-based primitive system

### 12.2 The GPU Rendering Path

```
Host receives submit_frame(ptr, len)
        │
        ▼
Parse command buffer → build GPU-side uniform buffer
        │
        ▼
Upload UBO to GPU (one write per frame)
        │
        ▼
Full-screen draw call (3 vertices, no vertex buffer)
        │
        ▼
Fragment shader:
    for each widget in UBO:
        d = evalSdf(fragCoord, widget)
        alpha = smoothStep(1.0, -1.0, d)
        color = evalPaint(fragCoord, widget, alpha)
        output = blend(output, color)
        │
        ▼
Present
```

### 12.3 WGSL Shader Sketch

```wgsl
struct Widget {
    kind: u32,
    bounds: vec4<f32>,   // x, y, w, h
    radius: vec4<f32>,   // tl, tr, br, bl
    color: vec4<f32>,    // rgba normalized
    shadow_offset: vec2<f32>,
    shadow_blur: f32,
    _pad: f32,
}

@group(0) @binding(0) var<storage, read> widgets: array<Widget>;
@group(0) @binding(1) var<uniform> frame: FrameUniforms;

fn sd_rounded_rect(p: vec2<f32>, bounds: vec4<f32>, r: vec4<f32>) -> f32 {
    // ... standard SDF implementation
}

@fragment
fn fs_main(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {
    var out = vec4<f32>(0.0);
    for (var i = 0u; i < frame.widget_count; i++) {
        let w = widgets[i];
        let d = sd_rounded_rect(pos.xy, w.bounds, w.radius);
        let a = smoothstep(1.0, -1.0, d);
        out = blend_over(out, w.color * a);
    }
    return out;
}
```

This exact approach is documented in `12f-some-thoughts.md` and is the confirmed target architecture for the GPU path.

---

## 13. Developer Experience

### 13.1 The 10-Line Program

A working animated UI in F# should take ~10 lines. This is the DX target:

```fsharp
open FSharp.Graphics
open FSharp.Graphics.Tea

type Msg = | Tick of float32

let init () = 0.0f, Cmd.none
let update x = function Tick dt -> x + dt * 0.5f, Cmd.none
let view x =
    scene {
        Shape(Circle { Center=Vec2.create x 200.0f; Radius=30.0f
                       Fill=Solid(Color.hex 0xFF6432u); Stroke=None; Shadow=None })
    } |> Scene.animated (Transition.spring 180.0f 12.0f)

[<EntryPoint>]
let main _ = Runtime.run { Init=init; Update=update; View=view }
```

### 13.2 Error Messages

The library shall provide meaningful F# compile-time errors through smart use of the type system. No stringly-typed configuration. Every invalid state should be unrepresentable.

### 13.3 Documentation

Every public type and module shall have XML doc comments. The spec is normative; the XML docs are the implementation contract.

---

## 14. Milestones & Success Criteria

| Milestone | Deliverables | Done When |
|-----------|-------------|-----------|
| **M0: Math Foundation** | `FSharp.Math` complete + all property tests pass | FsCheck reports 0 failures on 10,000 random inputs per law |
| **M1: Static Scene** | Software rasterizer renders golden 001–010 | All 10 golden tests pass in CI |
| **M2: Text Integration** | GlyphAuro TextNode integrated, golden 011 passes | Text renders pixel-accurately against golden master |
| **M3: Effects** | Shadow, blur, color filters, golden 012–018 | All 18 golden tests pass in CI |
| **M4: TEA Loop** | `Runtime.runHeadless` + all unit tests pass | Zero unit test failures; headless TEA test suite green |
| **M5: Animations** | Easing correctness test; animated golden test (016) | Golden 016 passes with fixed timestamp input |
| **M6: WASM Host** | First WasmGC binary rendering golden 001 | Same pixel output as software rasterizer for golden 001, tolerance ≤ 2 |
| **M7: Full Stack** | All 18 goldens pass in WASM | CI on WASM path identical to software rasterizer path |

**The project is complete at M7.** No milestone is "done" until CI is green. Not "locally works." CI.

---

## 15. What We Deliberately Skip

| Feature | Reason | When to add |
|---------|--------|-------------|
| Full CSS Grid | Complexity:benefit ratio | After M7 |
| 3D transforms | Out of scope (2D stack) | Separate `FSharp.Graphics.3D` |
| Video / canvas capture | Host-specific, not portable | Per-host extension |
| Accessibility tree | Browser-specific | Browser host extension |
| Flexbox wrap | Uncommon case; use nested Stacks | After M7 |
| Font fallback chains | GlyphAuro scope | GlyphAuro roadmap |
| wasm-bindgen style JS FFI | We own our bridge | Not needed |
| Immediate mode API | Antithetical to retained scene model | Never — use command buffer directly if needed |
| React-style reconciler keys | v1 uses structural diff; keys are v2 optimization | M7 → M8 |

---

## Appendix A: BCL Compatibility Rules

The following is the list of types from the .NET BCL that are **permitted** in `src/**` because they are either already implemented in our BCL or will be:

| Type | Status |
|------|--------|
| `array<T>`, `T[]` | ✅ Implemented |
| `ResizeArray<T>` | ✅ Implemented |
| `option<T>`, `voption<T>` | ✅ Implemented |
| `Result<T,E>` | ✅ Implemented |
| `string` | ✅ Implemented |
| `int`, `float32`, `uint8` etc. | ✅ Implemented |
| `Span<T>` | 🔜 In progress — only in Encode.fs |

Types that are **forbidden** in `src/**`:
- `System.Math` → use `FSharp.Math.Scalar`
- `System.Numerics.*` → use `FSharp.Math.*`
- `System.Text.Encoding` → forbidden (use raw byte operations)
- `System.IO.*` → forbidden (no file I/O in library code)
- `System.Collections.Generic.*` → use `array`, `ResizeArray`, `list`

---

## Appendix B: Coordination with Compiler Team

The graphics library can proceed today using .NET F# as the runner. The following compiler features are **blockers** for the WASM target only (M6+):

| Compiler Feature | Required for | Target Sprint |
|-----------------|-------------|--------------|
| `[<WasmImport>]` / `[<WasmExport>]` attributes | `Runtime.fs`, `Encode.fs` | Sprint 3/4 |
| `Span<T>` in linear memory | Zero-copy command buffer | Sprint 5 |
| Generics / monomorphization | `Program<'Model,'Msg>` | Sprint 5 |
| Structural equality on DUs | `Diff.fs` fast path | Sprint 6 |
| Object linking (separate library) | Library boundary | Sprint 7 |

The library will compile and all tests will pass against the .NET runner today. When each compiler feature lands, the corresponding CI gate activates automatically.

