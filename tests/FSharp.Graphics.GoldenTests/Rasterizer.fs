module FSharp.Graphics.GoldenTests.Rasterizer

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

// Framebuffer with uint8 RGBA pixels
type Framebuffer(width: int, height: int) =
    let stride = width * 4
    let pixels = Array.zeroCreate<byte> (height * stride)

    member _.Width  = width
    member _.Height = height
    member _.Pixels = pixels  // RGBA byte array

    member _.Clear(c: Color) =
        for i = 0 to width * height - 1 do
            pixels.[i*4]   <- c.R
            pixels.[i*4+1] <- c.G
            pixels.[i*4+2] <- c.B
            pixels.[i*4+3] <- c.A

    member fb.GetPixel(x: int, y: int) : Color =
        if x < 0 || x >= width || y < 0 || y >= height then Color.Transparent
        else
            let o = y * stride + x * 4
            { R = pixels.[o]; G = pixels.[o+1]; B = pixels.[o+2]; A = pixels.[o+3] }

    member fb.SetPixel(x: int, y: int, c: Color) =
        if x >= 0 && x < width && y >= 0 && y < height then
            let o = y * stride + x * 4
            pixels.[o] <- c.R; pixels.[o+1] <- c.G; pixels.[o+2] <- c.B; pixels.[o+3] <- c.A

    member fb.Composite(x: int, y: int, src: Color) =
        let dst = fb.GetPixel(x, y)
        fb.SetPixel(x, y, Color.blend dst src)

// Sample a paint at a position within the given bounds
let private samplePaint (paint: Paint) (p: Vec2) (bounds: Rect) : Color =
    match paint with
    | Solid c -> c
    | LinearGradient(stops, angleDeg) ->
        if stops.Length = 0 then Color.Transparent
        else
            let rad = Scalar.toRadians angleDeg
            let dir = Vec2.create (Scalar.cos rad) (Scalar.sin rad)
            let cx = bounds.X + bounds.W * 0.5f
            let cy = bounds.Y + bounds.H * 0.5f
            let rel = Vec2.create (p.X - cx) (p.Y - cy)
            let halfDiag = Vec2.len (Vec2.create (bounds.W * 0.5f) (bounds.H * 0.5f))
            let proj = Vec2.dot rel dir
            let t = Scalar.clamp 0.0f 1.0f (proj / (halfDiag * 2.0f) + 0.5f)
            let mutable lo = stops.[0]
            let mutable hi = stops.[stops.Length - 1]
            for s in stops do
                if s.Offset <= t && s.Offset >= lo.Offset then lo <- s
                if s.Offset >= t && s.Offset <= hi.Offset then hi <- s
            if lo.Offset >= hi.Offset then lo.Color
            else Color.lerpLinear lo.Color hi.Color ((t - lo.Offset) / (hi.Offset - lo.Offset))
    | RadialGradient(stops, center) ->
        if stops.Length = 0 then Color.Transparent
        else
            let halfR = Scalar.max' (bounds.W * 0.5f) (bounds.H * 0.5f)
            let dist = Vec2.distance p center
            let t = Scalar.clamp 0.0f 1.0f (dist / (halfR + 0.001f))
            let mutable lo = stops.[0]
            let mutable hi = stops.[stops.Length - 1]
            for s in stops do
                if s.Offset <= t && s.Offset >= lo.Offset then lo <- s
                if s.Offset >= t && s.Offset <= hi.Offset then hi <- s
            if lo.Offset >= hi.Offset then lo.Color
            else Color.lerpLinear lo.Color hi.Color ((t - lo.Offset) / (hi.Offset - lo.Offset))
    | ImageFill _ -> Color.Gray  // Placeholder

// Render a drop shadow for a given SDF
let private renderShadow (fb: Framebuffer) (sdf: ISdfShape) (shadow: Shadow) =
    let bounds = sdf.Bounds
    let pad = shadow.Blur * 3.0f + shadow.Spread
    let x0 = int (bounds.X - pad + shadow.Offset.X) - 1
    let y0 = int (bounds.Y - pad + shadow.Offset.Y) - 1
    let x1 = int (Rect.right bounds + pad + shadow.Offset.X) + 2
    let y1 = int (Rect.bottom bounds + pad + shadow.Offset.Y) + 2
    for py = y0 to y1 do
        for px = x0 to x1 do
            let p = Vec2.create (float32 px + 0.5f - shadow.Offset.X) (float32 py + 0.5f - shadow.Offset.Y)
            let d = sdf.Sdf p + shadow.Spread
            let alpha =
                if shadow.Blur < 0.001f then
                    if d < 0.0f then 1.0f else 0.0f
                else
                    let t = Scalar.clamp 0.0f 1.0f (-d / (shadow.Blur * 2.0f) + 0.5f)
                    t * t * (3.0f - 2.0f * t)  // smoothstep
            if alpha > 0.004f then
                let col = Color.withAlpha alpha shadow.Color
                fb.Composite(px, py, col)

// Render a single primitive shape to the framebuffer
let renderShape (fb: Framebuffer) (shape: PrimitiveShape) =
    let sdfShape = Shapes.toSdf shape
    let bounds = sdfShape.Bounds
    let fill = Shapes.fill shape

    let shadowOpt, strokeOpt =
        match shape with
        | Rectangle r -> r.Shadow, r.Stroke
        | Circle c    -> c.Shadow, c.Stroke
        | Ellipse e   -> e.Shadow, e.Stroke
        | Path(_, _, st, sh) -> sh, st

    // Render shadow first (below fill)
    match shadowOpt with
    | Some shadow when not shadow.Inner -> renderShadow fb sdfShape shadow
    | _ -> ()

    // Render fill
    let x0 = int bounds.X - 1
    let y0 = int bounds.Y - 1
    let x1 = int (Rect.right bounds) + 2
    let y1 = int (Rect.bottom bounds) + 2

    for py = y0 to y1 do
        for px = x0 to x1 do
            let p = Vec2.create (float32 px + 0.5f) (float32 py + 0.5f)
            let d = sdfShape.Sdf p
            let alpha = Scalar.clamp 0.0f 1.0f (0.5f - d)
            if alpha > 0.004f then
                let baseColor = samplePaint fill p bounds
                let col = Color.withAlpha (float32 baseColor.A / 255.0f * alpha) baseColor
                fb.Composite(px, py, col)

    // Render stroke (above fill)
    match strokeOpt with
    | Some stroke ->
        let hw = stroke.Width * 0.5f
        for py = y0 to y1 do
            for px = x0 to x1 do
                let p = Vec2.create (float32 px + 0.5f) (float32 py + 0.5f)
                let d = sdfShape.Sdf p
                let edgeAlpha = Scalar.clamp 0.0f 1.0f (hw + 0.5f - abs d)
                if edgeAlpha > 0.004f then
                    let baseColor =
                        match stroke.Paint with
                        | Solid c -> c
                        | _ -> Color.Black
                    let col = Color.withAlpha (float32 baseColor.A / 255.0f * edgeAlpha) baseColor
                    fb.Composite(px, py, col)
    | None -> ()

// Walk a Scene tree and render to framebuffer
let rec renderScene (fb: Framebuffer) (scene: Scene) =
    match scene with
    | Empty -> ()
    | Shape shape -> renderShape fb shape
    | Group children -> for c in children do renderScene fb c
    | Stack(_, children) -> for c in children do renderScene fb c
    | Padding(_, child) -> renderScene fb child
    | Transform(_, child) -> renderScene fb child  // Simplified: ignore transform for now
    | Opacity(_, child) -> renderScene fb child
    | Clip(_, child) -> renderScene fb child
    | Reactive(_, child) -> renderScene fb child
    | Animated(_, child) -> renderScene fb child
    | Blur(_, child) -> renderScene fb child
    | ColorFilter(_, child) -> renderScene fb child
    | BlendMode(_, child) -> renderScene fb child
    | Mask(_, child) -> renderScene fb child
    | Text _ -> ()   // Text rendering requires glyph atlas
    | Image _ -> ()  // Image rendering requires asset loading

/// Render a Scene to a Framebuffer
let render (scene: Scene) (width: int) (height: int) (bg: Color) : Framebuffer =
    let fb = Framebuffer(width, height)
    fb.Clear(bg)
    renderScene fb scene
    fb
