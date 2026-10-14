/// Pure F# software SDF rasterizer.
///
/// Evaluates each shape's signed-distance function at every pixel and blends
/// the result into an RGBA canvas using smooth anti-aliased coverage.
/// This is the reference software path; the WASM host will use GPU shaders.
module FSharp.Graphics.Demo.Rasterizer

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

// ─────────────────────────────────────────────────────────────────────────────
// Canvas  (mutable RGBA pixel buffer)
// ─────────────────────────────────────────────────────────────────────────────

/// Mutable RGBA canvas backed by a flat byte array (4 bytes per pixel, RGBA).
type Canvas(width: int, height: int) =

    let stride = width * 4
    let pixels = Array.zeroCreate (height * stride)

    member _.Width  = width
    member _.Height = height
    member _.Pixels = pixels

    member _.Clear(c: Color) =
        let r = byte (int (c.R * 255.0f + 0.5f) |> max 0 |> min 255)
        let g = byte (int (c.G * 255.0f + 0.5f) |> max 0 |> min 255)
        let b = byte (int (c.B * 255.0f + 0.5f) |> max 0 |> min 255)
        let a = byte (int (c.A * 255.0f + 0.5f) |> max 0 |> min 255)
        for i = 0 to width * height - 1 do
            pixels.[i * 4]     <- r
            pixels.[i * 4 + 1] <- g
            pixels.[i * 4 + 2] <- b
            pixels.[i * 4 + 3] <- a

    /// Read back a pixel as a Color.
    member _.GetPixel(x: int, y: int) : Color =
        if x < 0 || x >= width || y < 0 || y >= height then Color.Transparent
        else
            let off = y * stride + x * 4
            { R = float32 pixels.[off]     / 255.0f
              G = float32 pixels.[off + 1] / 255.0f
              B = float32 pixels.[off + 2] / 255.0f
              A = float32 pixels.[off + 3] / 255.0f }

    /// Write a pixel as a Color (clamps to [0,1]).
    member _.SetPixel(x: int, y: int, c: Color) =
        if x >= 0 && x < width && y >= 0 && y < height then
            let off = y * stride + x * 4
            let clamp v = byte (int (v * 255.0f + 0.5f) |> max 0 |> min 255)
            pixels.[off]     <- clamp c.R
            pixels.[off + 1] <- clamp c.G
            pixels.[off + 2] <- clamp c.B
            pixels.[off + 3] <- clamp c.A

    /// Alpha-composite `src` over the existing pixel at (x, y).
    member canvas.Composite(x: int, y: int, src: Color) =
        let dst = canvas.GetPixel(x, y)
        canvas.SetPixel(x, y, Color.Over src dst)

// ─────────────────────────────────────────────────────────────────────────────
// Paint sampling
// ─────────────────────────────────────────────────────────────────────────────

/// Sample a Paint at a given point, returning the colour at that location.
let private samplePaint (paint: Paint) (p: Vec2) (bounds: Rect) : Color =
    match paint with
    | Solid color -> color

    | LinearGradient(from', to', stops) ->
        if stops.Length = 0 then Color.Transparent
        else
            let dir = to' - from'
            let len2 = dir.LengthSquared
            let t =
                if len2 < 1e-7f then 0.0f
                else Vec2.Dot(p - from', dir) / len2 |> max 0.0f |> min 1.0f
            // Find the two stops bracketing t.
            let mutable lo = stops.[0]
            let mutable hi = stops.[stops.Length - 1]
            for s in stops do
                if s.Offset <= t && s.Offset >= lo.Offset then lo <- s
                if s.Offset >= t && s.Offset <= hi.Offset then hi <- s
            if lo.Offset = hi.Offset then lo.Color
            else
                let blend = (t - lo.Offset) / (hi.Offset - lo.Offset)
                Color.Lerp(lo.Color, hi.Color, blend)

    | RadialGradient(center, radius, stops) ->
        if stops.Length = 0 then Color.Transparent
        else
            let dist = (p - center).Length
            let t = (dist / radius) |> max 0.0f |> min 1.0f
            let mutable lo = stops.[0]
            let mutable hi = stops.[stops.Length - 1]
            for s in stops do
                if s.Offset <= t && s.Offset >= lo.Offset then lo <- s
                if s.Offset >= t && s.Offset <= hi.Offset then hi <- s
            if lo.Offset = hi.Offset then lo.Color
            else
                let blend = (t - lo.Offset) / (hi.Offset - lo.Offset)
                Color.Lerp(lo.Color, hi.Color, blend)

    | ImagePaint(_, _, opacity) ->
        // Image data not available in the software path; render as transparent.
        Color.Create(0.5f, 0.5f, 0.5f, opacity)

// ─────────────────────────────────────────────────────────────────────────────
// SDF anti-aliasing helper
// ─────────────────────────────────────────────────────────────────────────────

/// Convert a signed-distance value to a smooth coverage value in [0, 1].
/// `pixelSize` is the world-space size of one pixel (for anti-aliasing width).
let private sdToCoverage (sdf: float32) (pixelSize: float32) =
    let edge = 0.5f * pixelSize
    // Smooth-step from full inside to full outside over one pixel.
    let t = (sdf + edge) / (2.0f * edge)
    let t = 1.0f - (t |> max 0.0f |> min 1.0f)
    t * t * (3.0f - 2.0f * t)  // smoothstep

// ─────────────────────────────────────────────────────────────────────────────
// Primitive rasterizer
// ─────────────────────────────────────────────────────────────────────────────

/// Rasterize a single PrimitiveShape onto the canvas.
let rasterizePrimitive (canvas: Canvas) (primitive: PrimitiveShape) =
    let shape  = primitive.ToSdfShape()
    let bounds = shape.Bounds.Expand 2.0f  // include anti-aliasing fringe

    let x0 = int (floor bounds.X)     |> max 0
    let y0 = int (floor bounds.Y)     |> max 0
    let x1 = int (ceil  bounds.Right) |> min (canvas.Width  - 1)
    let y1 = int (ceil  bounds.Bottom)|> min (canvas.Height - 1)

    // Shadow pass
    match primitive.Shadow with
    | Some sh ->
        let shadowBounds =
            { X = bounds.X + sh.Offset.X - sh.Blur - sh.Spread
              Y = bounds.Y + sh.Offset.Y - sh.Blur - sh.Spread
              W = bounds.W + (sh.Blur + sh.Spread) * 2.0f
              H = bounds.H + (sh.Blur + sh.Spread) * 2.0f }
        let sx0 = int (floor shadowBounds.X)     |> max 0
        let sy0 = int (floor shadowBounds.Y)     |> max 0
        let sx1 = int (ceil  shadowBounds.Right) |> min (canvas.Width  - 1)
        let sy1 = int (ceil  shadowBounds.Bottom)|> min (canvas.Height - 1)
        for py = sy0 to sy1 do
            for px = sx0 to sx1 do
                let p = Vec2.Create(float32 px + 0.5f, float32 py + 0.5f)
                let shadowP = Vec2.Create(p.X - sh.Offset.X, p.Y - sh.Offset.Y)
                let sdf  = shape.Sdf shadowP
                let sdf' = sdf - sh.Spread
                let blurRadius = max 1.0f sh.Blur
                // Gaussian shadow: approximate with SDF remapping.
                let t = ((-sdf') / blurRadius) |> max 0.0f |> min 1.0f
                let alpha = t * t * sh.Color.A
                if alpha > 0.001f then
                    let shadowColor = { sh.Color with A = alpha }
                    canvas.Composite(px, py, shadowColor)
    | None -> ()

    // Fill pass
    for py = y0 to y1 do
        for px = x0 to x1 do
            let p        = Vec2.Create(float32 px + 0.5f, float32 py + 0.5f)
            let sdf      = shape.Sdf p
            let coverage = sdToCoverage sdf 1.0f
            if coverage > 0.001f then
                let fillColor = samplePaint primitive.Fill p shape.Bounds
                let blended   = { fillColor with A = fillColor.A * coverage }
                canvas.Composite(px, py, blended)

    // Stroke pass
    match primitive.Stroke with
    | Some stroke ->
        let hw = stroke.Thickness * 0.5f
        for py = y0 to y1 do
            for px = x0 to x1 do
                let p    = Vec2.Create(float32 px + 0.5f, float32 py + 0.5f)
                let sdf  = shape.Sdf p
                // Stroke = ring between (sdf = -hw) and (sdf = +hw)
                let strokeSdf = abs sdf - hw
                let coverage  = sdToCoverage strokeSdf 1.0f
                if coverage > 0.001f then
                    let strokeColor = samplePaint stroke.Paint p shape.Bounds
                    let blended     = { strokeColor with A = strokeColor.A * coverage }
                    canvas.Composite(px, py, blended)
    | None -> ()

// ─────────────────────────────────────────────────────────────────────────────
// Text rasterizer  (placeholder – renders a solid rectangle per glyph)
// ─────────────────────────────────────────────────────────────────────────────

/// Rasterize a GlyphAuro TextNode onto the canvas at the given origin.
/// In a full implementation this would use a font rasterizer; here we draw a
/// coloured rectangle for each glyph so layout and hit-test can be verified.
let rasterizeText (canvas: Canvas) (origin: Vec2) (node: GlyphAuro.TextNode) =
    let glyphs, _ = GlyphAuro.TextNode.layout node
    for g in glyphs do
        let x0 = int (floor (origin.X + g.X))       |> max 0
        let y0 = int (floor (origin.Y + g.Y))        |> max 0
        let x1 = int (ceil  (origin.X + g.X + g.Width  - 1.0f)) |> min (canvas.Width  - 1)
        let y1 = int (ceil  (origin.Y + g.Y + g.Height - 1.0f)) |> min (canvas.Height - 1)
        for py = y0 to y1 do
            for px = x0 to x1 do
                canvas.Composite(px, py, node.Color)

// ─────────────────────────────────────────────────────────────────────────────
// Scene rasterizer (recursive)
// ─────────────────────────────────────────────────────────────────────────────

/// Rasterize an entire laid-out scene onto the canvas.
/// `transform` is the current accumulated affine transform.
let rec rasterizeScene (canvas: Canvas) (transform: Matrix3x2) (scene: Scene) =

    let transformed (p: Vec2) = transform.Transform p

    match scene with
    | Scene.Empty -> ()

    | Scene.Shape primitive ->
        // Apply transform by creating new primitives at the transformed position.
        // For simplicity in the software rasterizer we only support translation.
        rasterizePrimitive canvas primitive

    | Scene.Text node ->
        let origin = transformed Vec2.Zero
        rasterizeText canvas origin node

    | Scene.Image _ ->
        ()   // image data not available in software path

    | Scene.Group children ->
        List.iter (rasterizeScene canvas transform) children

    | Scene.Stack(_, _, _, children) ->
        List.iter (rasterizeScene canvas transform) children

    | Scene.Clip(region, child) ->
        // Simple scissor: clip canvas writes to region.
        // For the software rasterizer we use a clip canvas and blit.
        let clip = Canvas(canvas.Width, canvas.Height)
        rasterizeScene clip transform child
        // Copy only pixels within region.
        let x0 = int (floor region.X)     |> max 0
        let y0 = int (floor region.Y)     |> max 0
        let x1 = int (ceil  region.Right) |> min (canvas.Width  - 1)
        let y1 = int (ceil  region.Bottom)|> min (canvas.Height - 1)
        for py = y0 to y1 do
            for px = x0 to x1 do
                let src = clip.GetPixel(px, py)
                if src.A > 0.0f then canvas.Composite(px, py, src)

    | Scene.Padding(_, child) ->
        rasterizeScene canvas transform child

    | Scene.Transform(matrix, child) ->
        rasterizeScene canvas (transform * matrix) child

    | Scene.Opacity(alpha, child) ->
        // Render to an offscreen canvas, then composite with reduced opacity.
        let off = Canvas(canvas.Width, canvas.Height)
        rasterizeScene off Matrix3x2.Identity child
        for py = 0 to canvas.Height - 1 do
            for px = 0 to canvas.Width - 1 do
                let src = off.GetPixel(px, py)
                if src.A > 0.0f then
                    canvas.Composite(px, py, { src with A = src.A * alpha })

    | Scene.Reactive(_, child)
    | Scene.Animated(_, child) ->
        rasterizeScene canvas transform child
