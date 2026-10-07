module FSharp.Graphics.GoldenTests.Rasterizer

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open System
open System.IO

// ─────────────────────────────────────────────────────────────────────────────
// Framebuffer
// ─────────────────────────────────────────────────────────────────────────────

/// Framebuffer with uint8 RGBA pixels
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

    member fb.Composite(x: int, y: int, src: Color, clipRect: Rect voption) =
        match clipRect with
        | ValueSome clip ->
            let fx = float32 x + 0.5f
            let fy = float32 y + 0.5f
            if fx >= clip.X && fx < clip.X + clip.W && fy >= clip.Y && fy < clip.Y + clip.H then
                fb.Composite(x, y, src)
        | ValueNone -> fb.Composite(x, y, src)

    /// Copy pixels to a new framebuffer (for offscreen compositing)
    member fb.Clone() =
        let copy = Framebuffer(width, height)
        Array.blit pixels 0 copy.Pixels 0 pixels.Length
        copy

    /// Alpha-blend another framebuffer onto this one with uniform opacity
    member fb.BlendFrom(src: Framebuffer, opacity: float32) =
        let cnt = width * height
        for i = 0 to cnt - 1 do
            let so = i * 4
            let srcCol : Color =
                { R = src.Pixels.[so]; G = src.Pixels.[so+1]
                  B = src.Pixels.[so+2]; A = src.Pixels.[so+3] }
            let scaled = Color.withAlpha (float32 srcCol.A / 255.0f * opacity) srcCol
            let dst = fb.GetPixel(i % width, i / width)
            let blended = Color.blend dst scaled
            fb.SetPixel(i % width, i / width, blended)

// ─────────────────────────────────────────────────────────────────────────────
// Paint sampling
// ─────────────────────────────────────────────────────────────────────────────
let inline private sampleStops (stops: GradientStop array) (t: float32) =
    if stops.Length = 0 then 
        Color.Transparent
    else
        let tClamped = Scalar.clamp 0.0f 1.0f t
        let mutable lo = stops.[0]
        let mutable hi = stops.[stops.Length - 1]
        
        for i = 0 to stops.Length - 1 do
            let s = stops.[i]
            if s.Offset <= tClamped && s.Offset >= lo.Offset then lo <- s
            if s.Offset >= tClamped && s.Offset <= hi.Offset then hi <- s
            
        if lo.Offset >= hi.Offset then 
            lo.Color
        else 
            Color.lerpLinear lo.Color hi.Color ((tClamped - lo.Offset) / (hi.Offset - lo.Offset))

let private samplePaint (paint: Paint) (p: Vec2) (bounds: Rect) : Color =
    match paint with
    | Solid c -> c
    
    | LinearGradient(startPoint, endPoint, stops) ->
        // Vektor von Start zu Ende
        let v = Vec2.create (endPoint.X - startPoint.X) (endPoint.Y - startPoint.Y)
        // Quadratische Länge des Richtungsvektors
        let lenSq = v.X * v.X + v.Y * v.Y
        
        if lenSq < 0.0001f then 
            if stops.Length > 0 then stops.[0].Color else Color.Transparent
        else
            // Vektor vom Startpunkt zum aktuellen Pixel p
            let d = Vec2.create (p.X - startPoint.X) (p.Y - startPoint.Y)
            // Skalarprodukt für die Projektion (Dot Product)
            let dot = d.X * v.X + d.Y * v.Y
            // t ist der relative Fortschritt auf der Linie (0.0 bis 1.0)
            let t = dot / lenSq
            sampleStops stops t
            
    | RadialGradient(center, radius, stops) ->
        if radius < 0.0001f then
            if stops.Length > 0 then stops.[stops.Length - 1].Color else Color.Transparent
        else
            // Tatsächliche Distanz vom Pixel zum Zentrum berechnen
            let dx = p.X - center.X
            let dy = p.Y - center.Y
            let dist = Scalar.sqrt (dx * dx + dy * dy)
            // t ist das Verhältnis von Distanz zu Radius
            let t = dist / radius
            sampleStops stops t
            
    | ImageFill _ -> 
        Color.Gray  // Placeholder


// ─────────────────────────────────────────────────────────────────────────────
// Shadow rendering (drop + inner)
// ─────────────────────────────────────────────────────────────────────────────

let private renderDropShadow (fb: Framebuffer) (sdf: ISdfShape) (shadow: Shadow) (clip: Rect voption) =
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
                fb.Composite(px, py, col, clip)

/// Inner shadow: only visible inside the shape, where the SDF approaches the boundary from inside.
let private renderInnerShadow (fb: Framebuffer) (sdf: ISdfShape) (shadow: Shadow) (clip: Rect voption) =
    let bounds = sdf.Bounds
    let x0 = int bounds.X - 1
    let y0 = int bounds.Y - 1
    let x1 = int (Rect.right bounds) + 2
    let y1 = int (Rect.bottom bounds) + 2
    let blur = max shadow.Blur 0.001f
    for py = y0 to y1 do
        for px = x0 to x1 do
            let p = Vec2.create (float32 px + 0.5f) (float32 py + 0.5f)
            let d = sdf.Sdf p  // negative inside the shape
            if d < 0.0f then   // only inside
                // Distance from offset inner edge
                let dp = Vec2.create (p.X - shadow.Offset.X) (p.Y - shadow.Offset.Y)
                let di = sdf.Sdf dp  // distance at offset position
                // Shadow intensity: how far inside the shape we are from the boundary
                let innerDist = -d  // positive = depth inside shape
                let shadowD = di + shadow.Spread  // inner boundary
                let alpha =
                    if shadowD < 0.0f then 0.0f  // well inside, no shadow
                    else
                        let t = Scalar.clamp 0.0f 1.0f (shadowD / (blur * 2.0f))
                        t * t * (3.0f - 2.0f * t) * (Scalar.clamp 0.0f 1.0f (innerDist / blur))
                if alpha > 0.004f then
                    let col = Color.withAlpha alpha shadow.Color
                    fb.Composite(px, py, col, clip)

// ─────────────────────────────────────────────────────────────────────────────
// Shape rendering
// ─────────────────────────────────────────────────────────────────────────────

let renderShape (fb: Framebuffer) (shape: PrimitiveShape) (clip: Rect voption) =
    let sdfShape = Shapes.toSdf shape
    let bounds = sdfShape.Bounds
    let fill = Shapes.fill shape

    let shadowOpt, strokeOpt =
        match shape with
        | Rectangle r -> r.Shadow, r.Stroke
        | Circle c    -> c.Shadow, c.Stroke
        | Ellipse e   -> e.Shadow, e.Stroke
        | Path(_, _, st, sh) -> sh, st

    // Render drop shadow first (below fill)
    match shadowOpt with
    | Some shadow when not shadow.Inner -> renderDropShadow fb sdfShape shadow clip
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
                fb.Composite(px, py, col, clip)

    // Render inner shadow (above fill, below stroke)
    match shadowOpt with
    | Some shadow when shadow.Inner -> renderInnerShadow fb sdfShape shadow clip
    | _ -> ()

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
                    fb.Composite(px, py, col, clip)
    | None -> ()

// ─────────────────────────────────────────────────────────────────────────────
// M2 — GlyphAura text rendering
// ─────────────────────────────────────────────────────────────────────────────

/// Lazy-loaded font tables from DejaVuSans.ttf (bundled in TestData/)
let private loadedFont : Lazy<(GlyphAura.FontTables * GlyphAura.FontMetrics) voption> =
    lazy (
        let fontPath = Path.Combine(AppContext.BaseDirectory, "TestData", "DejaVuSans.ttf")
        if File.Exists(fontPath) then
            let bytes = File.ReadAllBytes(fontPath)
            let mem = System.ReadOnlyMemory(bytes)
            match GlyphAura.FontProvider.load mem with
            | Ok(tables, metrics) -> ValueSome(tables, metrics)
            | Error _ -> ValueNone
        else ValueNone
    )

/// Create a rectangle ISdfShape for GlyphAura's layout engine.
let private makeRectShape (x: float32) (y: float32) (w: float32) (h: float32) : GlyphAura.Shapes.ISdfShape =
    { new GlyphAura.Shapes.ISdfShape with
        member _.GetDistance(p) =
            let dx = abs (p.X - (x + w * 0.5f)) - w * 0.5f
            let dy = abs (p.Y - (y + h * 0.5f)) - h * 0.5f
            if dx <= 0.0f && dy <= 0.0f then max dx dy
            elif dx <= 0.0f then dy
            elif dy <= 0.0f then dx
            else sqrt (dx * dx + dy * dy)
        member _.GetInterval(scanY) =
            if scanY >= y && scanY <= y + h then ValueSome(struct(x, x + w))
            else ValueNone
        member _.BoundingBox =
            { GlyphAura.Shapes.Left = x; Top = y; Right = x + w; Bottom = y + h } }

/// Render a text string using GlyphAura layout + box glyph rasterization.
/// Each glyph is rendered as a filled rectangle at its positioned location.
let renderText (fb: Framebuffer) (text: string) (style: FSharp.Graphics.Core.TextStyle)
               (origin: Vec2) (maxWidth: float32) (clip: Rect voption) =
    match loadedFont.Value with
    | ValueNone -> ()  // Font not available; skip text rendering
    | ValueSome(tables, metrics) ->
        let sizePx = style.SizePx
        let shape = makeRectShape origin.X origin.Y maxWidth (sizePx * float32 (text.Length / 10 + 3))
        let req : GlyphAura.LayoutEngine.LayoutRequest = {
            Text = System.ReadOnlyMemory(text.ToCharArray())
            Shape = shape
            Font = tables
            Metrics = metrics
            FontSizePx = sizePx
            Mode = GlyphAura.LayoutEngine.Flow
            Alignment = GlyphAura.LayoutEngine.Left
        }
        let result = GlyphAura.LayoutEngine.layout req
        let glyphs : System.Span<GlyphAura.LayoutEngine.GlyphPosition> = result.Glyphs.Span
        // Scale from font units to pixels
        let scale = sizePx / float32 metrics.UnitsPerEm
        let glyphH = float32 (int metrics.Ascender - int metrics.Descender) * scale
        let glyphW = glyphH * 0.55f  // approx average advance
        let textColor = style.Color
        for i = 0 to result.Glyphs.Length - 1 do
            let gp = glyphs.[i]
            let gx = int (origin.X + gp.X)
            let gy = int (origin.Y + gp.Y - float32 metrics.Ascender * scale)
            let gw = max 1 (int glyphW)
            let gh = max 1 (int glyphH)
            // Render each glyph as a filled rectangle (box font approximation)
            for dy = 0 to gh - 1 do
                for dx = 0 to gw - 1 do
                    let alpha =
                        // Simple SDF for a character-box: solid center with AA edges
                        let ex = float32 dx / float32 gw
                        let ey = float32 dy / float32 gh
                        let edgeA = Scalar.clamp 0.0f 1.0f (ex * 10.0f) * Scalar.clamp 0.0f 1.0f ((1.0f - ex) * 10.0f)
                        let edgeB = Scalar.clamp 0.0f 1.0f (ey * 10.0f) * Scalar.clamp 0.0f 1.0f ((1.0f - ey) * 10.0f)
                        edgeA * edgeB
                    if alpha > 0.004f then
                        let col = Color.withAlpha (float32 textColor.A / 255.0f * alpha) textColor
                        fb.Composite(gx + dx, gy + dy, col, clip)
        // Return the pool buffer
        System.Buffers.ArrayPool.Shared.Return(result.GlyphBuffer)

// ─────────────────────────────────────────────────────────────────────────────
// Gaussian blur (M3 — approximate box blur)
// ─────────────────────────────────────────────────────────────────────────────

/// Approximate Gaussian blur using two-pass box blur.
let private applyBlur (fb: Framebuffer) (sigma: float32) =
    let radius = max 1 (int (sigma * 1.5f + 0.5f))
    let w = fb.Width
    let h = fb.Height
    let tmp = Array.copy fb.Pixels
    let src = fb.Pixels
    // Horizontal pass
    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            let mutable r, g, b, a = 0.0f, 0.0f, 0.0f, 0.0f
            let mutable cnt = 0.0f
            for dx = -radius to radius do
                let sx = x + dx
                if sx >= 0 && sx < w then
                    let o = (y * w + sx) * 4
                    r <- r + float32 src.[o]
                    g <- g + float32 src.[o+1]
                    b <- b + float32 src.[o+2]
                    a <- a + float32 src.[o+3]
                    cnt <- cnt + 1.0f
            let o = (y * w + x) * 4
            tmp.[o]   <- byte (r / cnt + 0.5f)
            tmp.[o+1] <- byte (g / cnt + 0.5f)
            tmp.[o+2] <- byte (b / cnt + 0.5f)
            tmp.[o+3] <- byte (a / cnt + 0.5f)
    // Vertical pass into src
    for y = 0 to h - 1 do
        for x = 0 to w - 1 do
            let mutable r, g, b, a = 0.0f, 0.0f, 0.0f, 0.0f
            let mutable cnt = 0.0f
            for dy = -radius to radius do
                let sy = y + dy
                if sy >= 0 && sy < h then
                    let o = (sy * w + x) * 4
                    r <- r + float32 tmp.[o]
                    g <- g + float32 tmp.[o+1]
                    b <- b + float32 tmp.[o+2]
                    a <- a + float32 tmp.[o+3]
                    cnt <- cnt + 1.0f
            let o = (y * w + x) * 4
            src.[o]   <- byte (r / cnt + 0.5f)
            src.[o+1] <- byte (g / cnt + 0.5f)
            src.[o+2] <- byte (b / cnt + 0.5f)
            src.[o+3] <- byte (a / cnt + 0.5f)

// ─────────────────────────────────────────────────────────────────────────────
// ColorMatrix filter (M3)
// ─────────────────────────────────────────────────────────────────────────────

/// Apply a 4×5 color matrix to each pixel in the framebuffer.
let private applyColorFilter (fb: Framebuffer) (matrix: ColorMatrix) =
    if matrix.Length < 20 then ()
    else
    let w = fb.Width
    let h = fb.Height
    let px = fb.Pixels
    for i = 0 to w * h - 1 do
        let o = i * 4
        let r = float32 px.[o]   / 255.0f
        let g = float32 px.[o+1] / 255.0f
        let b = float32 px.[o+2] / 255.0f
        let a = float32 px.[o+3] / 255.0f
        // 4×5 matrix row-major: [R_out, G_out, B_out, A_out] = M * [r, g, b, a, 1]
        let nr = matrix.[0]*r + matrix.[1]*g + matrix.[2]*b + matrix.[3]*a + matrix.[4]
        let ng = matrix.[5]*r + matrix.[6]*g + matrix.[7]*b + matrix.[8]*a + matrix.[9]
        let nb = matrix.[10]*r + matrix.[11]*g + matrix.[12]*b + matrix.[13]*a + matrix.[14]
        let na = matrix.[15]*r + matrix.[16]*g + matrix.[17]*b + matrix.[18]*a + matrix.[19]
        let clampB (v: float32) = byte (int (Scalar.clamp 0.0f 1.0f v * 255.0f + 0.5f) |> max 0 |> min 255)
        px.[o]   <- clampB nr
        px.[o+1] <- clampB ng
        px.[o+2] <- clampB nb
        px.[o+3] <- clampB na

// ─────────────────────────────────────────────────────────────────────────────
// Scene tree walker
// ─────────────────────────────────────────────────────────────────────────────

/// Walk a Scene tree and render to framebuffer, respecting clip rects and opacity.
/// textY tracks running Y for consecutive Text nodes within a Group.
let rec private renderSceneImpl (fb: Framebuffer) (scene: Scene) (clip: Rect voption) (textY: float32 ref) =
    match scene with
    | Empty -> ()
    | Shape shape -> renderShape fb shape clip
    | Group children ->
        let childTextY = ref !textY
        for c in children do renderSceneImpl fb c clip childTextY
        textY := !childTextY
    | Stack(_, children) ->
        for c in children do renderSceneImpl fb c clip textY
    | Padding(_, child) -> renderSceneImpl fb child clip textY
    | Transform(_, child) -> renderSceneImpl fb child clip textY  // Simplified: ignore matrix
    | Clip(clipRect, child) ->
        let newClip =
            match clip with
            | ValueNone -> ValueSome clipRect
            | ValueSome existing -> ValueSome (Rect.intersect existing clipRect)
        renderSceneImpl fb child newClip textY
    | Opacity(alpha, child) ->
        let offscreen = Framebuffer(fb.Width, fb.Height)
        renderSceneImpl offscreen child clip (ref !textY)
        fb.BlendFrom(offscreen, alpha)
    | Reactive(_, child) -> renderSceneImpl fb child clip textY
    | Animated(_, child) -> renderSceneImpl fb child clip textY
    | Blur(sigma, child) ->
        let offscreen = Framebuffer(fb.Width, fb.Height)
        renderSceneImpl offscreen child clip (ref !textY)
        if sigma > 0.001f then applyBlur offscreen sigma
        fb.BlendFrom(offscreen, 1.0f)
    | ColorFilter(matrix, child) ->
        let offscreen = Framebuffer(fb.Width, fb.Height)
        renderSceneImpl offscreen child clip (ref !textY)
        applyColorFilter offscreen matrix
        fb.BlendFrom(offscreen, 1.0f)
    | BlendMode(_, child) -> renderSceneImpl fb child clip textY
    | Mask(_, child) -> renderSceneImpl fb child clip textY
    | Text(txt, style) ->
        // M2: Use GlyphAura to lay out and render text at the current Y position
        let margin = 55.0f
        let origin = Vec2.create margin !textY
        renderText fb txt style origin (float32 fb.Width - margin * 2.0f) clip
        textY := !textY + style.SizePx * style.LineHeight + 4.0f
    | Image _ -> ()

/// Render a Scene tree to framebuffer (public entry for scene walker)
let renderScene (fb: Framebuffer) (scene: Scene) (clip: Rect voption) =
    renderSceneImpl fb scene clip (ref 60.0f)

/// Render a Scene to a Framebuffer
let render (scene: Scene) (width: int) (height: int) (bg: Color) : Framebuffer =
    let fb = Framebuffer(width, height)
    fb.Clear(bg)
    renderScene fb scene ValueNone
    fb
