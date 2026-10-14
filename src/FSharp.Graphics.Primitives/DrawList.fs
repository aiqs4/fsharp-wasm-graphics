namespace FSharp.Graphics.Primitives

open FSharp.Math
open FSharp.Graphics.Core

/// A single draw command in the command list
type DrawCommand =
    | DrawRect      of bounds: Rect * radius: CornerRadius * fill: Paint
    | DrawCircle    of center: Vec2 * radius: float32 * fill: Paint
    | DrawEllipse   of center: Vec2 * rx: float32 * ry: float32 * fill: Paint
    | DrawPath      of sdfId: int * fill: Paint
    | DrawImage     of imageId: ImageId * bounds: Rect * opacity: float32
    | DrawTextBatch of fontId: FontId * sizePx: float32 * glyphs: GlyphPosition array * color: Color
    | DrawStroke    of shape: ISdfShape * stroke: Stroke
    | DrawShadow    of bounds: Rect * shadow: Shadow
    | SetOpacity    of float32
    | PushClip      of Rect
    | PopClip
    | PushTransform of Mat3x2
    | PopTransform

/// A flat list of draw commands for a single frame
type DrawList = DrawCommand list

/// Helper for building draw lists
module DrawList =
    let empty: DrawList = []

    let inline append (cmd: DrawCommand) (dl: DrawList) : DrawList = dl @ [ cmd ]

    let inline concat (a: DrawList) (b: DrawList) : DrawList = a @ b

    /// Build draw list from a primitive shape
    let fromShape (shape: PrimitiveShape) : DrawList =
        let cmds = ResizeArray<DrawCommand>()

        // Shadow first
        match shape with
        | Rectangle r when r.Shadow.IsSome ->
            cmds.Add(DrawShadow(r.Bounds, r.Shadow.Value))
        | Circle c when c.Shadow.IsSome ->
            let b =
                Rect.create (c.Center.X - c.Radius) (c.Center.Y - c.Radius) (c.Radius * 2.0f) (c.Radius * 2.0f)

            cmds.Add(DrawShadow(b, c.Shadow.Value))
        | Ellipse e when e.Shadow.IsSome ->
            let b =
                Rect.create
                    (e.Center.X - e.RadiusX)
                    (e.Center.Y - e.RadiusY)
                    (e.RadiusX * 2.0f)
                    (e.RadiusY * 2.0f)

            cmds.Add(DrawShadow(b, e.Shadow.Value))
        | _ -> ()

        // Fill
        match shape with
        | Rectangle r -> cmds.Add(DrawRect(r.Bounds, r.Radius, r.Fill))
        | Circle c    -> cmds.Add(DrawCircle(c.Center, c.Radius, c.Fill))
        | Ellipse e   -> cmds.Add(DrawEllipse(e.Center, e.RadiusX, e.RadiusY, e.Fill))
        | Path (s, f, _, _) -> cmds.Add(DrawPath(s.CacheKey, f))

        // Stroke
        match shape with
        | Rectangle r when r.Stroke.IsSome ->
            cmds.Add(DrawStroke(RectSdf(r.Bounds, r.Radius) :> ISdfShape, r.Stroke.Value))
        | Circle c when c.Stroke.IsSome ->
            cmds.Add(DrawStroke(CircleSdf(c.Center, c.Radius) :> ISdfShape, c.Stroke.Value))
        | Ellipse e when e.Stroke.IsSome ->
            cmds.Add(DrawStroke(EllipseSdf(e.Center, e.RadiusX, e.RadiusY) :> ISdfShape, e.Stroke.Value))
        | Path (s, _, Some stroke, _) -> cmds.Add(DrawStroke(s, stroke))
        | _ -> ()

        Seq.toList cmds
