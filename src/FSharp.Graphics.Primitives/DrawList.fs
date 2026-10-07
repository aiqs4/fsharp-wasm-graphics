namespace FSharp.Graphics.Primitives

open FSharp.Math
open FSharp.Graphics.Core

/// Offscreen-layer parameters. A layer renders its content in isolation and composites it onto
/// the backdrop as ONE unit (group opacity differs from per-shape opacity when shapes overlap).
/// Applied on pop in this order: blur -> colour matrix -> alpha/blend composite.
[<NoComparison>]
type LayerParams = {
    Alpha     : float32
    /// Gaussian sigma in local units (0 = none)
    BlurSigma : float32
    Blend     : Blend
    /// 4x5 colour matrix; empty = none
    Filter    : ColorMatrix
}

module LayerParams =
    let none = { Alpha = 1.0f; BlurSigma = 0.0f; Blend = Normal; Filter = [||] }

/// A single draw command
[<NoComparison>]
type DrawCommand =
    | DrawRect      of bounds: Rect * radius: CornerRadius * fill: Paint
    | DrawCircle    of center: Vec2 * radius: float32 * fill: Paint
    | DrawEllipse   of center: Vec2 * rx: float32 * ry: float32 * fill: Paint
    /// Fill of an arbitrary shape (geometry travels with the command)
    | DrawPath      of shape: ISdfShape * fill: Paint
    | DrawImage     of imageId: ImageId * bounds: Rect * opacity: float32
    | DrawTextBatch of fontId: FontId * sizePx: float32 * glyphs: GlyphPosition array * color: Color
    /// Stroke centred on the shape boundary
    | DrawStroke    of shape: ISdfShape * stroke: Stroke
    | DrawShadow    of shape: ISdfShape * shadow: Shadow
//    | SetOpacity    of float32
    | PushClip      of Rect
    | PopClip
    /// Concatenates: child points go through `m`, then through the enclosing transform.
    | PushTransform of Mat3x2
    | PopTransform
    | PushLayer     of LayerParams
    | PopLayer

/// A flat list of draw commands for a single frame
type DrawList = DrawCommand list

/// Helper for building draw lists
module DrawList =
    let empty: DrawList = []
    let inline concat (a: DrawList) (b: DrawList) : DrawList = a @ b

    /// Append the commands for one primitive (shadow -> fill -> stroke) to `out`.
    let emitShape (out: ResizeArray<DrawCommand>) (shape: PrimitiveShape) : unit =
        let sdf = Shapes.toSdf shape
        let opt f o = match o with Some x -> out.Add(f x) | None -> ()
        match shape with
        | Rectangle r ->
            opt (fun s -> DrawShadow(sdf, s)) r.Shadow
            out.Add(DrawRect(r.Bounds, r.Radius, r.Fill))
            opt (fun s -> DrawStroke(sdf, s)) r.Stroke
        | Circle c ->
            opt (fun s -> DrawShadow(sdf, s)) c.Shadow
            out.Add(DrawCircle(c.Center, c.Radius, c.Fill))
            opt (fun s -> DrawStroke(sdf, s)) c.Stroke
        | Ellipse e ->
            opt (fun s -> DrawShadow(sdf, s)) e.Shadow
            out.Add(DrawEllipse(e.Center, e.RadiusX, e.RadiusY, e.Fill))
            opt (fun s -> DrawStroke(sdf, s)) e.Stroke
        | Path (s, f, st, sh) ->
            opt (fun x -> DrawShadow(s, x)) sh
            out.Add(DrawPath(s, f))
            opt (fun x -> DrawStroke(s, x)) st

    /// Build draw list from a primitive shape
    let fromShape (shape: PrimitiveShape) : DrawList =
        let cmds = ResizeArray<DrawCommand>()
        emitShape cmds shape
        Seq.toList cmds
