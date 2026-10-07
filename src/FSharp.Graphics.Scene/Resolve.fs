namespace FSharp.Graphics.Scene

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives

/// Turns an authored scene (with Stack / Padding / Text) into a *resolved* scene that only
/// contains positioned content: layout becomes plain translations, text becomes GlyphRuns.
/// Every backend, the diff, the hit-tester and the encoder then work on one simple tree and
/// none of them needs to know about layout.
///
/// Placement rule: a node's natural box is the union of its content bounds. A container puts
/// that box's top-left at the slot position (so an absolutely-positioned shape in a stack is
/// laid out by its size, not by its authored coordinates).
module Resolve =

    let private unionOpt (a: Rect option) (b: Rect option) =
        match a, b with
        | None, x | x, None -> x
        | Some p, Some q -> Some (Rect.union p q)

    /// Content bounds of a *resolved* scene (None = draws nothing).
    let rec bounds (s: Scene) : Rect option =
        match s with
        | Scene.Empty | Scene.Text _ -> None
        | Scene.Shape p -> Some (Shapes.bounds p)
        | Scene.GlyphRun(_, _, _, _, b) -> Some b
        | Scene.Image(_, r, _) -> Some r
        | Scene.Group cs | Stack(_, cs) -> cs |> List.fold (fun acc c -> unionOpt acc (bounds c)) None
        | Scene.Padding(i, c) ->
            bounds c |> Option.map (fun b ->
                Rect.create (b.X - i.Left) (b.Y - i.Top) (b.W + i.Left + i.Right) (b.H + i.Top + i.Bottom))
        | Scene.Clip(r, c) -> bounds c |> Option.map (Rect.intersect r)
        | Scene.Transform(m, c) -> bounds c |> Option.map (Mat3x2.transformRect m)
        | Scene.Opacity(_, c) | Reactive(_, c) | Animated(_, c) | Blur(_, c)
        | Scene.ColorFilter(_, c) | BlendMode(_, c) | Mask(_, c) -> bounds c

    let private placeAt (x: float32) (y: float32) (b: Rect) (c: Scene) : Scene =
        let dx = x - b.X
        let dy = y - b.Y
        if dx = 0.0f && dy = 0.0f then c else Transform(Mat3x2.translation dx dy, c)

    /// `maxW` is the width available to wrapping text (use infinity / Single.MaxValue for none).
    let rec resolve (te: ITextEngine) (maxW: float32) (s: Scene) : Scene =
        let go = resolve te maxW
        match s with
        | Text(t, style) ->
            let l = te.Layout(t, style, (if style.Wrap then maxW else 0.0f))
            GlyphRun(style.FontId, style.SizePx, style.Color, l.Glyphs, Rect.create 0.0f 0.0f l.Width l.Height)
        | Group cs -> Group (List.map go cs)
        | Stack({ Direction = ZStack }, cs) -> Group (List.map go cs)
        | Stack(props, cs) -> resolveStack te maxW props cs
        | Padding(i, c) ->
            let inner = resolve te (Scalar.max' 0.0f (maxW - i.Left - i.Right)) c
            match bounds inner with
            | None   -> Empty
            | Some b -> Padding(i, placeAt i.Left i.Top b inner)
        // a clip region doubles as a text box: wrapping text inside wraps at the region's width
        | Clip(r, c)        -> Scene.Clip(r, resolve te (Scalar.min' maxW r.W) c)
        | Transform(m, c)   -> Transform(m, go c)
        | Opacity(a, c)     -> Opacity(a, go c)
        | Reactive(h, c)    -> Reactive(h, go c)
        | Animated(t, c)    -> Animated(t, go c)
        | Blur(sg, c)       -> Blur(sg, go c)
        | ColorFilter(m, c) -> ColorFilter(m, go c)
        | BlendMode(b, c)   -> BlendMode(b, go c)
        | Mask(m, c)        -> Mask(go m, go c)
        | Shape _ | GlyphRun _ | Image _ | Empty -> s

    /// Horizontal / vertical flow. Cross-axis alignment is relative to the tallest/widest child;
    /// `Stretch` currently behaves like `Start` (shapes carry absolute sizes and are not resized).
    and resolveStack (te: ITextEngine) (maxW: float32) (props: StackProps) (cs: Scene list) : Scene =
        let horiz = props.Direction = Horizontal
        let items =
            cs |> List.map (resolve te maxW)
               |> List.choose (fun c -> bounds c |> Option.map (fun b -> c, b))
        let cross (b: Rect) = if horiz then b.H else b.W
        let main  (b: Rect) = if horiz then b.W else b.H
        let crossMax = items |> List.fold (fun m (_, b) -> Scalar.max' m (cross b)) 0.0f
        let step (pos: float32, acc: Scene list) (c: Scene, b: Rect) =
            let off =
                match props.Alignment with
                | Center -> (crossMax - cross b) * 0.5f
                | End_   -> crossMax - cross b
                | Stretch | Start -> 0.0f
            let x, y = if horiz then pos, off else off, pos
            pos + main b + props.Gap, placeAt x y b c :: acc
        let _, placed = List.fold step (0.0f, []) items
        Group (List.rev placed)
