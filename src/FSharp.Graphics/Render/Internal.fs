/// Internal mutable cache layer for the retained-mode render pass.
/// NOT part of the public API – consumers use Scene values directly.
namespace FSharp.Graphics.Render.Internal

open FSharp.Graphics
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open GlyphAuro

// ─────────────────────────────────────────────────────────────────────────────
// Mutable state helpers (F# DU cases cannot have mutable fields directly)
// ─────────────────────────────────────────────────────────────────────────────

[<AllowNullLiteral>]
type internal ShapeState(primitive: PrimitiveShape) =
    member val Primitive     : PrimitiveShape   = primitive
    member val Dirty         : bool             = true       with get, set
    member val CachedDrawList: DrawList voption = ValueNone  with get, set

[<AllowNullLiteral>]
type internal TextState(node: TextNode) =
    member val Node          : TextNode         = node
    member val Dirty         : bool             = true       with get, set
    member val CachedDrawList: DrawList voption = ValueNone  with get, set

// ─────────────────────────────────────────────────────────────────────────────
// CachedNode
// ─────────────────────────────────────────────────────────────────────────────

[<NoComparison>]
type internal CachedNode =
    | CachedShape     of state     : ShapeState
    | CachedText      of state     : TextState
    | CachedImage     of imageId   : ImageId * destRect : Rect * opacity : float32
    | CachedGroup     of children  : CachedNode array * dirty : bool ref * lastBounds : Rect ref
    | CachedStack     of direction : Direction * gap : float32 * alignment : Alignment
                       * children  : CachedNode array * dirty : bool ref
    | CachedClip      of region    : Rect      * child : CachedNode * dirty : bool ref
    | CachedPadding   of insets    : Insets    * child : CachedNode * dirty : bool ref
    | CachedTransform of matrix    : Matrix3x2 * child : CachedNode * dirty : bool ref
    | CachedOpacity   of alpha     : float32   * child : CachedNode * dirty : bool ref
    | CachedReactive  of handler   : (InputEvent -> bool) * child : CachedNode * dirty : bool ref
    | CachedAnimated  of transition: Transition * child : CachedNode * dirty : bool ref
    | CachedEmpty

// ─────────────────────────────────────────────────────────────────────────────
// CachedNode operations
// ─────────────────────────────────────────────────────────────────────────────

module internal CachedNode =

    let rec build (scene: Scene) : CachedNode =
        match scene with
        | Scene.Shape p          -> CachedShape(ShapeState(p))
        | Scene.Text n           -> CachedText(TextState(n))
        | Scene.Image(id, r, op) -> CachedImage(id, r, op)
        | Scene.Group children   ->
            CachedGroup(Array.map build (List.toArray children), ref true, ref Rect.Zero)
        | Scene.Stack(d, g, a, children) ->
            CachedStack(d, g, a, Array.map build (List.toArray children), ref true)
        | Scene.Clip(r, c)      -> CachedClip(r, build c, ref true)
        | Scene.Padding(ins, c) -> CachedPadding(ins, build c, ref true)
        | Scene.Transform(m, c) -> CachedTransform(m, build c, ref true)
        | Scene.Opacity(a, c)   -> CachedOpacity(a, build c, ref true)
        | Scene.Reactive(h, c)  -> CachedReactive(h, build c, ref true)
        | Scene.Animated(t, c)  -> CachedAnimated(t, build c, ref true)
        | Scene.Empty           -> CachedEmpty

    let rec applyDelta (delta: Diff.Delta) (node: CachedNode) : CachedNode =
        match delta with
        | Diff.NoChange -> node
        | Diff.Replace newScene -> build newScene

        | Diff.UpdateTransform newMatrix ->
            match node with
            | CachedTransform(_, c, _) -> CachedTransform(newMatrix, c, ref true)
            | _ -> build (Transform(newMatrix, Empty))

        | Diff.UpdateOpacity newAlpha ->
            match node with
            | CachedOpacity(_, c, _) -> CachedOpacity(newAlpha, c, ref true)
            | _ -> build (Opacity(newAlpha, Empty))

        | Diff.UpdateClip newRegion ->
            match node with
            | CachedClip(_, c, _) -> CachedClip(newRegion, c, ref true)
            | _ -> build (Clip(newRegion, Empty))

        | Diff.UpdateChildren childDeltas ->
            let applyChild (children: CachedNode array) =
                let ch = Array.copy children
                for (i, d) in childDeltas do
                    if i < ch.Length then ch.[i] <- applyDelta d ch.[i]
                ch
            let singleChild child =
                match childDeltas with
                | [(0, d)] -> applyDelta d child
                | _        -> child
            match node with
            | CachedGroup(ch, _, lb)        -> CachedGroup(applyChild ch, ref true, lb)
            | CachedStack(dir, g, a, ch, _) -> CachedStack(dir, g, a, applyChild ch, ref true)
            | CachedTransform(m, c, _)      -> CachedTransform(m, singleChild c, ref true)
            | CachedOpacity(a, c, _)        -> CachedOpacity(a, singleChild c, ref true)
            | CachedClip(r, c, _)           -> CachedClip(r, singleChild c, ref true)
            | CachedPadding(ins, c, _)      -> CachedPadding(ins, singleChild c, ref true)
            | CachedReactive(h, c, _)       -> CachedReactive(h, singleChild c, ref true)
            | CachedAnimated(t, c, _)       -> CachedAnimated(t, singleChild c, ref true)
            | _                             -> node

    let rec getDrawList (node: CachedNode) (shapeCache: ShapeCache) : DrawList =
        match node with
        | CachedShape state ->
            match state.CachedDrawList with
            | ValueSome dl when not state.Dirty -> dl
            | _ ->
                let p  = state.Primitive
                let sh = p.ToSdfShape()
                let dl : DrawList =
                    [| match p.Shadow with
                       | Some _ -> yield DrawShape(sh, p.Fill, None, p.Shadow)
                       | None   -> ()
                       yield DrawShape(sh, p.Fill, p.Stroke, None) |]
                shapeCache.Put(sh.CacheKey, dl)
                state.CachedDrawList <- ValueSome dl
                state.Dirty <- false
                dl

        | CachedText state ->
            match state.CachedDrawList with
            | ValueSome dl when not state.Dirty -> dl
            | _ ->
                let dl = TextNode.update state.Node shapeCache
                state.CachedDrawList <- ValueSome dl
                state.Dirty <- false
                dl

        | CachedImage(imageId, destRect, opacity) ->
            [| DrawImage(imageId, destRect, opacity) |]

        | CachedGroup(children, _, _)
        | CachedStack(_, _, _, children, _) ->
            children |> Array.collect (fun c -> getDrawList c shapeCache)

        | CachedClip(region, child, _) ->
            [| yield PushClip region
               yield! getDrawList child shapeCache
               yield  PopClip |]

        | CachedPadding(_, child, _)
        | CachedReactive(_, child, _)
        | CachedAnimated(_, child, _) ->
            getDrawList child shapeCache

        | CachedTransform(matrix, child, _) ->
            [| yield SetTransform matrix
               yield! getDrawList child shapeCache
               yield  SetTransform Matrix3x2.Identity |]

        | CachedOpacity(alpha, child, _) ->
            [| yield SetOpacity alpha
               yield! getDrawList child shapeCache
               yield  SetOpacity 1.0f |]

        | CachedEmpty -> [||]
