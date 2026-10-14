/// Internal mutable cache layer for the retained-mode render pass.
/// NOT part of the public API – consumers use Scene values directly.
namespace FSharp.Graphics.Render

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

// ─────────────────────────────────────────────────────────────────────────────
// Mutable state helpers (F# DU cases cannot have mutable fields directly)
// ─────────────────────────────────────────────────────────────────────────────

/// Mutable wrapper for shape rendering state
[<AllowNullLiteral>]
type internal ShapeState(primitive: PrimitiveShape) =
    member val Primitive      : PrimitiveShape   = primitive
    member val Dirty          : bool             = true       with get, set
    member val CachedDrawList : DrawList voption = ValueNone with get, set

/// Internal animation state
[<AllowNullLiteral>]
type internal AnimState() =
    member val StartTimeMs = 0.0f with get, set
    member val CurrentT    = 0.0f with get, set
    member val IsComplete  = false with get, set

// ─────────────────────────────────────────────────────────────────────────────
// CachedNode
// ─────────────────────────────────────────────────────────────────────────────

/// Internal cached node tree — retains mutable state for dirty tracking
[<NoComparison>]
type internal CachedNode =
    | CachedShape     of state: ShapeState
    | CachedText      of text: string * style: TextStyle * dirty: bool ref
    | CachedImage     of imageId: ImageId * destRect: Rect * opacity: float32
    | CachedGroup     of children: CachedNode array * dirty: bool ref * lastBounds: Rect ref
    | CachedStack     of props: StackProps * children: CachedNode array * dirty: bool ref
    | CachedClip      of region: Rect * child: CachedNode * dirty: bool ref
    | CachedPadding   of insets: Insets * child: CachedNode * dirty: bool ref
    | CachedTransform of matrix: Mat3x2 * child: CachedNode * dirty: bool ref
    | CachedOpacity   of alpha: float32 * child: CachedNode * dirty: bool ref
    | CachedReactive  of handler: (InputEvent -> bool) * child: CachedNode * dirty: bool ref
    | CachedAnimated  of transition: Transition * animState: AnimState * child: CachedNode * dirty: bool ref
    | CachedBlur      of sigma: float32 * child: CachedNode * dirty: bool ref
    | CachedEmpty

// ─────────────────────────────────────────────────────────────────────────────
// CachedNode operations
// ─────────────────────────────────────────────────────────────────────────────

module internal CachedNode =

    /// Build a cache tree from a Scene tree
    let rec build (scene: Scene) : CachedNode =
        match scene with
        | Scene.Empty               -> CachedEmpty
        | Scene.Shape p             -> CachedShape(ShapeState(p))
        | Scene.Text(t, style)      -> CachedText(t, style, ref true)
        | Scene.Image(id, r, op)    -> CachedImage(id, r, op)
        | Scene.Group children      ->
            CachedGroup(Array.map build (List.toArray children), ref true, ref Rect.zero)
        | Scene.Stack(props, children) ->
            CachedStack(props, Array.map build (List.toArray children), ref true)
        | Scene.Clip(r, c)          -> CachedClip(r, build c, ref true)
        | Scene.Padding(ins, c)     -> CachedPadding(ins, build c, ref true)
        | Scene.Transform(m, c)     -> CachedTransform(m, build c, ref true)
        | Scene.Opacity(a, c)       -> CachedOpacity(a, build c, ref true)
        | Scene.Reactive(h, c)      -> CachedReactive(h, build c, ref true)
        | Scene.Animated(t, c)      -> CachedAnimated(t, AnimState(), build c, ref true)
        | Scene.Blur(sigma, c)      -> CachedBlur(sigma, build c, ref true)
        // ColorFilter, BlendMode, Mask — pass through for now
        | Scene.ColorFilter(_, c)   -> build c
        | Scene.BlendMode(_, c)     -> build c
        | Scene.Mask(_, c)          -> build c

    /// Apply a delta to update the cache tree
    let rec applyDelta (delta: Delta) (node: CachedNode) : CachedNode =
        match delta with
        | NoChange -> node
        | Replace newScene -> build newScene

        | UpdateTransform newMatrix ->
            match node with
            | CachedTransform(_, c, _) -> CachedTransform(newMatrix, c, ref true)
            | _ -> node

        | UpdateOpacity newAlpha ->
            match node with
            | CachedOpacity(_, c, _) -> CachedOpacity(newAlpha, c, ref true)
            | _ -> node

        | UpdateClip newRegion ->
            match node with
            | CachedClip(_, c, _) -> CachedClip(newRegion, c, ref true)
            | _ -> node

        | UpdateAnimation newTransition ->
            match node with
            | CachedAnimated(_, st, c, _) -> CachedAnimated(newTransition, st, c, ref true)
            | _ -> node

        | UpdateChildren childDeltas ->
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
            | CachedGroup(ch, _, lb)          -> CachedGroup(applyChild ch, ref true, lb)
            | CachedStack(props, ch, _)       -> CachedStack(props, applyChild ch, ref true)
            | CachedTransform(m, c, _)        -> CachedTransform(m, singleChild c, ref true)
            | CachedOpacity(a, c, _)          -> CachedOpacity(a, singleChild c, ref true)
            | CachedClip(r, c, _)             -> CachedClip(r, singleChild c, ref true)
            | CachedPadding(ins, c, _)        -> CachedPadding(ins, singleChild c, ref true)
            | CachedReactive(h, c, _)         -> CachedReactive(h, singleChild c, ref true)
            | CachedAnimated(t, st, c, _)     -> CachedAnimated(t, st, singleChild c, ref true)
            | CachedBlur(sigma, c, _)         -> CachedBlur(sigma, singleChild c, ref true)
            | _                               -> node

    /// Generate draw list from cached tree
    let rec getDrawList (node: CachedNode) (shapeCache: ShapeCache) : DrawList =
        match node with
        | CachedShape state ->
            match state.CachedDrawList with
            | ValueSome dl when not state.Dirty -> dl
            | _ ->
                let dl = DrawList.fromShape state.Primitive
                state.CachedDrawList <- ValueSome dl
                state.Dirty <- false
                dl

        | CachedText(_, style, dirty) ->
            // Text rendering placeholder — emit a colored rect at zero bounds
            dirty.Value <- false
            [DrawRect(Rect.zero, CornerRadius.zero, Solid style.Color)]

        | CachedImage(imageId, destRect, opacity) ->
            [DrawImage(imageId, destRect, opacity)]

        | CachedGroup(children, _, _)
        | CachedStack(_, children, _) ->
            children |> Array.toList |> List.collect (fun c -> getDrawList c shapeCache)

        | CachedClip(region, child, _) ->
            PushClip region :: getDrawList child shapeCache @ [PopClip]

        | CachedPadding(_, child, _)
        | CachedReactive(_, child, _) ->
            getDrawList child shapeCache

        | CachedAnimated(_, _, child, _) ->
            getDrawList child shapeCache

        | CachedTransform(matrix, child, _) ->
            PushTransform matrix :: getDrawList child shapeCache @ [PopTransform]

        | CachedOpacity(alpha, child, _) ->
            SetOpacity alpha :: getDrawList child shapeCache @ [SetOpacity 1.0f]

        | CachedBlur(_, child, _) ->
            // Blur is a post-processing effect — pass through for now
            getDrawList child shapeCache

        | CachedEmpty -> []
