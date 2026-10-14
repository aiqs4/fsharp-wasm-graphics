/// Structural diff algorithm for the Scene graph.
/// Produces a Delta that describes only what changed between two consecutive
/// frames, enabling the encoder to issue minimal GPU updates.
namespace FSharp.Graphics.Render

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

// ─────────────────────────────────────────────────────────────────────────────
// Delta
// ─────────────────────────────────────────────────────────────────────────────

/// Minimal set of changes between two scene frames.
[<NoEquality; NoComparison>]
type Delta =
    /// The sub-tree is identical to the previous frame – nothing to do.
    | NoChange
    /// Completely replace the sub-tree (new node type or fully new content).
    | Replace of newScene: Scene
    /// Only the opacity changed.
    | UpdateOpacity of alpha: float32
    /// Only the transform changed.
    | UpdateTransform of matrix: Mat3x2
    /// Only child deltas changed; the node structure is the same.
    | UpdateChildren of deltas: (int * Delta) list
    /// Only the clip region changed.
    | UpdateClip of region: Rect
    /// Only the animation transition changed.
    | UpdateAnimation of Transition

// ─────────────────────────────────────────────────────────────────────────────
// Diff module
// ─────────────────────────────────────────────────────────────────────────────

module Diff =

    // ── Leaf equality helpers ─────────────────────────────────────────────────

    let private paintEq (a: Paint) (b: Paint) =
        match a, b with
        | Solid ca, Solid cb -> ca = cb
        | LinearGradient(sa, aa), LinearGradient(sb, ab) -> sa = sb && aa = ab
        | RadialGradient(sa, ca), RadialGradient(sb, cb) -> sa = sb && ca = cb
        | ImageFill(ia, oa), ImageFill(ib, ob) -> ia = ib && oa = ob
        | _ -> false

    let private strokeEq (a: Stroke option) (b: Stroke option) =
        match a, b with
        | None, None -> true
        | Some sa, Some sb -> sa = sb
        | _ -> false

    let private shadowEq (a: Shadow option) (b: Shadow option) =
        match a, b with
        | None, None -> true
        | Some sa, Some sb -> sa = sb
        | _ -> false

    let private primitiveEq (a: PrimitiveShape) (b: PrimitiveShape) =
        match a, b with
        | Rectangle ra, Rectangle rb ->
            ra.Bounds = rb.Bounds && ra.Radius = rb.Radius && paintEq ra.Fill rb.Fill
            && strokeEq ra.Stroke rb.Stroke && shadowEq ra.Shadow rb.Shadow
        | Circle ca, Circle cb ->
            ca.Center = cb.Center && ca.Radius = cb.Radius && paintEq ca.Fill cb.Fill
            && strokeEq ca.Stroke cb.Stroke && shadowEq ca.Shadow cb.Shadow
        | Ellipse ea, Ellipse eb ->
            ea.Center = eb.Center && ea.RadiusX = eb.RadiusX && ea.RadiusY = eb.RadiusY
            && paintEq ea.Fill eb.Fill && strokeEq ea.Stroke eb.Stroke && shadowEq ea.Shadow eb.Shadow
        | Path(sa, fa, stra, sha), Path(sb, fb, strb, shb) ->
            sa.CacheKey = sb.CacheKey && paintEq fa fb && strokeEq stra strb && shadowEq sha shb
        | _ -> false

    // ── Recursive diff ────────────────────────────────────────────────────────

    /// Compute the structural delta between `prev` and `next`.
    /// Uses reference equality first for O(1) fast-path on unchanged sub-trees.
    let rec diff (prev: Scene) (next: Scene) : Delta =
        if obj.ReferenceEquals(prev, next) then NoChange
        else
        match prev, next with

        // ── Leaves ────────────────────────────────────────────────────────────
        | Empty, Empty -> NoChange

        | Shape a, Shape b ->
            if primitiveEq a b then NoChange else Replace next

        | Text(ta, sa), Text(tb, sb) ->
            if ta = tb && sa = sb then NoChange else Replace next

        | Image(ia, ba, oa), Image(ib, bb, ob) ->
            if ia = ib && ba = bb && oa = ob then NoChange else Replace next

        // ── Transform (targeted update) ───────────────────────────────────────
        | Transform(ma, ca), Transform(mb, cb) ->
            let childDelta = diff ca cb
            if ma = mb then
                match childDelta with
                | NoChange -> NoChange
                | _ -> UpdateChildren [0, childDelta]
            else
                match childDelta with
                | NoChange -> UpdateTransform mb
                | _ -> Replace next

        // ── Opacity (targeted update) ─────────────────────────────────────────
        | Opacity(aa, ca), Opacity(ab, cb) ->
            let childDelta = diff ca cb
            if aa = ab then
                match childDelta with
                | NoChange -> NoChange
                | _ -> UpdateChildren [0, childDelta]
            else
                match childDelta with
                | NoChange -> UpdateOpacity ab
                | _ -> Replace next

        // ── Clip (targeted update) ────────────────────────────────────────────
        | Clip(ra, ca), Clip(rb, cb) ->
            let childDelta = diff ca cb
            if ra = rb then
                match childDelta with
                | NoChange -> NoChange
                | _ -> UpdateChildren [0, childDelta]
            else
                match childDelta with
                | NoChange -> UpdateClip rb
                | _ -> Replace next

        // ── Padding ───────────────────────────────────────────────────────────
        | Padding(ia, ca), Padding(ib, cb) ->
            if ia = ib then
                match diff ca cb with
                | NoChange -> NoChange
                | d -> UpdateChildren [0, d]
            else Replace next

        // ── Reactive ──────────────────────────────────────────────────────────
        | Reactive(_, ca), Reactive(_, cb) ->
            match diff ca cb with
            | NoChange -> NoChange
            | d -> UpdateChildren [0, d]

        // ── Animated ─────────────────────────────────────────────────────────
        | Animated(ta, ca), Animated(tb, cb) ->
            if ta = tb then
                match diff ca cb with
                | NoChange -> NoChange
                | d -> UpdateChildren [0, d]
            else UpdateAnimation tb

        // ── Group ─────────────────────────────────────────────────────────────
        | Group as_, Group bs ->
            diffChildren as_ bs

        // ── Stack ─────────────────────────────────────────────────────────────
        | Stack(pa, as_), Stack(pb, bs) when pa = pb ->
            diffChildren as_ bs

        // ── Blur ──────────────────────────────────────────────────────────────
        | Blur(sa, ca), Blur(sb, cb) when sa = sb ->
            match diff ca cb with
            | NoChange -> NoChange
            | d -> UpdateChildren [0, d]

        // ── Structural mismatch → full replace ───────────────────────────────
        | _ -> Replace next

    /// Diff two child lists and return only the changed (index, delta) pairs.
    and private diffChildren (prev: Scene list) (next: Scene list) =
        let pa = List.toArray prev
        let na = List.toArray next
        if pa.Length <> na.Length then
            Replace(Group (List.ofArray na))
        else
            let deltas =
                [| for i = 0 to pa.Length - 1 do
                       let d = diff pa.[i] na.[i]
                       match d with
                       | NoChange -> ()
                       | _ -> yield i, d |]
            if deltas.Length = 0 then NoChange
            else UpdateChildren (List.ofArray deltas)

    /// Entry-point overload: compare optional previous scene to current.
    let diffOpt (prev: Scene option) (next: Scene) : Delta =
        match prev with
        | None   -> Replace next
        | Some p -> diff p next
