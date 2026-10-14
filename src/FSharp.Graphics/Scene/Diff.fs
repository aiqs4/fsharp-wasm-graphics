/// Structural diff algorithm for the Scene graph.
/// Produces a Delta that describes only what changed between two consecutive
/// frames, enabling the encoder to issue minimal GPU updates.
namespace FSharp.Graphics.Scene

open FSharp.Graphics
open FSharp.Graphics.Primitives

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
    /// Only child deltas changed; the node structure is the same.
    | UpdateChildren of deltas: (int * Delta) list
    /// Only the transform changed.
    | UpdateTransform of matrix: Matrix3x2
    /// Only the opacity changed.
    | UpdateOpacity of alpha: float32
    /// Only the clip region changed.
    | UpdateClip of region: Rect

// ─────────────────────────────────────────────────────────────────────────────
// Diff module
// ─────────────────────────────────────────────────────────────────────────────

module Diff =

    // ── Leaf equality helpers ─────────────────────────────────────────────────

    let private fillEq (a: Fill) (b: Fill) =
        match a, b with
        | Solid ca,  Solid cb  -> ca = cb
        | LinearGradient(fa, ta, sa), LinearGradient(fb, tb, sb) ->
            fa = fb && ta = tb && sa = sb
        | RadialGradient(ca, ra, sa), RadialGradient(cb, rb, sb) ->
            ca = cb && ra = rb && sa = sb
        | ImagePaint(ia, ra, oa), ImagePaint(ib, rb, ob) ->
            ia = ib && ra = rb && oa = ob
        | _ -> false

    let private strokeEq (a: Stroke option) (b: Stroke option) =
        match a, b with
        | None, None -> true
        | Some sa, Some sb -> sa.Thickness = sb.Thickness && fillEq sa.Paint sb.Paint
        | _ -> false

    let private shadowEq (a: Shadow option) (b: Shadow option) =
        match a, b with
        | None, None -> true
        | Some sa, Some sb -> sa = sb
        | _ -> false

    let private primitiveEq (a: PrimitiveShape) (b: PrimitiveShape) =
        match a, b with
        | Rectangle(ba, ra, fa, sa, sha), Rectangle(bb, rb, fb, sb, shb) ->
            ba = bb && ra = rb && fillEq fa fb && strokeEq sa sb && shadowEq sha shb
        | Circle(ca, ra, fa, sa, sha), Circle(cb, rb, fb, sb, shb) ->
            ca = cb && ra = rb && fillEq fa fb && strokeEq sa sb && shadowEq sha shb
        | Ellipse(ca, rxa, rya, fa, sa, sha), Ellipse(cb, rxb, ryb, fb, sb, shb) ->
            ca = cb && rxa = rxb && rya = ryb && fillEq fa fb && strokeEq sa sb && shadowEq sha shb
        | Path(sa, fa, stra, sha), Path(sb, fb, strb, shb) ->
            sa.CacheKey = sb.CacheKey && fillEq fa fb && strokeEq stra strb && shadowEq sha shb
        | _ -> false

    let private textNodeEq (a: GlyphAuro.TextNode) (b: GlyphAuro.TextNode) =
        a.Text = b.Text && a.Font.Id = b.Font.Id && a.Font.Size = b.Font.Size
        && a.Color = b.Color && a.Align = b.Align
        && a.MaxWidth = b.MaxWidth && a.LetterSpacing = b.LetterSpacing

    // ── Recursive diff ────────────────────────────────────────────────────────

    /// Compute the structural delta between `prev` and `next`.
    /// Uses reference equality first for O(1) fast-path on unchanged sub-trees.
    let rec diff (prev: Scene) (next: Scene) : Delta =
        if obj.ReferenceEquals(prev, next) then NoChange
        else
        match prev, next with

        // ── Leaves ────────────────────────────────────────────────────────────
        | Shape a,  Shape b  ->
            if primitiveEq a b then NoChange else Replace next
        | Text  a,  Text  b  ->
            if textNodeEq a b then NoChange else Replace next
        | RichText(a, pa), RichText(b, pb) ->
            // GlyphAura TextNode identity: same reference = unchanged; positions differ = replace.
            if obj.ReferenceEquals(a, b) && pa = pb then NoChange else Replace next
        | Image(ia, ra, oa), Image(ib, rb, ob) ->
            if ia = ib && ra = rb && oa = ob then NoChange else Replace next

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
            // Handlers are functions – always consider them equal if child unchanged.
            match diff ca cb with
            | NoChange -> NoChange
            | d -> UpdateChildren [0, d]

        // ── Animated ─────────────────────────────────────────────────────────
        | Animated(ta, ca), Animated(tb, cb) ->
            if ta = tb then
                match diff ca cb with
                | NoChange -> NoChange
                | d -> UpdateChildren [0, d]
            else Replace next

        // ── Group ─────────────────────────────────────────────────────────────
        | Group as_, Group bs ->
            diffChildren as_ bs

        // ── Stack ─────────────────────────────────────────────────────────────
        | Stack(da, ga, aa, as_), Stack(db, gb, ab, bs) when da = db && ga = gb && aa = ab ->
            diffChildren as_ bs

        // ── Empty ─────────────────────────────────────────────────────────────
        | Empty, Empty -> NoChange

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
