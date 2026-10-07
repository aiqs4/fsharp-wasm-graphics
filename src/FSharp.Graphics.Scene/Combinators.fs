namespace FSharp.Graphics.Scene

open FSharp.Math

/// Additional scene query and traversal helpers
[<AutoOpen>]
module SceneCombinators =
    /// Recursively collect all leaf nodes from a scene tree
    let rec collectLeaves (s: Scene) : Scene list =
        match s with
        | Shape _ | Text _ | Image _ | GlyphRun _ -> [s]
        | Empty -> []
        | Group children | Stack(_, children) ->
            children |> List.collect collectLeaves
        | Padding(_, child) | Clip(_, child)
        | Transform(_, child) | Opacity(_, child)
        | Reactive(_, child) | Animated(_, child)
        | Blur(_, child) | ColorFilter(_, child)
        | BlendMode(_, child) ->
            collectLeaves child
        | Mask(mask, child) ->
            collectLeaves mask @ collectLeaves child

    /// Count all nodes in a scene tree
    let rec countNodes (s: Scene) : int =
        match s with
        | Empty -> 0
        | Shape _ | Text _ | Image _ | GlyphRun _ -> 1
        | Group children | Stack(_, children) ->
            1 + (children |> List.sumBy countNodes)
        | Padding(_, child) | Clip(_, child)
        | Transform(_, child) | Opacity(_, child)
        | Reactive(_, child) | Animated(_, child)
        | Blur(_, child) | ColorFilter(_, child)
        | BlendMode(_, child) ->
            1 + countNodes child
        | Mask(mask, child) ->
            1 + countNodes mask + countNodes child
