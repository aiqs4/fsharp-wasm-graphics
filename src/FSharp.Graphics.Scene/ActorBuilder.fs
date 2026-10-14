namespace FSharp.Graphics.Scene

/// Computation expression builder for Scene trees
type SceneBuilder() =
    member _.Yield(s: Scene)   = [s]
    member _.Yield(())         = []
    member _.Combine(a: Scene list, b: Scene list) = a @ b
    member _.Delay(f: unit -> Scene list) = f()
    member _.Zero() : Scene list = []
    member _.For(seq: seq<'a>, f: 'a -> Scene list) = seq |> Seq.toList |> List.collect f
    member _.Run(children: Scene list) = Group children

[<AutoOpen>]
module SceneBuilderModule =
    /// Builder for creating Scene trees using computation expression syntax
    let scene = SceneBuilder()
