namespace FSharp.Graphics.Widgets

open FSharp.Graphics.Scene

/// Single-slot memo for building a sub-scene: while `key` is unchanged the *same* `Scene`
/// object is returned, which saves rebuilding (layout measuring, SDF construction) the subtree.
/// Keys must be plain data - closures in widget props make poor keys, so key on the model slice.
[<Sealed; AllowNullLiteral>]
type Memo<'K when 'K : equality>() =
    let mutable has = false
    let mutable lastKey = Unchecked.defaultof<'K>
    let mutable lastScene = Scene.Empty
    member _.Get(key: 'K, build: unit -> Scene) : Scene =
        if has && lastKey = key then lastScene
        else
            lastScene <- build ()
            lastKey <- key
            has <- true
            lastScene
