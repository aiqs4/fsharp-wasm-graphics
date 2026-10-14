namespace FSharp.Graphics.Tea

open FSharp.Graphics.Scene

/// The TEA program definition
type Program<'Model, 'Msg> = {
    Init   : unit -> 'Model * Cmd<'Msg>
    Update : 'Model -> 'Msg -> 'Model * Cmd<'Msg>
    View   : 'Model -> Scene
}

/// Subscription type (optional, for future use)
type Sub<'Msg> = (('Msg -> unit) -> unit) list
