namespace FSharp.Graphics.Tea

open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Render

/// Mutable headless runtime handle for testing
type HeadlessRuntime<'Model, 'Msg> = {
    Dispatch    : 'Msg -> unit
    RenderFrame : unit -> byte array * int
    GetModel    : unit -> 'Model
    LastBuffer  : unit -> byte array
}

/// Runtime helpers (the impure part)
module Runtime =

    /// Run a TEA program headlessly, processing message batches.
    /// Returns encoded command buffers for each message batch.
    let runHeadless (program: Program<'Model, 'Msg>) (msgBatches: 'Msg list list) : byte array list =
        let (initModel, initCmd) = program.Init()
        let mutable model = initModel
        let mutable prevScene = Scene.Empty
        let mutable cachedRoot = CachedNode.build Scene.Empty

        // Process init commands (just flatten and emit for now)
        let initMsgs =
            Cmd.flatten initCmd
            |> List.choose (fun c -> match c with Emit m -> Some m | _ -> None)

        for msg in initMsgs do
            let (m', _) = program.Update model msg
            model <- m'

        let results = ResizeArray<byte array>()

        for batch in msgBatches do
            for msg in batch do
                let (m', cmd) = program.Update model msg
                model <- m'
                let extraMsgs =
                    Cmd.flatten cmd
                    |> List.choose (fun c -> match c with Emit m -> Some m | _ -> None)
                for em in extraMsgs do
                    let (m'', _) = program.Update model em
                    model <- m''

            // Render frame
            let scene = program.View model
            let delta = Diff.diff prevScene scene
            let node' = CachedNode.applyDelta delta cachedRoot
            let dl = CachedNode.getDrawList node' ShapeCache.Shared
            let (buf, len) = Encode.encode dl
            let result = Array.zeroCreate len
            System.Array.Copy(buf, result, len)
            results.Add(result)

            prevScene <- scene
            cachedRoot <- node'

        Seq.toList results

    /// Start a headless runtime with imperative control
    let start (program: Program<'Model, 'Msg>) : HeadlessRuntime<'Model, 'Msg> =
        let (initModel, initCmd) = program.Init()
        let mutable model = initModel
        let mutable prevScene = Scene.Empty
        let mutable cachedRoot = CachedNode.build Scene.Empty
        let mutable lastBuf : byte array = [||]

        // Process init commands
        let initMsgs =
            Cmd.flatten initCmd
            |> List.choose (fun c -> match c with Emit m -> Some m | _ -> None)
        for msg in initMsgs do
            let (m', _) = program.Update model msg
            model <- m'

        let dispatch msg =
            let (m', cmd) = program.Update model msg
            model <- m'
            let extraMsgs =
                Cmd.flatten cmd
                |> List.choose (fun c -> match c with Emit m -> Some m | _ -> None)
            for em in extraMsgs do
                let (m'', _) = program.Update model em
                model <- m''

        let renderFrame () =
            let scene = program.View model
            let delta = Diff.diff prevScene scene
            let node' = CachedNode.applyDelta delta cachedRoot
            let dl = CachedNode.getDrawList node' ShapeCache.Shared
            let (buf, len) = Encode.encode dl
            let result = Array.zeroCreate len
            System.Array.Copy(buf, result, len)
            lastBuf <- result
            prevScene <- scene
            cachedRoot <- node'
            (result, len)

        {
            Dispatch    = dispatch
            RenderFrame = renderFrame
            GetModel    = fun () -> model
            LastBuffer  = fun () -> lastBuf
        }
