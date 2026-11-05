namespace FSharp.Graphics.Scene

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives

/// Picking and event routing over a *resolved* scene.
///
/// Identity: the scene is immutable and rebuilt every frame, so handler closures are new
/// objects each time. A target is therefore identified by its structural path (child indices
/// from the root), which is stable across rebuilds as long as the tree shape is - the same
/// idea as key-less reconciliation in React / Elm's virtual DOM.
module Hit =

    [<Struct>]
    type Target = { Id: int; Handler: InputEvent -> bool; ToLocal: Mat3x2 }

    let private contains (p: Vec2) (r: Rect) = Rect.contains p r

    /// `toLocal` maps window coordinates into the current node's local space (accumulated inverse of
    /// the enclosing transforms); handlers receive pointer positions in *their own* local space.
    let rec private collect (s: Scene) (p: Vec2) (toLocal: Mat3x2) (path: int) (acc: ResizeArray<Target>) : bool =
        let child i c = collect c p toLocal (path * 31 + i + 1) acc
        match s with
        | Empty | Text _ -> false
        | Shape sh ->
            let slack =
                match sh.Stroke with
                | Some st -> st.Width * 0.5f
                | None    -> 0.0f
            (Shapes.toSdf sh).Sdf p <= slack
        | GlyphRun(_, _, _, _, b) -> contains p b
        | Image(_, r, _) -> contains p r
        | Group cs | Stack(_, cs) ->
            // topmost (last drawn) child wins; stop at the first hit
            let arr = List.toArray cs
            let mutable hit = false
            let mutable i = arr.Length - 1
            while not hit && i >= 0 do
                hit <- child i arr.[i]
                i <- i - 1
            hit
        | Scene.Clip(r, c) -> contains p r && child 0 c
        | Transform(m, c) ->
            match Mat3x2.invert m with
            | Some inv -> collect c (Mat3x2.transformPoint inv p) (Mat3x2.multiply toLocal inv) (path * 31 + 1) acc
            | None -> false
        | Reactive(h, c) ->
            let id = path * 31 + 7
            let hit = collect c p toLocal id acc
            if hit then acc.Add { Id = id; Handler = h; ToLocal = toLocal }
            hit
        | Padding(_, c) | Opacity(_, c) | Animated(_, c) | Blur(_, c)
        | ColorFilter(_, c) | BlendMode(_, c) | Mask(_, c) -> child 0 c

    /// Reactive targets under `p`, innermost first (bubbling order).
    let pick (scene: Scene) (p: Vec2) : Target[] =
        let acc = ResizeArray<Target>()
        collect scene p Mat3x2.identity 17 acc |> ignore
        acc.ToArray()

    /// Stateful router: hover tracking (enter/leave), press capture, bubbling until handled.
    type Dispatcher() =
        let mutable hover   : Target[] = [||]
        let mutable pressed : Target[] = [||]

        let has (ts: Target[]) (t: Target) = ts |> Array.exists (fun x -> x.Id = t.Id)
        /// Express pointer positions in the target's own coordinate space.
        let local (t: Target) (evt: InputEvent) =
            let f p = Mat3x2.transformPoint t.ToLocal p
            match evt with
            | PointerDown(p, b)  -> PointerDown(f p, b)
            | PointerUp(p, b)    -> PointerUp(f p, b)
            | PointerMove p      -> PointerMove(f p)
            | PointerEnter p     -> PointerEnter(f p)
            | PointerLeave p     -> PointerLeave(f p)
            | other -> other
        let send (t: Target) (evt: InputEvent) = t.Handler (local t evt)
        let bubble (ts: Target[]) (evt: InputEvent) = ts |> Array.exists (fun t -> send t evt)

        /// Ids of the targets currently under the pointer.
        member _.Hover = hover |> Array.map (fun t -> t.Id)

        /// Route one event against the current resolved scene. True if a handler consumed it.
        member _.Handle(scene: Scene, evt: InputEvent) : bool =
            match evt with
            | PointerMove p ->
                let now = pick scene p
                for t in hover do if not (has now t) then send t (PointerLeave p) |> ignore
                for t in now   do if not (has hover t) then send t (PointerEnter p) |> ignore
                hover <- now
                // implicit capture: while pressed, the press target keeps receiving moves (drags)
                bubble (if pressed.Length > 0 then pressed else now) evt
            | PointerDown(p, _) ->
                pressed <- pick scene p
                bubble pressed evt
            | PointerUp(p, _) ->
                // the press target keeps the pointer until release (implicit capture)
                let ts = if pressed.Length > 0 then pressed else pick scene p
                pressed <- [||]
                bubble ts evt
            | Scroll _ | KeyDown _ | KeyUp _ -> bubble hover evt
            | PointerEnter _ | PointerLeave _ -> false
