module FSharp.Graphics.GuiBuilder.Shots.Program

open System
open System.IO
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Host
open FSharp.Graphics.Widgets
open FSharp.Graphics.SceneDsl
open FSharp.Graphics.GuiBuilder

let white = Color.rgba 255uy 255uy 255uy 255uy

[<EntryPoint>]
let main argv =
    let out = if argv.Length > 0 then argv.[0] else "shots"
    Directory.CreateDirectory out |> ignore
    let fonts = FontLocator.loadDefault ()
    let text = GlyphAuraTextEngine fonts :> ITextEngine
    let mutable failures = 0

    let save (name: string) (w: int) (h: int) (buf: byte[]) (len: int) =
        let r = SoftwareRenderer(w, h, fonts)
        match CommandReader.run r buf len with
        | Decoded n ->
            File.WriteAllBytes(Path.Combine(out, name + ".png"), Png.encode w h (r.ToRgba8 white))
            printfn "%-22s %7d bytes  %5d cmds" name len n
        | Malformed(off, msg) ->
            failures <- failures + 1
            eprintfn "%s: MALFORMED at %d: %s" name off msg

    let shot (name: string) (w: int) (h: int) (scene: Scene) =
        let stage = Stage(text, float32 w, float32 h)
        let struct (buf, len) = stage.Frame scene
        save name w h buf len

    // 1. DSL, plus proof that the optimiser changes nothing observable
    let dslScene = Showcase.dsl ()
    shot "dsl" 900 560 dslScene
    let encode (s: Scene) =
        let stage = Stage(text, 900.0f, 560.0f)
        let struct (b, l) = stage.Frame s
        Array.sub b 0 l
    let optimised = Optimize.scene dslScene
    let same = encode dslScene = encode optimised
    printfn "optimiser: %d -> %d nodes, identical command stream: %b" (countNodes dslScene) (countNodes optimised) same
    if not same then failures <- failures + 1

    // 2. widget gallery + CSG
    let galleryUi : Ui<unit> =
        { Theme = Theme.dark; Text = text; State = UiState.empty; Time = 0.0f; Lift = ignore; Send = ignore }
    shot "gallery" 900 620 (Showcase.gallery galleryUi)
    shot "gallery-light" 900 620 (Showcase.gallery { galleryUi with Theme = Theme.light })
    shot "csg" 920 240 (Showcase.csg ())

    // 3. GUI builder session driven through the real hit-tester
    let w, h = 1280, 800
    let app = App text
    let stage = Stage(text, float32 w, float32 h)
    let mutable t = 0.0f
    let frame (name: string option) =
        let ctx : FSharp.Graphics.Scenes.Kit.Ctx = { Time = t; Size = Vec2.create (float32 w) (float32 h); Font = FontId 0UL }
        let struct (buf, len) = stage.Frame(app.Build ctx)
        name |> Option.iter (fun n -> save n w h buf len)
        t <- t + 100.0f
    let move x y = frame None; stage.Input(PointerMove(Vec2.create x y)) |> ignore
    let click x y =
        move x y
        frame None
        stage.Input(PointerDown(Vec2.create x y, 0)) |> ignore
        frame None
        stage.Input(PointerUp(Vec2.create x y, 0)) |> ignore
    let drag (x0: float32) y0 x1 y1 =
        move x0 y0
        frame None
        stage.Input(PointerDown(Vec2.create x0 y0, 0)) |> ignore
        for i in 1 .. 6 do
            let k = float32 i / 6.0f
            frame None
            stage.Input(PointerMove(Vec2.create (x0 + (x1 - x0) * k) (y0 + (y1 - y0) * k))) |> ignore
        frame None
        stage.Input(PointerUp(Vec2.create x1 y1, 0)) |> ignore
    let key k = frame None; stage.Input(KeyDown k) |> ignore

    frame (Some "builder-1-initial")
    let before = app.Model
    // drag the "Sign in" heading (node 1) 160px right / 40px down
    drag 250.0f 112.0f 410.0f 152.0f
    frame (Some "builder-2-dragged")
    let n1 (m: Model) = m.Nodes |> List.find (fun n -> n.Id = 1)
    printfn "drag: heading %A -> %A" (n1 before |> fun n -> n.X, n.Y) (n1 app.Model |> fun n -> n.X, n.Y)
    let check (what: string) (ok: bool) =
        printfn "check %-34s %s" what (if ok then "ok" else "FAILED")
        if not ok then failures <- failures + 1
    check "pointer drag moves the element" ((n1 app.Model).X = (n1 before).X + 160.0f && (n1 app.Model).Y = (n1 before).Y + 40.0f)
    // add a slider from the palette, then open the code panel
    click 100.0f 336.0f
    frame (Some "builder-3-added")
    check "palette click adds + selects" (app.Model.Nodes.Length = before.Nodes.Length + 1 && app.Model.Selected = Some 6)
    // focus the inspector text field of a button and type into it through the key path
    app.Dispatch (Select (Some 5))
    app.Dispatch (UiMsg (UiMsg.Focus "insp-text"))
    for k in [ "Backspace"; "Backspace"; "O"; "K"; "Space"; "Number1" ] do key k
    check "inspector text editing via keys" ((app.Model.Nodes |> List.find (fun n -> n.Id = 5)).Text = "Sign ok 1")
    app.Dispatch (UiMsg (UiMsg.Blur))
    app.Dispatch (Select (Some 6))
    app.Dispatch ToggleCode
    frame (Some "builder-4-code")
    app.Dispatch ToggleCode
    app.Dispatch TogglePreview
    frame (Some "builder-5-preview")
    if failures > 0 then 1 else 0
