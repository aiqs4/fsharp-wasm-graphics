namespace FSharp.Graphics.GuiBuilder

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Tea
open FSharp.Graphics.Widgets
open FSharp.Graphics.Scenes.Kit

/// Hosts the TEA program. The scene's event handlers call `Dispatch`, which runs `update`
/// immediately; the next frame's `Build` renders the new model.
type App(text: ITextEngine) =
    let mutable model = fst (Model.init ())

    let program : Program<Model, Msg> =
        { Init = Model.init
          Update = Model.update
          View = fun m -> View.view { Theme = Theme.dark; Text = text; State = m.Ui; Time = 0.0f
                                      Lift = UiMsg; Send = ignore } (Vec2.create 1280.0f 800.0f) m }

    member _.Model = model
    member _.Program = program

    member _.Dispatch(msg: Msg) =
        let m', _cmd = Model.update model msg
        model <- m'

    member this.Build(ctx: Ctx) : Scene =
        let ui : Ui<Msg> =
            { Theme = Theme.dark; Text = text; State = model.Ui; Time = ctx.Time
              Lift = UiMsg; Send = this.Dispatch }
        View.view ui ctx.Size model

module GuiBuilderDef =
    /// The builder as a regular demo scene (re-rendered every frame because it is interactive).
    let make (text: ITextEngine) : SceneDef =
        let app = App text
        { Id = "builder"; Title = "GUI Builder"
          Covers = "Widgets + SceneDsl: drag, resize, inspect, export F#"
          Build = app.Build; Animated = true }
