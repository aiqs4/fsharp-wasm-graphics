/// Native desktop host.
///
///   dotnet run --project demo/FSharp.Graphics.DesktopRunner -- [--scene ID] [--software] [--font PATH]
///
/// Keys:  <- / ->  or  PgUp / PgDn  previous / next scene      1..9, 0   jump to scene
///        B  toggle Skia <-> software backend (same command stream, so differences are bugs)
///        F12  screenshot (software backend) -> screenshot-<scene>-<n>.png      Esc  quit
module FSharp.Graphics.DesktopRunner.Program

open System
open System.Diagnostics
open System.IO
open Silk.NET.Windowing
open Silk.NET.Maths
open Silk.NET.Input
open SkiaSharp
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Host
open FSharp.Graphics.Host.Skia
open FSharp.Graphics.Scenes
open FSharp.Graphics.Scenes.Kit

/// Everything that changes between frames, kept in one object so window callbacks can share it
/// (F# closures cannot capture `let mutable` locals).
type Runner(defs: SceneDef[], fonts: FontStore, startIndex: int, startSoftware: bool) =
    let text   = GlyphAuraTextEngine fonts :> ITextEngine
    let stage  = Stage(text, 1024.0f, 768.0f)
    let clock  = Stopwatch.StartNew()
    let white  = Color.rgba 255uy 255uy 255uy 255uy
    let mutable index    = ((startIndex % defs.Length) + defs.Length) % defs.Length
    let mutable started  = 0.0
    let mutable software = startSoftware
    let mutable sink     : (SKCanvas * SkiaSink) option = None
    let mutable shots    = 0
    let mutable lastErr  = ""
    let mutable bytes    = 0

    let report (r: DecodeResult) =
        match r with
        | Malformed(off, msg) ->
            let m = sprintf "malformed command stream at byte %d: %s" off msg
            if m <> lastErr then eprintfn "%s" m
            lastErr <- m
        | Decoded _ -> ()

    let frame (w: int) (h: int) =
        let ctx = { Time = float32 (clock.Elapsed.TotalMilliseconds - started)
                    Size = Vec2.create (float32 w) (float32 h)
                    Font = FontId 0UL }
        stage.Resize(float32 w, float32 h)
        let struct (buf, len) = stage.Frame(defs.[index].Build ctx)
        bytes <- len
        struct (buf, len)

    member _.Scene = defs.[index]
    member _.Software = software
    member _.Bytes = bytes

    member _.Select(i: int) =
        index <- ((i % defs.Length) + defs.Length) % defs.Length
        started <- clock.Elapsed.TotalMilliseconds

    member this.Step(d: int) = this.Select(index + d)
    member _.ToggleBackend() = software <- not software

    /// Route a pointer / key event to the scene graph.
    member _.Input(e: InputEvent) = stage.Input e |> ignore

    /// Draw one frame on `canvas` (device pixels); scene space is (w, h) logical units.
    member _.Render(canvas: SKCanvas, w: int, h: int, sx: float32, sy: float32) =
        let struct (buf, len) = frame w h
        canvas.Save() |> ignore
        canvas.Scale(sx, sy)
        if software then
            let sw = SoftwareRenderer(w, h, fonts)
            report (CommandReader.run sw buf len)
            use img = SKImage.FromPixelCopy(SKImageInfo(w, h, SKColorType.Rgba8888, SKAlphaType.Unpremul), sw.ToRgba8 white)
            canvas.DrawImage(img, 0.0f, 0.0f)
        else
            // the sink is bound to one canvas; rebuild it if the surface was recreated (resize)
            match sink with
            | Some (c, _) when obj.ReferenceEquals(c, canvas) -> ()
            | other ->
                other |> Option.iter (fun (_, s) -> (s :> IDisposable).Dispose())
                sink <- Some (canvas, new SkiaSink(canvas, fonts, SKColors.White))
            let (_, s) = sink.Value
            report (CommandReader.run s buf len)
        canvas.Restore()

    /// Software-render the current scene to a PNG next to the executable's working dir.
    member _.Screenshot(w: int, h: int) =
        let struct (buf, len) = frame w h
        let sw = SoftwareRenderer(w, h, fonts)
        report (CommandReader.run sw buf len)
        shots <- shots + 1
        let path = sprintf "screenshot-%s-%d.png" defs.[index].Id shots
        File.WriteAllBytes(path, Png.encode w h (sw.ToRgba8 white))
        printfn "saved %s" (Path.GetFullPath path)

/// GPU objects that must be recreated when the framebuffer changes.
type Gpu() =
    member val Interface : GRGlInterface = null with get, set
    member val Context   : GRContext = null with get, set
    member val Target    : GRBackendRenderTarget = null with get, set
    member val Surface   : SKSurface = null with get, set
    member this.DisposeSurface() =
        if not (isNull this.Surface) then this.Surface.Dispose(); this.Surface <- null
        if not (isNull this.Target) then this.Target.Dispose(); this.Target <- null

let rec parse (o: string option * bool * string option) (args: string list) =
    let (scene, soft, font) = o
    match args with
    | [] -> o
    | "--scene" :: s :: t -> parse (Some s, soft, font) t
    | "--software" :: t   -> parse (scene, true, font) t
    | "--font" :: f :: t  -> parse (scene, soft, Some f) t
    | x :: _ -> failwithf "unknown argument '%s'" x

[<EntryPoint>]
let main argv =
    let (sceneArg, software, fontArg) = parse (None, false, None) (List.ofArray argv)
    fontArg |> Option.iter (fun f -> Environment.SetEnvironmentVariable("FSG_FONT", f))
    let fonts = FontLocator.loadDefault ()
    let builder = FSharp.Graphics.GuiBuilder.GuiBuilderDef.make (GlyphAuraTextEngine fonts :> ITextEngine)
    let defs = Array.ofList (Scenes.all @ [ builder ])
    let startIndex =
        match sceneArg with
        | Some id -> (match Array.tryFindIndex (fun (d: SceneDef) -> d.Id = id) defs with Some i -> i | None -> failwithf "unknown scene '%s'" id)
        | None -> 0
    let runner = Runner(defs, fonts, startIndex, software)
    let gpu = Gpu()

    let mutable options = WindowOptions.Default
    options.Size <- Vector2D<int>(1024, 768)
    options.Title <- "FSharp.Graphics"
    options.VSync <- true
    options.PreferredDepthBufferBits <- 0
    options.PreferredStencilBufferBits <- 8
    let window = Window.Create(options)

    let rebuildSurface () =
        if not (isNull gpu.Context) then
            gpu.DisposeSurface()
            let fb = window.FramebufferSize
            if fb.X > 0 && fb.Y > 0 then
                let info = GRGlFramebufferInfo(0u, 0x8058u)           // GL_RGBA8
                gpu.Target  <- new GRBackendRenderTarget(fb.X, fb.Y, 0, 8, info)
                gpu.Surface <- SKSurface.Create(gpu.Context, gpu.Target, GRSurfaceOrigin.BottomLeft, SKColorType.Rgba8888)

    let frames = ref 0
    let titleClock = Stopwatch.StartNew()

    window.add_Load(Action(fun () ->
        let native = window.GLContext
        let getProc = GRGlGetProcedureAddressDelegate(fun name ->
            let mutable ptr = IntPtr.Zero
            if native.TryGetProcAddress(name, &ptr) then ptr else IntPtr.Zero)
        gpu.Interface <- GRGlInterface.CreateOpenGl getProc
        if isNull gpu.Interface then eprintfn "Skia could not create a GL interface (no usable OpenGL context)."
        else gpu.Context <- GRContext.CreateGl gpu.Interface
        rebuildSurface ()

        let input = window.CreateInput()
        let pos (p: Numerics.Vector2) = Vec2.create p.X p.Y
        for mouse in input.Mice do
            mouse.add_MouseMove(Action<IMouse, Numerics.Vector2>(fun _ p -> runner.Input(PointerMove(pos p))))
            mouse.add_MouseDown(Action<IMouse, MouseButton>(fun m b -> runner.Input(PointerDown(pos m.Position, int b))))
            mouse.add_MouseUp(Action<IMouse, MouseButton>(fun m b -> runner.Input(PointerUp(pos m.Position, int b))))
            mouse.add_Scroll(Action<IMouse, ScrollWheel>(fun _ w -> runner.Input(Scroll(Vec2.create w.X w.Y))))
        for kb in input.Keyboards do
            kb.add_KeyDown(Action<IKeyboard, Key, int>(fun _ key _ ->
                match key with
                // the GUI builder wants every key (typing, arrows); only quit / screenshot / paging stay global
                | k when runner.Scene.Id = "builder" && k <> Key.Escape && k <> Key.F12 && k <> Key.PageUp && k <> Key.PageDown ->
                    runner.Input(KeyDown(string k))
                | Key.Right | Key.PageDown -> runner.Step 1
                | Key.Left | Key.PageUp    -> runner.Step(-1)
                | Key.B                    -> runner.ToggleBackend()
                | Key.F12                  -> runner.Screenshot(window.Size.X, window.Size.Y)
                | Key.Escape               -> window.Close()
                | k when k >= Key.Number1 && k <= Key.Number9 -> runner.Select(int k - int Key.Number1)
                | Key.Number0              -> runner.Select 9
                | k -> runner.Input(KeyDown(string k))))
            kb.add_KeyUp(Action<IKeyboard, Key, int>(fun _ key _ -> runner.Input(KeyUp(string key))))
    ))

    window.add_Resize(Action<Vector2D<int>>(fun _ -> rebuildSurface ()))
    window.add_FramebufferResize(Action<Vector2D<int>>(fun _ -> rebuildSurface ()))

    window.add_Render(Action<double>(fun _ ->
        if not (isNull gpu.Surface) then
            gpu.Context.ResetContext()
            let size = window.Size
            let fb = window.FramebufferSize
            runner.Render(gpu.Surface.Canvas, size.X, size.Y, float32 fb.X / float32 size.X, float32 fb.Y / float32 size.Y)
            gpu.Surface.Canvas.Flush()
            gpu.Context.Flush()
            frames.Value <- frames.Value + 1
            if titleClock.ElapsedMilliseconds >= 500L then
                let fps = float frames.Value * 1000.0 / float titleClock.ElapsedMilliseconds
                window.Title <- sprintf "FSharp.Graphics | %s - %s | %s | %.0f fps | %d B/frame   [<-/-> scene, B backend, F12 shot]"
                                    runner.Scene.Id runner.Scene.Covers (if runner.Software then "software" else "Skia") fps runner.Bytes
                frames.Value <- 0
                titleClock.Restart()
    ))

    window.add_Closing(Action(fun () ->
        gpu.DisposeSurface()
        if not (isNull gpu.Context) then gpu.Context.Dispose()
        if not (isNull gpu.Interface) then gpu.Interface.Dispose()
    ))

    window.Run()
    0
