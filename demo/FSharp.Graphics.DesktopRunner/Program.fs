module FSharp.Graphics.DesktopRunner.Program

open System
open Silk.NET.Windowing
open Silk.NET.Maths
open Silk.NET.OpenGL
open SkiaSharp
open FSharp.Graphics.DesktopRunner.OpcodeDecoder

// Safe-nullness compliant state
let mutable grContext: GRContext option = None
let mutable renderTarget: GRBackendRenderTarget option = None
let mutable surface: SKSurface option = None
let mutable glInterface: GRGlInterface option = None
let mutable defaultTypeface: SKTypeface option = None

[<EntryPoint>]
let main _ =
    // FS0256 Fix: Options must be mutable to assign properties
    let mutable options = WindowOptions.Default
    options.Size <- Vector2D<int>(1024, 768)
    options.Title <- "FSharp.Graphics | Native Skia+Silk.NET Runner"
    options.VSync <- true
    options.PreferredDepthBufferBits <- 0
    options.PreferredStencilBufferBits <- 8

    let window = Window.Create(options)
    let mutable gl: GL option = None

    // window.add_Load(Action(fun () ->
    //     gl <- Some(GL.GetApi(window))
    //     
    //     let glif = GRGlInterface.Create()
    //     glInterface <- Some glif
    //     grContext <- Some(GRContext.CreateGl(glif))
    //     
    //     let tf = SKTypeface.FromFamilyName("Arial")
    //     defaultTypeface <- Some(if isNull tf then SKTypeface.Default else tf)
    // ))

    window.add_Load(Action(fun () ->
        let api = GL.GetApi(window)
        gl <- Some api
        
        // Nullbarkeit von window.GLContext absichern
        match Option.ofObj window.GLContext with
        | Some nativeContext ->
            // Explizite Erstellung des von SkiaSharp erwarteten Delegate-Typs
            let getGlProc = GRGlGetProcedureAddressDelegate(fun name ->
                let mutable ptr = IntPtr.Zero
                if nativeContext.TryGetProcAddress(name, &ptr) then ptr
                else IntPtr.Zero
            )
            
            let glif = GRGlInterface.CreateOpenGl(getGlProc)
            glInterface <- Option.ofObj glif
            
            match glInterface with
            | Some iface ->
                let ctx = GRContext.CreateGl(iface)
                grContext <- Option.ofObj ctx
                
                if Option.isNone grContext then
                    printfn "FEHLER: GRContext konnte nicht aus dem Interface erstellt werden!"
            | None ->
                printfn "FEHLER: GRGlInterface konnte über Silk.NET nicht erstellt werden!"
        | None ->
            printfn "FEHLER: window.GLContext ist null!"
            
        let tf = SKTypeface.FromFamilyName("Arial")
        defaultTypeface <- Option.ofObj (if isNull tf then SKTypeface.Default else tf)
    ))


    window.add_Resize(Action<Vector2D<int>>(fun newSize ->
        match grContext with
        | Some ctx ->
            surface |> Option.iter (fun s -> s.Dispose())
            renderTarget |> Option.iter (fun rt -> rt.Dispose())

            let framebufferInfo = new GRGlFramebufferInfo(0u, 0x8058u)
            let rt = new GRBackendRenderTarget(newSize.X, newSize.Y, 0, 8, framebufferInfo)
            renderTarget <- Some rt
            
            surface <- Some(SKSurface.Create(ctx, rt, GRSurfaceOrigin.BottomLeft, SKColorType.Rgba8888))
        | None -> ()
    ))

    window.add_Render(Action<double>(fun _delta ->
        match surface, grContext, defaultTypeface with
        | Some surf, Some ctx, Some tf ->
            ctx.ResetContext()
            let canvas = surf.Canvas
            
            // ------------------------------------------------------------------
            // TODO: Plug your actual public graphics pipeline in here.
            // Since FSharp.Graphics.Render.Internal is locked, you must call 
            // whatever public entrypoint you have that returns the opcode buffer.
            // e.g., let bufferArray = Render.encode myScene
            // ------------------------------------------------------------------
            
            // A dummy frame buffer so this file safely compiles and runs a test clear:
            // [BeginFrame; R=40, G=44, B=52, A=255; EndFrame]
            let bufferArray = [| 0xFEuy; 40uy; 44uy; 52uy; 255uy; 0xFFuy |] 
            
            let span = ReadOnlySpan<byte>(bufferArray, 0, bufferArray.Length)
            let decoder = SkiaOpcodeDecoder(canvas, tf)
            decoder.DecodeAndRender(span)

            canvas.Flush()
            ctx.Flush()
        | _ -> ()
    ))

    window.add_Closing(Action(fun () ->
        surface |> Option.iter (fun x -> x.Dispose())
        renderTarget |> Option.iter (fun x -> x.Dispose())
        grContext |> Option.iter (fun x -> x.Dispose())
        glInterface |> Option.iter (fun x -> x.Dispose())
        defaultTypeface |> Option.iter (fun x -> x.Dispose())
    ))

    window.Run()
    0
