module FSharp.Graphics.DesktopRunner.Program

open System
open System.IO
open System.Diagnostics
open Silk.NET.Windowing
open Silk.NET.Maths
open Silk.NET.OpenGL
open SkiaSharp
open FSharp.Graphics
open FSharp.Graphics.Render
open FSharp.Graphics.DesktopRunner.OpcodeDecoder
open FSharp.Graphics.Shared.DemoScene
open FSharp.Graphics.Primitives

let mutable grContext: GRContext option = None
let mutable renderTarget: GRBackendRenderTarget option = None
let mutable surface: SKSurface option = None
let mutable glInterface: GRGlInterface option = None
let mutable defaultTypeface: SKTypeface option = None

[<EntryPoint>]
let main _ =
    let scene = buildScene() 
    let shapeCache = ShapeCache(256)

    let mutable options = WindowOptions.Default
    options.Size <- Vector2D<int>(1024, 768)
    options.Title <- "FSharp.Graphics | Native Skia+Silk.NET Runner"
    options.VSync <- true
    options.PreferredDepthBufferBits <- 0
    options.PreferredStencilBufferBits <- 8

    let window = Window.Create(options)
    let mutable gl: GL option = None

    window.add_Load(Action(fun () ->
        let api = GL.GetApi(window)
        gl <- Some api
        
        match Option.ofObj window.GLContext with
        | Some nativeContext ->
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
            | None -> ()
        | None -> ()
            
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
            
            // -----------------
            // let (bufferArray, length) = Pipeline.encodeScene scene shapeCache

            let cachedTree = Internal.CachedNode.build scene
            let drawList   = Internal.CachedNode.getDrawList cachedTree shapeCache
            let (bufferArray, length) = Encode.encode drawList
            // -----------------
            
            let span = ReadOnlySpan<byte>(bufferArray, 0, length)
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
