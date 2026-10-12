/// Usage:  dotnet run --project demo/FSharp.Graphics.Demo -- [--out DIR] [--scene ID]... [--size WxH] [--time MS] [--list]
/// Writes one PNG per scene plus index.html. Exit code 1 if any command stream is malformed.
module FSharp.Graphics.Demo.Program

open System
open System.IO
open System.Diagnostics
open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Render
open FSharp.Graphics.Host
open FSharp.Graphics.Scenes
open FSharp.Graphics.Scenes.Kit

type Options = { Out: string; Scenes: string list; W: int; H: int; Time: float32; List: bool }

let parse (argv: string[]) =
    let rec go (o: Options) (args: string list) =
        match args with
        | [] -> o
        | "--out" :: d :: t   -> go { o with Out = d } t
        | "--scene" :: s :: t -> go { o with Scenes = o.Scenes @ [ s ] } t
        | "--size" :: s :: t  ->
            let p = s.Split 'x'
            go { o with W = int p.[0]; H = int p.[1] } t
        | "--time" :: m :: t  -> go { o with Time = float32 m } t
        | "--list" :: t       -> go { o with List = true } t
        | x :: _ -> failwithf "unknown argument '%s'" x
    go { Out = "out"; Scenes = []; W = 800; H = 600; Time = 1234.0f; List = false } (List.ofArray argv)

[<EntryPoint>]
let main argv =
    let o = parse argv
    if o.List then
        for d in Scenes.all do printfn "%-12s %s" d.Id d.Covers
        0
    else
    let fonts = FontLocator.loadDefault ()
    let text = GlyphAuraTextEngine fonts
    let selected =
        if o.Scenes.IsEmpty then Scenes.all
        else Scenes.all |> List.filter (fun d -> List.contains d.Id o.Scenes)
    Directory.CreateDirectory o.Out |> ignore
    let bgWhite = Color.rgba 255uy 255uy 255uy 255uy
    let mutable failures = 0
    let rows = ResizeArray<string>()
    printfn "%-12s %8s %6s %9s %9s" "scene" "bytes" "cmds" "encode" "raster"
    for d in selected do
        let ctx = { Time = o.Time; Size = Vec2.create (float32 o.W) (float32 o.H); Font = FontId 0UL }
        let stage = Stage(text, float32 o.W, float32 o.H)
        let sw = Stopwatch.StartNew()
        let struct (buf, len) = stage.Frame(d.Build ctx)
        let tEncode = sw.Elapsed.TotalMilliseconds
        let renderer = SoftwareRenderer(o.W, o.H, fonts)
        sw.Restart()
        match CommandReader.run renderer buf len with
        | Decoded n ->
            let tRaster = sw.Elapsed.TotalMilliseconds
            let png = Png.encode o.W o.H (renderer.ToRgba8 bgWhite)
            let file = d.Id + ".png"
            File.WriteAllBytes(Path.Combine(o.Out, file), png)
            printfn "%-12s %8d %6d %7.1fms %7.0fms" d.Id len n tEncode tRaster
            rows.Add(sprintf "<figure><a href=\"%s\"><img src=\"%s\" loading=\"lazy\"></a><figcaption><b>%s</b><br><small>%s</small></figcaption></figure>" file file d.Title d.Covers)
        | Malformed(off, msg) ->
            failures <- failures + 1
            eprintfn "%-12s MALFORMED command stream at byte %d: %s" d.Id off msg
    let html =
        "<!doctype html><meta charset=utf-8><title>FSharp.Graphics scenes</title>"
        + "<style>body{font:14px system-ui;background:#0b1220;color:#e2e8f0;margin:24px}"
        + "main{display:grid;grid-template-columns:repeat(auto-fill,minmax(380px,1fr));gap:18px}"
        + "figure{margin:0;background:#111a2e;padding:10px;border-radius:10px}img{width:100%;border-radius:6px;display:block}"
        + "figcaption{margin-top:8px}small{color:#94a3b8}</style><h1>FSharp.Graphics scenes</h1><main>"
        + String.Join("", rows) + "</main>"
    File.WriteAllText(Path.Combine(o.Out, "index.html"), html)
    printfn "\n%d scene(s) -> %s%s" rows.Count (Path.GetFullPath o.Out) (if failures > 0 then sprintf "   [%d FAILED]" failures else "")
    if failures > 0 then 1 else 0
