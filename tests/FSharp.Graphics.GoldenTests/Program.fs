module FSharp.Graphics.GoldenTests.Program

open System
open System.IO

[<EntryPoint>]
let main argv =
    let generateGolden = argv |> Array.contains "--generate-golden"
    let goldenDir =
        let idx = argv |> Array.tryFindIndex ((=) "--golden-dir")
        match idx with Some i when i+1 < argv.Length -> argv.[i+1] | _ -> "golden/png"
    let actualDir =
        let idx = argv |> Array.tryFindIndex ((=) "--actual-dir")
        match idx with Some i when i+1 < argv.Length -> argv.[i+1] | _ -> ".golden-actual"

    let repoRoot =
        let rec find (dir: string) =
            if File.Exists(Path.Combine(dir, "FSharp.Graphics.slnx")) then dir
            else
                let parent = Path.GetDirectoryName(dir) |> Option.ofObj |> Option.defaultValue dir
                if parent = dir then Environment.CurrentDirectory
                else find parent
        find (AppDomain.CurrentDomain.BaseDirectory)

    let goldenAbsDir = if Path.IsPathRooted(goldenDir) then goldenDir else Path.Combine(repoRoot, goldenDir)
    let actualAbsDir = if Path.IsPathRooted(actualDir) then actualDir else Path.Combine(repoRoot, actualDir)

    printfn "=== FSharp.Graphics Golden Tests ==="
    printfn "Golden dir: %s" goldenAbsDir
    printfn "Actual dir: %s" actualAbsDir
    if generateGolden then printfn "MODE: GENERATING GOLDEN MASTERS"
    printfn ""

    let width  = 400
    let height = 300

    let mutable totalPass = 0
    let mutable totalFail = 0

    for (name, sceneFunc) in GoldenScenes.allScenes do
        let scene = sceneFunc()
        let fb = Rasterizer.render scene width height FSharp.Math.Color.White
        let passed = GoldenFramework.runGoldenTest name goldenAbsDir actualAbsDir generateGolden width height fb.Pixels
        if passed then totalPass <- totalPass + 1 else totalFail <- totalFail + 1

    printfn ""
    printfn "==========================="
    printfn "Results: %d passed, %d failed" totalPass totalFail

    if totalFail > 0 then 1 else 0
