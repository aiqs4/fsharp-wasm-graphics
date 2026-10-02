module FSharp.Math.Tests.Program

[<EntryPoint>]
let main _ =
    printfn "=== FSharp.Math Property-Based Tests ==="
    printfn ""

    let mutable totalPass = 0
    let mutable totalFail = 0

    let originalOut = System.Console.Out
    let sw = System.IO.StringWriter()
    System.Console.SetOut(sw)

    ScalarTests.run ()
    Vec2Tests.run ()
    Mat3x2Tests.run ()
    RectTests.run ()
    ColorTests.run ()

    System.Console.SetOut(originalOut)
    let output = sw.ToString()

    for line in output.Split('\n') do
        if line.Contains("PASS") then totalPass <- totalPass + 1
        elif line.Contains("FAIL") then totalFail <- totalFail + 1
        printfn "%s" line

    printfn ""
    printfn "==========================="
    printfn "Results: %d passed, %d failed" totalPass totalFail

    if totalFail > 0 then 1 else 0
