/// FSharp.Graphics WASM Tests.
/// Dual-mode: runs via `dotnet run` on .NET (CI) and compiles to WasmGC
/// for in-browser testing via wasmecto (tools/fsharp-wasmecto).
module FSharp.Graphics.WasmTests.Tests

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene
open FSharp.Graphics.Render
open FSharp.Graphics.Tea

#if FABLE_COMPILER
open Fable.Core
[<Import("WasmTest", "wasmecto")>]
let private WasmTest : obj = nativeOnly
#endif

// ─────────────────────────────────────────────────────────────────────────────
// Minimal test framework (replaced by wasmecto when compiled to WASM)
// ─────────────────────────────────────────────────────────────────────────────

type TestResult = Pass | Fail of string

let private results = System.Collections.Generic.List<string * TestResult>()

let private test (name: string) (f: unit -> unit) : string * (unit -> unit) = name, f

let private suite (suiteName: string) (tests: (string * (unit -> unit)) list) =
    for (name, f) in tests do
        let fullName = sprintf "%s / %s" suiteName name
        try
            f ()
            results.Add(fullName, Pass)
        with ex ->
            results.Add(fullName, Fail ex.Message)

let private check (cond: bool) (msg: string) =
    if not cond then failwithf "FAIL: %s" msg

let private approxEq (a: float32) (b: float32) =
    abs (a - b) < 0.001f

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

let private redRect bounds =
    Scene.Shape(Rectangle {
        Bounds  = bounds
        Radius  = CornerRadius.zero
        Fill    = Paint.solid Color.Red
        Stroke  = None
        Shadow  = None
    })

// ─────────────────────────────────────────────────────────────────────────────
// Test suites
// ─────────────────────────────────────────────────────────────────────────────

let mathTests () =
    suite "Math" [
        test "Vec2 zero" (fun () ->
            let v = Vec2.zero
            check (v.X = 0.0f && v.Y = 0.0f) "Vec2.zero should be (0,0)")

        test "Vec2 create" (fun () ->
            let v = Vec2.create 3.0f 4.0f
            check (v.X = 3.0f && v.Y = 4.0f) "Vec2.create")

        test "Vec2 len" (fun () ->
            let v = Vec2.create 3.0f 4.0f
            check (approxEq (Vec2.len v) 5.0f) "Vec2.len expected 5")

        test "Vec2 add" (fun () ->
            let c = Vec2.add (Vec2.create 1.0f 2.0f) (Vec2.create 3.0f 4.0f)
            check (c.X = 4.0f && c.Y = 6.0f) "Vec2.add")

        test "Scalar clamp" (fun () ->
            check (Scalar.clamp 0.0f 1.0f 2.0f = 1.0f) "clamp hi"
            check (Scalar.clamp 0.0f 1.0f -1.0f = 0.0f) "clamp lo"
            check (Scalar.clamp 0.0f 1.0f 0.5f = 0.5f) "clamp mid")

        test "Mat3x2 identity transform" (fun () ->
            let v = Vec2.create 3.0f 4.0f
            let r = Mat3x2.transformPoint Mat3x2.identity v
            check (approxEq r.X v.X && approxEq r.Y v.Y) "identity no-op")

        test "Mat3x2 translation" (fun () ->
            let r = Mat3x2.transformPoint (Mat3x2.translation 10.0f 20.0f) Vec2.zero
            check (approxEq r.X 10.0f && approxEq r.Y 20.0f) "translation (10,20)")

        test "Color constants" (fun () ->
            check (Color.Red.R = 255uy && Color.Red.G = 0uy) "Red"
            check (Color.Blue.B = 255uy && Color.Blue.R = 0uy) "Blue"
            check (Color.Transparent.A = 0uy) "Transparent.A")

        test "Rect fields" (fun () ->
            let r = { X=1.0f; Y=2.0f; W=3.0f; H=4.0f }
            check (r.X = 1.0f && r.W = 3.0f) "Rect fields")
    ]

let sceneTests () =
    suite "Scene" [
        test "Empty" (fun () ->
            match Scene.Empty with Empty -> () | _ -> failwith "expected Empty")

        test "Shape(Rectangle)" (fun () ->
            match redRect { X=0.0f; Y=0.0f; W=100.0f; H=100.0f } with
            | Shape(Rectangle _) -> ()
            | _ -> failwith "expected Shape(Rectangle)")

        test "Group" (fun () ->
            let s = Scene.Group [redRect { X=0.0f; Y=0.0f; W=50.0f; H=50.0f }; Scene.Empty]
            match s with
            | Group nodes -> check (nodes.Length = 2) "group 2 children"
            | _ -> failwith "expected Group")

        test "Transform" (fun () ->
            let m = Mat3x2.translation 50.0f 50.0f
            match Scene.Transform(m, Scene.Empty) with
            | Transform(m2, _) -> check (m2 = m) "matrix preserved"
            | _ -> failwith "expected Transform")

        test "Opacity" (fun () ->
            match Scene.Opacity(0.5f, Scene.Empty) with
            | Opacity(a, _) -> check (a = 0.5f) "opacity 0.5"
            | _ -> failwith "expected Opacity")
    ]

let diffTests () =
    suite "Diff" [
        test "Empty = Empty → NoChange" (fun () ->
            match Diff.diff Scene.Empty Scene.Empty with
            | NoChange -> ()
            | _ -> failwith "expected NoChange")

        test "Empty vs Shape → Replace" (fun () ->
            match Diff.diff Scene.Empty (redRect { X=0.0f; Y=0.0f; W=10.0f; H=10.0f }) with
            | Replace _ -> ()
            | _ -> failwith "expected Replace")

        test "Same shape → NoChange" (fun () ->
            let s = redRect { X=0.0f; Y=0.0f; W=10.0f; H=10.0f }
            match Diff.diff s s with
            | NoChange -> ()
            | _ -> failwith "expected NoChange")

        test "diffOpt None → Replace" (fun () ->
            match Diff.diffOpt None (redRect { X=0.0f; Y=0.0f; W=5.0f; H=5.0f }) with
            | Replace _ -> ()
            | _ -> failwith "expected Replace")
    ]

let encodeTests () =
    suite "Encode" [
        test "Empty list: header + footer" (fun () ->
            let (buf, len) = Encode.encode []
            check (len > 0) "non-zero length"
            check (buf.[0] = 0xFEuy) "BeginFrame opcode"
            check (buf.[len-1] = 0xFFuy) "EndFrame opcode")

        test "DrawRect deterministic" (fun () ->
            let cmd = DrawRect(
                { X=0.0f; Y=0.0f; W=100.0f; H=100.0f },
                CornerRadius.zero,
                Paint.solid Color.Red)
            let (b1, l1) = Encode.encode [cmd]
            let (b2, l2) = Encode.encode [cmd]
            check (l1 = l2) "same length"
            check (b1.[..l1-1] = b2.[..l2-1]) "same bytes")

        test "DrawCircle adds bytes" (fun () ->
            let (_, el) = Encode.encode []
            let (_, cl) = Encode.encode [DrawCircle(Vec2.create 50.0f 50.0f, 30.0f, Paint.solid Color.Blue)]
            check (cl > el) "circle adds bytes")
    ]

let teaTests () =
    suite "TEA" [
        test "Init model" (fun () ->
            let rt = Runtime.start {
                Init   = fun () -> 42, CmdNone
                Update = fun m _ -> m, CmdNone
                View   = fun _ -> Scene.Empty }
            check (rt.GetModel() = 42) "model = 42")

        test "Dispatch" (fun () ->
            let rt = Runtime.start {
                Init   = fun () -> 0, CmdNone
                Update = fun m msg -> m + msg, CmdNone
                View   = fun _ -> Scene.Empty }
            rt.Dispatch 5
            rt.Dispatch 3
            check (rt.GetModel() = 8) "model = 8")

        test "RenderFrame non-empty" (fun () ->
            let rt = Runtime.start {
                Init   = fun () -> (), CmdNone
                Update = fun () _ -> (), CmdNone
                View   = fun () -> redRect { X=0.0f; Y=0.0f; W=200.0f; H=100.0f }
            }
            let (_, len) = rt.RenderFrame()
            check (len > 0) "len > 0")

        test "runHeadless frame count" (fun () ->
            let prog = {
                Init   = fun () -> 0, CmdNone
                Update = fun m msg -> m + msg, CmdNone
                View   = fun _ -> Scene.Empty }
            let bufs = Runtime.runHeadless prog [[1];[2];[3]]
            check (bufs.Length = 3) "3 frames")

        test "Determinism: payload bytes match" (fun () ->
            let prog = {
                Init   = fun () -> 0, CmdNone
                Update = fun m msg -> m + msg, CmdNone
                View   = fun model -> redRect { X=float32 model; Y=0.0f; W=10.0f; H=10.0f } }
            let run () = Runtime.runHeadless prog [[1;2];[3]]
            let r1 = run ()
            let r2 = run ()
            check (r1.Length = r2.Length) "same buffer count"
            List.iter2 (fun (a: byte[]) (b: byte[]) ->
                check (a.Length = b.Length) "same length"
                if a.Length > 5 then check (a.[5..] = b.[5..]) "same payload"
            ) r1 r2)

        test "Cmd.emit runs on init" (fun () ->
            let rt = Runtime.start {
                Init   = fun () -> 0, Cmd.emit 10
                Update = fun m msg -> m + msg, CmdNone
                View   = fun _ -> Scene.Empty }
            check (rt.GetModel() = 10) "emit applied")
    ]

let wasmEntryTests () =
    suite "WasmEntry" [
        test "renderFrame with no program" (fun () ->
            FSharp.Graphics.Wasm.WasmEntry.renderFrame 0.0)

        test "mountProgram dispatch" (fun () ->
            let prog : Program<unit, unit> = {
                Init   = fun () -> (), CmdNone
                Update = fun () _ -> (), CmdNone
                View   = fun () -> Scene.Empty }
            let dispatch = FSharp.Graphics.Wasm.WasmEntry.mountProgram prog
            dispatch ()
            FSharp.Graphics.Wasm.WasmEntry.renderFrame 16.0)

#if !FABLE_COMPILER
        test "submit len > 0 after frame" (fun () ->
            let prog : Program<unit, unit> = {
                Init   = fun () -> (), CmdNone
                Update = fun () _ -> (), CmdNone
                View   = fun () -> redRect { X=0.0f; Y=0.0f; W=100.0f; H=100.0f } }
            let _ = FSharp.Graphics.Wasm.WasmEntry.mountProgram prog
            FSharp.Graphics.Wasm.WasmEntry.renderFrame 0.0
            check (FSharp.Graphics.Wasm.WasmEntry.getLastSubmitLen() > 0) "submit len > 0")
#endif
    ]

// ─────────────────────────────────────────────────────────────────────────────
// Entry point
// ─────────────────────────────────────────────────────────────────────────────

let runAllTests () =
    mathTests ()
    sceneTests ()
    diffTests ()
    encodeTests ()
    teaTests ()
    wasmEntryTests ()
    let passed = results |> Seq.filter (snd >> (=) Pass) |> Seq.length
    let failed = results |> Seq.filter (fun (_, r) -> match r with Fail _ -> true | _ -> false) |> Seq.length
    for (name, result) in results do
        match result with
        | Pass     -> printfn "  PASS  %s" name
        | Fail msg -> printfn "  FAIL  %s — %s" name msg
    printfn "\n==========================="
    printfn "Results: %d passed, %d failed" passed failed
    if failed > 0 then failwithf "%d WASM tests failed" failed

[<EntryPoint>]
let main _ =
    runAllTests ()
    0
