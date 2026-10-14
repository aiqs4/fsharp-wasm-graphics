module FSharp.Math.Tests.ColorTests

open FSharp.Math
open FsCheck
open FSharp.Math.Tests.Generators

let private test name f =
    try Check.One(fsCheckConfig, f); printfn "  PASS  %s" name
    with e -> printfn "  FAIL  %s\n         %s" name e.Message

let run () =
    printfn "-- Color properties --"

    test "Blend with Transparent fg leaves bg unchanged" (fun (bg: Color) ->
        let result = Color.blend bg Color.Transparent
        result = bg
    )

    test "Lerp(a, b, 0) ≈ a" (fun (a: Color, b: Color) ->
        let r = Color.lerp a b 0.0f
        abs(int r.R - int a.R) <= 1 && abs(int r.G - int a.G) <= 1 &&
        abs(int r.B - int a.B) <= 1 && abs(int r.A - int a.A) <= 1
    )

    test "Lerp(a, b, 1) ≈ b" (fun (a: Color, b: Color) ->
        let r = Color.lerp a b 1.0f
        abs(int r.R - int b.R) <= 1 && abs(int r.G - int b.G) <= 1 &&
        abs(int r.B - int b.B) <= 1 && abs(int r.A - int b.A) <= 1
    )

    test "toArgb/fromArgb roundtrip" (fun (c: Color) ->
        let rt = Color.fromArgb (Color.toArgb c)
        rt = c
    )

    test "withAlpha(1.0) preserves alpha" (fun (c: Color) ->
        let r = Color.withAlpha 1.0f c
        abs(int r.A - int c.A) <= 1
    )

    test "withAlpha(0.0) gives zero alpha" (fun (c: Color) ->
        let r = Color.withAlpha 0.0f c
        r.A <= 1uy
    )
