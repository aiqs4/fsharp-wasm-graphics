module FSharp.Math.Tests.Generators

open FSharp.Math
open FsCheck

module FsGen = FsCheck.FSharp.Gen
let gen = FsCheck.FSharp.GenBuilder.gen

/// Generate a bounded float32
let genFloat (lo: float32) (hi: float32) =
    FsGen.choose(int(lo * 100.0f), int(hi * 100.0f))
    |> FsGen.map (fun i -> float32 i / 100.0f)

/// Generate a float32 in [-1000, 1000]
let genSmallFloat = genFloat -1000.0f 1000.0f

/// Generate a Vec2
let genVec2 = gen {
    let! x = genSmallFloat
    let! y = genSmallFloat
    return Vec2.create x y
}

/// Generate a non-singular Mat3x2 (invertible)
let genInvertibleMat3x2 = gen {
    let! tx = genSmallFloat
    let! ty = genSmallFloat
    let! sx = genFloat 0.1f 10.0f
    let! sy = genFloat 0.1f 10.0f
    let! angle = genFloat 0.0f 6.28f
    let t = Mat3x2.translation tx ty
    let s = Mat3x2.scale sx sy
    let r = Mat3x2.rotation angle
    return Mat3x2.multiply (Mat3x2.multiply t s) r
}

/// Generate any Mat3x2
let genMat3x2 = gen {
    let! m11 = genSmallFloat
    let! m12 = genSmallFloat
    let! m21 = genSmallFloat
    let! m22 = genSmallFloat
    let! m31 = genSmallFloat
    let! m32 = genSmallFloat
    return { M11 = m11; M12 = m12; M21 = m21; M22 = m22; M31 = m31; M32 = m32 } : Mat3x2
}

/// Generate a positive-size Rect
let genRect = gen {
    let! x = genSmallFloat
    let! y = genSmallFloat
    let! w = genFloat 0.01f 500.0f
    let! h = genFloat 0.01f 500.0f
    return Rect.create x y w h
}

/// Generate a Color
let genColor = gen {
    let! r = FsGen.choose(0, 255) |> FsGen.map uint8
    let! g = FsGen.choose(0, 255) |> FsGen.map uint8
    let! b = FsGen.choose(0, 255) |> FsGen.map uint8
    let! a = FsGen.choose(0, 255) |> FsGen.map uint8
    return Color.rgba r g b a
}

/// FsCheck Arbitrary instances
type MathArbitraries =
    static member Vec2() : Arbitrary<Vec2> =
        { new Arbitrary<Vec2>() with
            override _.Generator = genVec2 }
    static member Mat3x2() : Arbitrary<Mat3x2> =
        { new Arbitrary<Mat3x2>() with
            override _.Generator = genMat3x2 }
    static member Rect() : Arbitrary<Rect> =
        { new Arbitrary<Rect>() with
            override _.Generator = genRect }
    static member Color() : Arbitrary<Color> =
        { new Arbitrary<Color>() with
            override _.Generator = genColor }

/// Standard test config
let fsCheckConfig =
    Config.QuickThrowOnFailure
        .WithMaxTest(200)
        .WithArbitrary([typeof<MathArbitraries>])

let fewTestsConfig =
    Config.QuickThrowOnFailure
        .WithMaxTest(50)
        .WithArbitrary([typeof<MathArbitraries>])
