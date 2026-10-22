namespace FSharp.Graphics.SceneDsl

open FSharp.Math
open FSharp.Graphics.Scene

/// Semantics-preserving peephole optimiser for authored scenes (the "scene compiler" pass).
/// It runs on the *authored* tree, before `Resolve.resolve`, and only applies rewrites that
/// cannot change layout, painting or hit-testing:
///   * `Empty` children vanish; `Group []` -> `Empty`; `Group [x]` -> `x`; nested `Group`s flatten
///   * `Transform` of identity vanishes; `Transform(a, Transform(b, c))` -> one matrix
///   * `Opacity 1` vanishes; nested `Opacity` multiplies
/// `Padding`, `Clip` and `Stack` are left alone because they take part in layout.
module Optimize =

    let private isIdentity (m: Mat3x2) =
        m.M11 = 1.0f && m.M12 = 0.0f && m.M21 = 0.0f && m.M22 = 1.0f && m.M31 = 0.0f && m.M32 = 0.0f

    let rec scene (s: Scene) : Scene =
        match s with
        | Group cs -> group (List.map scene cs)
        | Stack(p, cs) -> Stack(p, List.map scene cs)
        | Transform(m, c) ->
            match scene c with
            | Transform(m2, c2) -> mkTransform (Mat3x2.multiply m2 m) c2
            | c' -> mkTransform m c'
        | Opacity(a, c) ->
            match scene c with
            | Opacity(b, c2) -> mkOpacity (a * b) c2
            | c' -> mkOpacity a c'
        | Clip(r, c)        -> Clip(r, scene c)
        | Padding(i, c)     -> Padding(i, scene c)
        | Reactive(h, c)    -> Reactive(h, scene c)
        | Animated(t, c)    -> Animated(t, scene c)
        | Blur(sg, c)       -> Blur(sg, scene c)
        | ColorFilter(m, c) -> ColorFilter(m, scene c)
        | BlendMode(b, c)   -> BlendMode(b, scene c)
        | Mask(m, c)        -> Mask(scene m, scene c)
        | Shape _ | Text _ | GlyphRun _ | Image _ | Empty -> s

    and private mkTransform (m: Mat3x2) (c: Scene) =
        if isIdentity m || (match c with Empty -> true | _ -> false) then c else Transform(m, c)

    and private mkOpacity (a: float32) (c: Scene) =
        if a >= 1.0f || (match c with Empty -> true | _ -> false) then c else Opacity(a, c)

    and private group (cs: Scene list) : Scene =
        let flat =
            cs |> List.collect (fun c ->
                match c with
                | Empty -> []
                | Group inner -> inner
                | other -> [ other ])
        match flat with
        | [] -> Empty
        | [ one ] -> one
        | many -> Group many
