namespace FSharp.Graphics.Scene

open FSharp.Math

/// Easing function type
type Easing =
    | Linear
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier of p1: Vec2 * p2: Vec2
    | Spring      of stiffness: float32 * damping: float32
    | Steps       of count: int * jump: StepJump

and StepJump = JumpStart | JumpEnd | JumpBoth | JumpNone

/// Animation loop mode
type LoopMode = Once | Repeat of count: int | Infinite | Alternate

/// Animation fill mode
type FillMode = FillNone | FillForwards | FillBackwards | FillBoth

/// Animation transition definition
[<Struct>]
type Transition = {
    DurationMs : float32
    Easing     : Easing
    DelayMs    : float32
    Loop       : LoopMode
    FillMode   : FillMode
}

module Transition =
    let linear ms   = { DurationMs = float32 ms; Easing = Linear;  DelayMs = 0.0f; Loop = Once; FillMode = FillNone }
    let easeOut ms  = { DurationMs = float32 ms; Easing = EaseOut; DelayMs = 0.0f; Loop = Once; FillMode = FillNone }
    let easeIn ms   = { DurationMs = float32 ms; Easing = EaseIn;  DelayMs = 0.0f; Loop = Once; FillMode = FillNone }
    let easeInOut ms = { DurationMs = float32 ms; Easing = EaseInOut; DelayMs = 0.0f; Loop = Once; FillMode = FillNone }
    let spring k d  = { DurationMs = 500.0f; Easing = Spring(k, d); DelayMs = 0.0f; Loop = Once; FillMode = FillForwards }
    let loop t      = { t with Loop = Infinite }
    let delay ms t  = { t with DelayMs = float32 ms }
    let alternate t = { t with Loop = Alternate }
    let fillForwards t = { t with FillMode = FillForwards }

/// Evaluate an easing function at parameter t ∈ [0,1]
module Easing =
    let evaluate (easing: Easing) (t: float32) : float32 =
        let t = Scalar.clamp 0.0f 1.0f t
        match easing with
        | Linear -> t
        | EaseIn -> t * t
        | EaseOut -> t * (2.0f - t)
        | EaseInOut ->
            if t < 0.5f then 2.0f * t * t
            else -1.0f + (4.0f - 2.0f * t) * t
        | CubicBezier(p1, p2) ->
            // CSS semantics: t is the *x* coordinate of the curve, so solve x(u) = t for u
            // (bisection) and return y(u). Evaluating the curve at u = t is wrong for any
            // non-trivial control points.
            let b = BezierCubic.create Vec2.zero p1 p2 Vec2.one
            let mutable lo = 0.0f
            let mutable hi = 1.0f
            for _ in 1 .. 14 do
                let mid = (lo + hi) * 0.5f
                if (BezierCubic.evaluate mid b).X < t then lo <- mid else hi <- mid
            (BezierCubic.evaluate ((lo + hi) * 0.5f) b).Y
        | Spring(stiffness, damping) ->
            let omega = Scalar.sqrt stiffness
            let decay = Scalar.clamp 0.0f 1.0f (damping * t * 0.5f)
            let osc = Scalar.cos (omega * t * Scalar.TwoPi)
            1.0f - (1.0f - t) * osc * (1.0f - decay)
        | Steps(count, jump) ->
            let n = float32 count
            match jump with
            | JumpStart -> Scalar.ceil (t * n) / n
            | JumpEnd   -> Scalar.floor (t * n) / n
            | JumpBoth  ->
                let s = Scalar.clamp 0.0f 1.0f ((Scalar.floor (t * n) + 1.0f) / (n + 1.0f))
                s
            | JumpNone  ->
                let s = Scalar.clamp 0.0f 1.0f (Scalar.floor (t * n) / Scalar.max' 1.0f (n - 1.0f))
                s

/// Evaluates a Transition against a clock. Pure: the same (transition, time) always gives the
/// same progress, so animation state needs no mutable bookkeeping and works in every host.
module Timeline =
    let private frac (x: float32) = x - Scalar.floor x

    /// Eased progress in 0..1 at `timeMs` after the animation was started.
    let progress (tr: Transition) (timeMs: float32) : float32 =
        let t = timeMs - tr.DelayMs
        let d = Scalar.max' tr.DurationMs 0.001f
        if t < 0.0f then
            (match tr.FillMode with FillBackwards | FillBoth -> Easing.evaluate tr.Easing 0.0f | _ -> 0.0f)
        else
            let cycles = t / d
            let raw =
                match tr.Loop with
                | Once          -> Scalar.clamp 0.0f 1.0f cycles
                | Repeat n      -> if cycles >= float32 n then 1.0f else frac cycles
                | Infinite      -> frac cycles
                | Alternate     ->
                    let k = Scalar.floor cycles
                    let f = cycles - k
                    if int k % 2 = 0 then f else 1.0f - f
            Easing.evaluate tr.Easing raw

    /// True once a finite animation has run to completion.
    let isDone (tr: Transition) (timeMs: float32) : bool =
        match tr.Loop with
        | Once     -> timeMs >= tr.DelayMs + tr.DurationMs
        | Repeat n -> timeMs >= tr.DelayMs + tr.DurationMs * float32 n
        | _        -> false
