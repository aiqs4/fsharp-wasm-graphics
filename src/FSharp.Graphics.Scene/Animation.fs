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
            let b = BezierCubic.create Vec2.zero p1 p2 Vec2.one
            (BezierCubic.evaluate t b).Y
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
                let s = Scalar.clamp 0.0f 1.0f (Scalar.floor (t * n) / (n - 1.0f))
                s
