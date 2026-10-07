namespace FSharp.Graphics.Scene

open FSharp.Math
open FSharp.Graphics.Primitives

/// Box constraints for layout
[<Struct>]
type BoxConstraints = {
    MinWidth  : float32
    MaxWidth  : float32
    MinHeight : float32
    MaxHeight : float32
}

module BoxConstraints =
    let unconstrained = { MinWidth = 0.0f; MaxWidth = System.Single.MaxValue; MinHeight = 0.0f; MaxHeight = System.Single.MaxValue }
    let tight (w: float32) (h: float32) = { MinWidth = w; MaxWidth = w; MinHeight = h; MaxHeight = h }
    let fromRect (r: Rect) = { MinWidth = 0.0f; MaxWidth = r.W; MinHeight = 0.0f; MaxHeight = r.H }

/// Measured size after layout
[<Struct>]
type MeasuredSize = { Width: float32; Height: float32 }

/// Layout engine — pure, single-pass constraint layout
module LayoutEngine =

    /// Measure a scene node given box constraints
    let rec measure (constraints: BoxConstraints) (scene: Scene) : MeasuredSize =
        match scene with
        | Empty -> { Width = 0.0f; Height = 0.0f }

        | Shape shape ->
            let b = Shapes.bounds shape
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth b.W
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight b.H }

        | GlyphRun(_, _, _, _, b) ->
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth b.W
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight b.H }

        | Text(_, style) ->
            let approxWidth = Scalar.min' (style.SizePx * 10.0f) constraints.MaxWidth
            let approxHeight = style.SizePx * style.LineHeight
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth approxWidth
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight approxHeight }

        | Image(_, bounds, _) ->
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth bounds.W
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight bounds.H }

        | Group children | Stack({ Direction = ZStack }, children) ->
            let mutable maxW = 0.0f
            let mutable maxH = 0.0f
            for child in children do
                let sz = measure constraints child
                if sz.Width > maxW then maxW <- sz.Width
                if sz.Height > maxH then maxH <- sz.Height
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth maxW
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight maxH }

        | Stack(props, children) ->
            measureStack props constraints children

        | Padding(insets, child) ->
            let innerC = {
                MinWidth  = Scalar.max' 0.0f (constraints.MinWidth - insets.Left - insets.Right)
                MaxWidth  = Scalar.max' 0.0f (constraints.MaxWidth - insets.Left - insets.Right)
                MinHeight = Scalar.max' 0.0f (constraints.MinHeight - insets.Top - insets.Bottom)
                MaxHeight = Scalar.max' 0.0f (constraints.MaxHeight - insets.Top - insets.Bottom)
            }
            let sz = measure innerC child
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth (sz.Width + insets.Left + insets.Right)
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight (sz.Height + insets.Top + insets.Bottom) }

        | Clip(region, child) ->
            let clippedC = BoxConstraints.tight (Scalar.min' constraints.MaxWidth region.W) (Scalar.min' constraints.MaxHeight region.H)
            measure clippedC child

        | Transform(_, child) | Opacity(_, child) | Reactive(_, child)
        | Animated(_, child) | Blur(_, child) | ColorFilter(_, child)
        | BlendMode(_, child) ->
            measure constraints child

        | Mask(_, child) ->
            measure constraints child

    and measureStack (props: StackProps) (constraints: BoxConstraints) (children: Scene list) : MeasuredSize =
        let isHoriz = props.Direction = Horizontal
        let totalGap = if children.Length > 1 then props.Gap * float32 (children.Length - 1) else 0.0f

        let mutable mainTotal = 0.0f
        let mutable crossMax = 0.0f

        for child in children do
            let childC =
                if isHoriz then
                    { constraints with MaxWidth = constraints.MaxWidth - mainTotal - totalGap }
                else
                    { constraints with MaxHeight = constraints.MaxHeight - mainTotal - totalGap }
            let sz = measure childC child
            if isHoriz then
                mainTotal <- mainTotal + sz.Width
                crossMax <- Scalar.max' crossMax sz.Height
            else
                mainTotal <- mainTotal + sz.Height
                crossMax <- Scalar.max' crossMax sz.Width

        mainTotal <- mainTotal + totalGap

        if isHoriz then
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth mainTotal
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight crossMax }
        else
            { Width = Scalar.clamp constraints.MinWidth constraints.MaxWidth crossMax
              Height = Scalar.clamp constraints.MinHeight constraints.MaxHeight mainTotal }

    /// Layout: assign absolute positions to all nodes.
    /// Returns list of (Scene, Rect) pairs for rendering.
    let rec layout (bounds: Rect) (scene: Scene) : (Scene * Rect) list =
        match scene with
        | Empty -> []

        | Shape _ | Text _ | GlyphRun _ | Image _ -> [(scene, bounds)]

        | Group children ->
            children |> List.collect (layout bounds)

        | Stack(props, children) ->
            layoutStack props bounds children

        | Padding(insets, child) ->
            let inner = Rect.create
                            (bounds.X + insets.Left) (bounds.Y + insets.Top)
                            (Scalar.max' 0.0f (bounds.W - insets.Left - insets.Right))
                            (Scalar.max' 0.0f (bounds.H - insets.Top - insets.Bottom))
            layout inner child

        | Clip(region, child) ->
            let clipped = Rect.intersect bounds region
            layout clipped child

        | Transform(_, child) | Opacity(_, child) | Reactive(_, child)
        | Animated(_, child) | Blur(_, child) | ColorFilter(_, child)
        | BlendMode(_, child) ->
            [(scene, bounds)]

        | Mask(_, child) ->
            [(scene, bounds)]

    and layoutStack (props: StackProps) (bounds: Rect) (children: Scene list) : (Scene * Rect) list =
        match props.Direction with
        | ZStack ->
            children |> List.collect (layout bounds)
        | dir ->
            let isHoriz = dir = Horizontal
            let constraints = BoxConstraints.fromRect bounds

            let mutable pos = if isHoriz then bounds.X else bounds.Y
            let results = ResizeArray<Scene * Rect>()

            for child in children do
                let sz = measure constraints child
                let childRect =
                    if isHoriz then
                        let y =
                            match props.Alignment with
                            | Start | Stretch -> bounds.Y
                            | Center -> bounds.Y + (bounds.H - sz.Height) * 0.5f
                            | End_ -> bounds.Y + bounds.H - sz.Height
                        Rect.create pos y sz.Width (if props.Alignment = Stretch then bounds.H else sz.Height)
                    else
                        let x =
                            match props.Alignment with
                            | Start | Stretch -> bounds.X
                            | Center -> bounds.X + (bounds.W - sz.Width) * 0.5f
                            | End_ -> bounds.X + bounds.W - sz.Width
                        Rect.create x pos (if props.Alignment = Stretch then bounds.W else sz.Width) sz.Height

                results.AddRange(layout childRect child)
                pos <- pos + (if isHoriz then sz.Width else sz.Height) + props.Gap

            Seq.toList results

    /// Convenience: layout a scene within a viewport
    let layoutScene (viewport: Rect) (scene: Scene) : (Scene * Rect) list =
        layout viewport scene
