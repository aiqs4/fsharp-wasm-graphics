module FSharp.Graphics.GoldenTests.GoldenScenes

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.Primitives
open FSharp.Graphics.Scene

/// 001: Solid-color rectangle
let scene001 () =
    Shape(Rectangle {
        Bounds  = Rect.create 50.0f 50.0f 300.0f 200.0f
        Radius  = CornerRadius.zero
        Fill    = Solid(Color.rgb 66uy 135uy 245uy)
        Stroke  = None
        Shadow  = None
    })

/// 002: Rounded rectangle
let scene002 () =
    Shape(Rectangle {
        Bounds  = Rect.create 50.0f 50.0f 300.0f 200.0f
        Radius  = CornerRadius.all 20.0f
        Fill    = Solid(Color.rgb 245uy 66uy 66uy)
        Stroke  = None
        Shadow  = None
    })

/// 003: Circle with drop shadow
let scene003 () =
    Shape(Circle {
        Center  = Vec2.create 200.0f 150.0f
        Radius  = 80.0f
        Fill    = Solid(Color.rgb 66uy 245uy 135uy)
        Stroke  = None
        Shadow  = Some (Shadow.drop (Vec2.create 4.0f 6.0f) 12.0f (Color.rgba 0uy 0uy 0uy 100uy))
    })

/// 004: Linear gradient rect
let scene004 () =
    Shape(Rectangle {
        Bounds  = Rect.create 50.0f 50.0f 300.0f 200.0f
        Radius  = CornerRadius.zero
        Fill    = LinearGradient(
            [| { Offset = 0.0f; Color = Color.rgb 255uy 100uy 0uy }
               { Offset = 1.0f; Color = Color.rgb 100uy 0uy 255uy } |],
            90.0f)
        Stroke  = None
        Shadow  = None
    })

/// 005: Radial gradient circle
let scene005 () =
    Shape(Circle {
        Center  = Vec2.create 200.0f 150.0f
        Radius  = 100.0f
        Fill    = RadialGradient(
            [| { Offset = 0.0f; Color = Color.rgb 255uy 255uy 100uy }
               { Offset = 1.0f; Color = Color.rgb 255uy 50uy 50uy } |],
            Vec2.create 200.0f 150.0f)
        Stroke  = None
        Shadow  = None
    })

/// 006: Group of colored rows (stack vertical)
let scene006 () =
    let makeRow color y =
        Shape(Rectangle {
            Bounds  = Rect.create 50.0f y 300.0f 60.0f
            Radius  = CornerRadius.all 8.0f
            Fill    = Solid color
            Stroke  = None
            Shadow  = None
        })
    Group [
        makeRow (Color.rgb 70uy 130uy 180uy) 20.0f
        makeRow (Color.rgb 60uy 179uy 113uy) 90.0f
        makeRow (Color.rgb 255uy 165uy 0uy)  160.0f
        makeRow (Color.rgb 220uy 20uy 60uy)  230.0f
    ]

/// 007: Nested Stack Horizontal (4 colored columns side by side)
let scene007 () =
    let makeCol color x =
        Shape(Rectangle {
            Bounds  = Rect.create x 50.0f 70.0f 200.0f
            Radius  = CornerRadius.all 6.0f
            Fill    = Solid color
            Stroke  = None
            Shadow  = None
        })
    Group [
        makeCol (Color.rgb 70uy 130uy 180uy) 20.0f
        makeCol (Color.rgb 60uy 179uy 113uy) 100.0f
        makeCol (Color.rgb 255uy 165uy 0uy)  180.0f
        makeCol (Color.rgb 220uy 20uy 60uy)  260.0f
    ]

/// 008: ZStack with overlapping semi-transparent rects
let scene008 () =
    Group [
        Shape(Rectangle {
            Bounds = Rect.create 50.0f 50.0f 200.0f 200.0f
            Radius = CornerRadius.zero
            Fill   = Solid(Color.rgb 255uy 0uy 0uy)
            Stroke = None; Shadow = None
        })
        Shape(Rectangle {
            Bounds = Rect.create 150.0f 100.0f 200.0f 200.0f
            Radius = CornerRadius.zero
            Fill   = Solid(Color.rgba 0uy 0uy 255uy 160uy)
            Stroke = None; Shadow = None
        })
    ]

/// 009: Clip region
let scene009 () =
    Clip(
        Rect.create 100.0f 100.0f 200.0f 100.0f,
        Shape(Circle {
            Center = Vec2.create 200.0f 150.0f
            Radius = 100.0f
            Fill   = Solid(Color.rgb 255uy 200uy 0uy)
            Stroke = None; Shadow = None
        })
    )

/// 010: Affine transform (translation)
let scene010 () =
    Transform(
        Mat3x2.translation 100.0f 100.0f,
        Shape(Rectangle {
            Bounds = Rect.create 0.0f 0.0f 150.0f 100.0f
            Radius = CornerRadius.all 12.0f
            Fill   = Solid(Color.rgb 180uy 100uy 220uy)
            Stroke = None; Shadow = None
        })
    )

/// 011: Text + shape composition (M2 — GlyphAura integration)
let scene011 () =
    Group [
        // Background card
        Shape(Rectangle {
            Bounds  = Rect.create 40.0f 40.0f 320.0f 180.0f
            Radius  = CornerRadius.all 14.0f
            Fill    = Solid(Color.rgb 245uy 245uy 250uy)
            Stroke  = Some (Stroke.solid (Color.rgb 180uy 180uy 200uy) 1.5f)
            Shadow  = Some (Shadow.drop (Vec2.create 2.0f 4.0f) 8.0f (Color.rgba 0uy 0uy 0uy 60uy))
        })
        // Title text
        Text(
            "Hello, GlyphAura!",
            { FontId = FontId 1UL; SizePx = 22.0f; Color = Color.rgb 30uy 30uy 80uy
              Weight = Bold; Align = Left; Wrap = false
              Overflow = TextOverflow.Clip
              LetterSpacing = 0.0f; LineHeight = 1.2f }
        )
        // Subtitle text
        Text(
            "OpenType shaping in F#",
            { FontId = FontId 1UL; SizePx = 14.0f; Color = Color.rgb 80uy 80uy 120uy
              Weight = Regular; Align = Left; Wrap = false
              Overflow = TextOverflow.Clip
              LetterSpacing = 0.0f; LineHeight = 1.2f }
        )
    ]

/// 012: Inner shadow on rect
let scene012 () =
    Shape(Rectangle {
        Bounds  = Rect.create 80.0f 80.0f 240.0f 140.0f
        Radius  = CornerRadius.all 16.0f
        Fill    = Solid(Color.rgb 240uy 240uy 240uy)
        Stroke  = None
        Shadow  = Some { Offset = Vec2.create 3.0f 4.0f; Blur = 8.0f; Spread = 0.0f
                         Color = Color.rgba 0uy 0uy 0uy 120uy; Inner = true }
    })

/// 013: Image fill (placeholder — rendered as gray since no image loading)
let scene013 () =
    Shape(Rectangle {
        Bounds  = Rect.create 80.0f 80.0f 240.0f 140.0f
        Radius  = CornerRadius.all 8.0f
        Fill    = ImageFill(ImageId 42u, 1.0f)
        Stroke  = None
        Shadow  = None
    })

/// 014: Stroked circle
let scene014 () =
    Shape(Circle {
        Center  = Vec2.create 200.0f 150.0f
        Radius  = 80.0f
        Fill    = Solid(Color.rgb 240uy 240uy 240uy)
        Stroke  = Some (Stroke.solid (Color.rgb 30uy 30uy 180uy) 4.0f)
        Shadow  = None
    })

/// 015: Complex nested scene (4 cards)
let scene015 () =
    let card x y w h color =
        Shape(Rectangle {
            Bounds  = Rect.create x y w h
            Radius  = CornerRadius.all 12.0f
            Fill    = Solid color
            Stroke  = Some (Stroke.solid (Color.rgba 0uy 0uy 0uy 40uy) 1.5f)
            Shadow  = Some (Shadow.drop (Vec2.create 2.0f 3.0f) 6.0f (Color.rgba 0uy 0uy 0uy 80uy))
        })
    Group [
        card 20.0f  20.0f  170.0f 120.0f (Color.rgb 100uy 149uy 237uy)
        card 210.0f 20.0f  170.0f 120.0f (Color.rgb 144uy 238uy 144uy)
        card 20.0f  160.0f 170.0f 120.0f (Color.rgb 255uy 165uy 0uy)
        card 210.0f 160.0f 170.0f 120.0f (Color.rgb 255uy 99uy 71uy)
    ]

/// 016: Animation easing at fixed timestamp (Animated wrapper passthrough)
let scene016 () =
    Animated(
        { DurationMs = 500.0f; Easing = EaseInOut; DelayMs = 0.0f
          Loop = Once; FillMode = FillNone },
        Shape(Rectangle {
            Bounds = Rect.create 50.0f 100.0f 200.0f 100.0f
            Radius = CornerRadius.all 8.0f
            Fill   = Solid(Color.rgb 90uy 90uy 200uy)
            Stroke = None; Shadow = None
        })
    )

/// 017: Gaussian blur (rasterizer renders child unchanged — blur not yet implemented)
let scene017 () =
    Blur(
        12.0f,
        Shape(Circle {
            Center = Vec2.create 200.0f 150.0f
            Radius = 80.0f
            Fill   = Solid(Color.rgb 255uy 100uy 50uy)
            Stroke = None; Shadow = None
        })
    )

/// 018: Color filter — grayscale (rasterizer renders child unchanged — filter not yet implemented)
let scene018 () =
    ColorFilter(
        [| 0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f
           0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f
           0.2126f; 0.7152f; 0.0722f; 0.0f; 0.0f
           0.0f;    0.0f;    0.0f;    1.0f; 0.0f |],
        Shape(Rectangle {
            Bounds = Rect.create 80.0f 80.0f 240.0f 140.0f
            Radius = CornerRadius.zero
            Fill   = LinearGradient(
                [| { Offset = 0.0f; Color = Color.rgb 255uy 100uy 0uy }
                   { Offset = 1.0f; Color = Color.rgb 0uy 100uy 255uy } |],
                0.0f)
            Stroke = None; Shadow = None
        })
    )

let allScenes = [
    "001_rect_solid",          scene001
    "002_rounded_rect",        scene002
    "003_circle_shadow",       scene003
    "004_linear_gradient",     scene004
    "005_radial_gradient",     scene005
    "006_stack_vertical",      scene006
    "007_stack_horizontal",    scene007
    "008_zstack_opacity",      scene008
    "009_clip",                scene009
    "010_transform_translate", scene010
    "011_text_shape",          scene011
    "012_inner_shadow",        scene012
    "013_image_fill",          scene013
    "014_stroked_circle",      scene014
    "015_complex_scene",       scene015
    "016_animation_wrapper",   scene016
    "017_blur",                scene017
    "018_color_filter",        scene018
]
