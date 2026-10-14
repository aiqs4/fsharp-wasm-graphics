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

/// 006: Group of colored rows
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

/// 007: Stroked circle
let scene007 () =
    Shape(Circle {
        Center  = Vec2.create 200.0f 150.0f
        Radius  = 80.0f
        Fill    = Solid(Color.rgb 240uy 240uy 240uy)
        Stroke  = Some (Stroke.solid (Color.rgb 30uy 30uy 180uy) 4.0f)
        Shadow  = None
    })

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

let allScenes = [
    "001_rect_solid",      scene001
    "002_rounded_rect",    scene002
    "003_circle_shadow",   scene003
    "004_linear_gradient", scene004
    "005_radial_gradient", scene005
    "006_stack_vertical",  scene006
    "007_stroked_circle",  scene007
    "008_zstack_opacity",  scene008
    "009_clip",            scene009
    "010_transform",       scene010
]
