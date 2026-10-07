namespace FSharp.Graphics.Core

open FSharp.Math

/// A gradient color stop
[<Struct>]
type GradientStop = { Offset: float32; Color: Color }

/// How a shape or region is filled
type Paint =
    | Solid          of Color
    // | LinearGradient of stops: GradientStop array * angleDeg: float32
    // | RadialGradient of stops: GradientStop array * center: Vec2
    | LinearGradient of startPoint: Vec2 * endPoint: Vec2 * stops: GradientStop array
    | RadialGradient of center: Vec2 * radius: float32 * stops: GradientStop array
    | ImageFill      of ImageId * opacity: float32

module Paint =
    let solid c                     = Solid c
    // Vertikal: Von oben (0,0) nach unten (0,1)
    let linearV stops               = LinearGradient({ X = 0.0f; Y = 0.0f }, { X = 0.0f; Y = 1.0f }, stops)
    // Horizontal: Von links (0,0) nach rechts (1,0)
    let linearH stops               = LinearGradient({ X = 0.0f; Y = 0.0f }, { X = 1.0f; Y = 0.0f }, stops)
    let linear startP endP stops    = LinearGradient(startP, endP, stops)

    // THIS is our future implementation
    /// Creates a linear gradient based on an angle (in degrees) within a bounding rectangle
    // let linearAngle (r: Rect) (angleDeg: float32) (stops: GradientStop array) =
    //     // 1. Calculate the center of the rectangle
    //     let cx = r.X + r.W * 0.5f
    //     let cy = r.Y + r.H * 0.5f
        
    //     // 2. Convert angle to radians natively in float32 using a constant downcast
    //     let rad = angleDeg * (float32 System.Math.PI) / 180.0f
        
    //     // 3. Compute direction vector natively via extended float32 trig functions
    //     let dx = System.Math.Cos(rad)
    //     let dy = System.Math.Sin(rad)
        
    //     // 4. Project corner extents using native float32 Abs overloads
    //     let wComp = System.Math.Abs(r.W * dx)
    //     let hComp = System.Math.Abs(r.H * dy)
    //     let halfLen = (wComp + hComp) * 0.5f
        
    //     // 5. Build start and end points symmetrically around the center
    //     let startP = Vec2.create (cx - dx * halfLen) (cy - dy * halfLen)
    //     let endP   = Vec2.create (cx + dx * halfLen) (cy + dy * halfLen)
        
    //     LinearGradient(startP, endP, stops)

    // TODO: Important - for FSharp.WASM we use our own math method. So use the commented above - and move Math.fs into our own BCL!
    /// Creates a linear gradient based on an angle (in degrees) within a bounding rectangle
    let linearAngle (r: Rect) (angleDeg: float32) (stops: GradientStop array) =
        // 1. Calculate the center of the rectangle
        let cx = r.X + r.W * 0.5f
        let cy = r.Y + r.H * 0.5f
        
        // 2. Convert angle to radians
        let rad = angleDeg * 3.14159265f / 180.0f
        
        // 3. Compute direction vector from angle
        let dx = System.MathF.Cos(rad)
        let dy = System.MathF.Sin(rad)
        
        // 4. Project corner extents onto the angle vector to calculate the perfect gradient length
        let wComp = System.MathF.Abs(r.W * dx)
        let hComp = System.MathF.Abs(r.H * dy)
        let halfLen = (wComp + hComp) * 0.5f
        
        // 5. Build start and end points symmetrically around the center
        let startP = Vec2.create (cx - dx * halfLen) (cy - dy * halfLen)
        let endP   = Vec2.create (cx + dx * halfLen) (cy + dy * halfLen)
        
        LinearGradient(startP, endP, stops)

    let radial center radius stops  = RadialGradient(center, radius, stops)
    let image id                    = ImageFill(id, 1.0f)
    let imageWithOpacity id a       = ImageFill(id, a)
