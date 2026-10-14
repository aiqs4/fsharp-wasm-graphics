namespace FSharp.Math

/// RGBA color with 8-bit components
[<Struct>]
type Color = { R: uint8; G: uint8; B: uint8; A: uint8 }

module Color =
    let inline rgb (r: uint8) (g: uint8) (b: uint8) : Color = { R=r; G=g; B=b; A=255uy }
    let inline rgba (r: uint8) (g: uint8) (b: uint8) (a: uint8) : Color = { R=r; G=g; B=b; A=a }
    
    let inline hex (h: uint32) : Color =
        { R = uint8 (h >>> 16); G = uint8 (h >>> 8); B = uint8 h; A = 255uy }
    
    let inline hexA (h: uint32) : Color =
        { R = uint8 (h >>> 24); G = uint8 (h >>> 16); B = uint8 (h >>> 8); A = uint8 h }
    
    // Named constants
    let Black       = rgb 0uy 0uy 0uy
    let White       = rgb 255uy 255uy 255uy
    let Transparent = rgba 0uy 0uy 0uy 0uy
    let Red         = rgb 255uy 0uy 0uy
    let Green       = rgb 0uy 128uy 0uy
    let Blue        = rgb 0uy 0uy 255uy
    let Yellow      = rgb 255uy 255uy 0uy
    let Cyan        = rgb 0uy 255uy 255uy
    let Magenta     = rgb 255uy 0uy 255uy
    let Gray        = rgb 128uy 128uy 128uy
    
    /// sRGB → linear (gamma decode)
    let inline srgbToLinear (c: uint8) : float32 =
        let s = float32 c / 255.0f
        if s <= 0.04045f then s / 12.92f
        else
            let x = (s + 0.055f) / 1.055f
            x * x * (0.8f * x + 0.2f)
    
    /// linear → sRGB (gamma encode)
    let inline linearToSrgb (l: float32) : uint8 =
        let s =
            if l <= 0.0031308f then l * 12.92f
            else 1.055f * Scalar.sqrt (Scalar.sqrt l * Scalar.sqrt (Scalar.sqrt l)) - 0.055f
        uint8 (Scalar.clamp 0.0f 255.0f (s * 255.0f + 0.5f))
    
    /// Convert to linear float RGBA
    let inline toLinear (c: Color) : Vec4 =
        Vec4.create (srgbToLinear c.R) (srgbToLinear c.G) (srgbToLinear c.B) (float32 c.A / 255.0f)
    
    /// Convert from linear float RGBA to sRGB
    let inline fromLinear (v: Vec4) : Color =
        { R = linearToSrgb v.X; G = linearToSrgb v.Y; B = linearToSrgb v.Z
          A = uint8 (Scalar.clamp 0.0f 255.0f (v.W * 255.0f + 0.5f)) }
    
    /// Premultiplied alpha blending (Porter-Duff over)
    let inline blend (bg: Color) (fg: Color) : Color =
        let fa = float32 fg.A / 255.0f
        let ba = float32 bg.A / 255.0f
        let outA = fa + ba * (1.0f - fa)
        if outA < Scalar.Eps then Transparent
        else
            let fr = float32 fg.R * fa
            let fg' = float32 fg.G * fa
            let fb = float32 fg.B * fa
            let br = float32 bg.R * ba * (1.0f - fa)
            let bg' = float32 bg.G * ba * (1.0f - fa)
            let bb = float32 bg.B * ba * (1.0f - fa)
            { R = uint8 (Scalar.clamp 0.0f 255.0f ((fr + br) / outA + 0.5f))
              G = uint8 (Scalar.clamp 0.0f 255.0f ((fg' + bg') / outA + 0.5f))
              B = uint8 (Scalar.clamp 0.0f 255.0f ((fb + bb) / outA + 0.5f))
              A = uint8 (Scalar.clamp 0.0f 255.0f (outA * 255.0f + 0.5f)) }
    
    /// Lerp between two colors (in sRGB space — fast but not perceptually linear)
    let inline lerp (a: Color) (b: Color) (t: float32) : Color =
        let t' = Scalar.clamp 0.0f 1.0f t
        { R = uint8 (Scalar.lerp (float32 a.R) (float32 b.R) t' + 0.5f)
          G = uint8 (Scalar.lerp (float32 a.G) (float32 b.G) t' + 0.5f)
          B = uint8 (Scalar.lerp (float32 a.B) (float32 b.B) t' + 0.5f)
          A = uint8 (Scalar.lerp (float32 a.A) (float32 b.A) t' + 0.5f) }
    
    /// Lerp in linear space (perceptually correct for gradients)
    let lerpLinear (a: Color) (b: Color) (t: float32) : Color =
        let la = toLinear a
        let lb = toLinear b
        fromLinear (Vec4.lerp la lb t)
    
    /// Convert to packed uint32 (ARGB)
    let inline toArgb (c: Color) : uint32 =
        (uint32 c.A <<< 24) ||| (uint32 c.R <<< 16) ||| (uint32 c.G <<< 8) ||| uint32 c.B
    
    /// Convert from packed uint32 (ARGB)
    let inline fromArgb (argb: uint32) : Color =
        { A = uint8 (argb >>> 24); R = uint8 (argb >>> 16); G = uint8 (argb >>> 8); B = uint8 argb }
    
    /// Multiply alpha (apply opacity)
    let inline withAlpha (alpha: float32) (c: Color) : Color =
        { c with A = uint8 (float32 c.A * Scalar.clamp 0.0f 1.0f alpha + 0.5f) }
