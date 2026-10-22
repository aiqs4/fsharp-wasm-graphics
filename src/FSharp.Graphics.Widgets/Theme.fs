namespace FSharp.Graphics.Widgets

open FSharp.Math
open FSharp.Graphics.Core
open FSharp.Graphics.SceneDsl

/// Design tokens. Widgets read everything visual from here, so re-theming is one record.
type Theme =
    { Font: FontId
      Background: Color
      Surface: Color
      SurfaceRaised: Color
      Border: Color
      Text: Color
      TextMuted: Color
      Accent: Color
      AccentHover: Color
      AccentPressed: Color
      OnAccent: Color
      Danger: Color
      Success: Color
      Radius: float32
      ControlHeight: float32
      PadX: float32
      Gap: float32
      FontSize: float32 }

module Theme =
    let dark : Theme =
        { Font = FontId 0UL
          Background = hex 0x0F172A; Surface = hex 0x1E293B; SurfaceRaised = hex 0x273449
          Border = hex 0x3B4A63; Text = hex 0xF1F5F9; TextMuted = hex 0x94A3B8
          Accent = hex 0x3B82F6; AccentHover = hex 0x60A5FA; AccentPressed = hex 0x2563EB; OnAccent = hex 0xFFFFFF
          Danger = hex 0xEF4444; Success = hex 0x22C55E
          Radius = 8.0f; ControlHeight = 34.0f; PadX = 14.0f; Gap = 8.0f; FontSize = 14.0f }

    let light : Theme =
        { dark with
            Background = hex 0xF1F5F9; Surface = hex 0xFFFFFF; SurfaceRaised = hex 0xE2E8F0
            Border = hex 0xCBD5E1; Text = hex 0x0F172A; TextMuted = hex 0x64748B }
