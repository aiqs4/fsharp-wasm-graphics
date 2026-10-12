# FSharp.Graphics: Architektur, Backend-Strategie, Design-Review

## 1. Entscheidung: Skia ja, aber nur als *optionales Backend*

Die Bibliothek wird nach WasmGC kompiliert (kein Span, keine Exceptions, minimale BCL). Skia läuft dort nicht
und darf deshalb **nie** Teil der Bibliothek sein. Stabiler Vertrag ist der binäre **Befehlsstrom**
(`Render/Protocol.fs`, Version 2). Jedes Backend ist nur eine Implementierung von `IRenderSink`:

```
Model --View--> Scene --Resolve--> positionierte Scene --Diff/Cache--> DrawList --Encode--> Bytes (Protokoll v2)
                (Layout, Text -> GlyphRun)                                     |
                                      CommandReader (1 Decoder, bounds-checked) |
        +--------------------+-----------------------+---------------------------+
   SoftwareRenderer      SkiaSink                 (später) Canvas2D / WebGPU
   reine F#-Referenz     GPU, nur Desktop-Dev     Browser / gemeinsamer WGSL-SDF-Evaluator
   = Ground Truth        austauschbar
```

* **Software-Backend** (`Host/Raster.fs`) ist die Wahrheit: läuft überall, ist deterministisch, Golden-Tests laufen darauf.
* **Skia** (`Host.Skia`) ist Beschleuniger/Vergleichsorakel. Löschbar, sobald ein eigenes GPU-Backend existiert.
* **Konformität:** Im Runner schaltet `B` live zwischen Skia und Software um (gleicher Byte-Strom, Unterschiede = Bugs),
  `F12` speichert einen Software-Screenshot. Später: Pixel-Diff mit Toleranz in CI.
* **Neue Ziele** (Canvas2D, WebGPU, Vulkan) brauchen nur einen weiteren Sink. Scene, Layout, Diff, Protokoll bleiben unberührt.

## 2. Was in dieser Runde behoben wurde (Ursachen)

| Symptom | Ursache | Fix |
|---|---|---|
| Hunderte `unknown opcode 0x0` | Decoder implementierte ein *erfundenes* Protokoll (BeginFrame mit Farbe, andere Opcodes, absolute Transforms) -> Desync -> Null-Padding | Ein Decoder (`CommandReader`) auf Basis **derselben** Konstanten wie der Encoder (`Protocol.fs`); kaputte Streams -> `Malformed(offset, msg)` statt Rauschen |
| Unscharfe / halbtransparente Shapes, Stroke nur an den Ecken, achteckige Schatten | `RectSdf` (falscher Innenterm `min(qx,0)+min(qy,0)`), `EllipseSdf` (Gradient != 1) | exakte SDFs; `\|grad\|≈1` getestet |
| Verschobene Shapes, Balken im Ursprung gestapelt | Rasterizer ignorierte Transform; Stack-Layout wurde nie angewandt | `Resolve.resolve` macht Layout zu Translationen, Text zu `GlyphRun`; Rasterizer hat echten Transform-/Clip-Stack |
| Protokoll verlustbehaftet | `DrawStroke` ohne Shape, `DrawPath` nur Cache-Key, Shadow nur Bounds, `SetOpacity` setzt auf 1.0 zurück | Geometrie reist im Strom (`ShapeGeometry`), Layer (`PushLayer/PopLayer`) für Opacity/Blur/Blend/Farbmatrix |
| Inner-Shadow unsichtbar | wurde *vor* der Füllung gemalt | Reihenfolge: Drop-Shadow -> Fill -> Inner-Shadow -> Stroke |
| 100 Compilerfehler im Monolith | falscher Pfad `../src/FSharp.Math` + halbfertige Vec2-Migration | Monolith auf letzten funktionierenden Stand; er ist Legacy |

## 3. Starten

```bash
dotnet run --project demo/FSharp.Graphics.Demo -- --out out          # alle Szenen -> out/*.png + index.html
dotnet run --project demo/FSharp.Graphics.Demo -- --scene layout     # eine Szene
dotnet run --project tests/FSharp.Graphics.HostTests                 # Regressionstests (Exit-Code 1 bei Fehler)
dotnet run --project demo/FSharp.Graphics.DesktopRunner              # Fenster. <-/-> Szene, B Backend, F12 Screenshot
```

## 4. Design-Review (nach Priorität)

**Hoch**
1. **Zwei Codebasen.** Monolith `FSharp.Graphics` und die gesplitteten Projekte driften. Port der 118 Property-Tests auf `Core/Primitives/Scene`, dann Monolith löschen.
2. **`ISdfShape.CacheKey : int`** ist ein 32-Bit-Hash. Kollisionen lassen `Diff` zwei *verschiedene* Shapes als gleich behandeln (stale Rendering). Besser: strukturelle Gleichheit über `Geometry` (existiert jetzt).
3. **Gradients:** Die Einheits-Variante `Paint.linearH stops` (0,0)->(1,0) ergibt Verläufe über 1 Pixel (praktisch einfarbig), weil Geometrie im lokalen Raum liegt. Helfer nehmen daher ein `Rect`. Wer Bounding-Box-Einheiten (SVG `objectBoundingBox`) will, muss das als eigenes Paint-Fall im Protokoll abbilden.
4. **Kein echtes Retained-Rendering.** `CachedNode`-Dirty-Flags bringen nichts, solange jedes Frame den ganzen Baum neu encodiert. Gewinn: `Diff = NoChange` -> Frame überspringen; Damage-Rects; Draw-List-Caching pro Subtree.
5. **Handler-Identität = Baumpfad.** Umsortierte Listen verlieren Hover-/Press-State. Braucht optionale Schlüssel (`Scene.key "id"`), wie React/Elm-Keys.

**Mittel**
6. `FSharp.Math.Vec2` hat jetzt *zwei* APIs (Modul-Funktionen **und** statische Member/Operatoren, inline `sqrt`). Eine wählen. Für WasmGC/Fable sind Modul-Funktionen der sichere Weg, und dein eigener TODO sagt ohnehin "kein OOP-Stil".
7. `Shadow.simple` hat still die Signatur gewechselt (`blur color` -> `ox oy blur color`), `Shadow.Simple` ist ein *curried* Alias; Aufrufe mit Tupel schlagen fehl. Breaking Change sichtbar machen oder Alias entfernen.
8. `Mat3x2.invert` nutzt ein **absolutes** Epsilon auf der Determinante: Skalierungen < ~1e-3 gelten als singulär.
9. **Clip nur als Rechteck** (kein Rounded/Path-Clip) und `Mask` ist nirgends implementiert (wird ohne Maske gezeichnet). Für Scroll-Views mit runden Ecken nötig.
10. `Blur` erweitert die Bounds nicht (Ränder können abgeschnitten wirken, wenn ein Parent clippt).
11. **Easing:** `Spring` ist keine Feder (Formel, keine Physik); `CubicBezier` wertete die Kurve bei `u = t` aus statt x(u)=t zu lösen (**behoben**); `Steps` dividierte durch 0 für n=1 (**behoben**).
12. `TextStyle` hat keine `MaxWidth`; aktuell definiert ein `Clip`-Bereich die Textbox (Notlösung, funktioniert, ist aber implizit).
13. Decoder allokiert pro Befehl (DU + Arrays). Für Dev ok, für Hot-Path später struct-basiert/Callback-basiert.

**Niedrig**
14. Golden-Tests haben eine **eigene** Rasterizer-/PNG-Kopie. Auf `Host.SoftwareRenderer` umstellen (DRY) und Golden-Masters **neu erzeugen**: sie wurden mit den falschen SDFs aufgenommen.
15. Software-AA nimmt gleichmäßige Skalierung an (`sqrt|det|`); bei stark ungleichmäßiger Skalierung ist die Kantenbreite nur genähert.
16. Schatten: analytische erf-Näherung der SDF vs. echter Gauss in Skia. Bei sehr dünnen Shapes weichen sie ab. Konformitäts-Toleranz definieren.
17. Strokes kennen Caps/Joins, aber keine Dashes; Stroke auf `GLine` ist genähert.

## 5. "Besser als Clutter": Fahrplan

Clutter (Actor-Baum, Stage, Timelines, Constraints, Actions, Effects) war ein *mutabler* Objektgraph. Vorteil unseres Ansatzes: ein
**reiner, diffbarer Baum**, ein **Byte-Protokoll** als Grenze und **mehrere Backends**. Bereits vorhanden: `Stage`
(~ClutterStage), Hit-Test + Hover + Capture + Bubbling, Layer-Effekte (Blur/Blend/Farbmatrix), `Timeline.progress`.

1. **Animation:** `Animated`-Knoten in `Resolve` auswerten (Zeit als Parameter), implizites `Scene.animate prop transition`. Basis steht (`Timeline`).
2. **Layout:** Grid/Flow/Bin, echte `FillParent`/`Fixed`-Größen (aktuell ignoriert), Min/Natural/Max-Verhandlung, `Stretch` wirklich dehnen.
3. **Constraints** (Clutter-Idee): `align`, `anchor`, `snap` als Knoten, aufgelöst in `Resolve`.
4. **Events:** Capture-Phase, Gesten (Tap, Drag, Swipe) als Recognizer über dem `Dispatcher`, Fokus + Tastaturnavigation.
5. **Barrierefreiheit:** Accessibility-Baum aus der aufgelösten Scene exportieren (Clutter hatte das nie; Alleinstellungsmerkmal).
6. **Effekte:** Custom-Effekte als Shader-Hook (WGSL) im Layer-Parameter.
7. **GPU-Backend:** ein WGSL-SDF-Evaluator für Browser *und* Desktop (Silk.NET.WebGPU), gegen das Software-Backend konformitäts-getestet.
8. **Text:** Rich-Text-Runs, Fallback-Fonts, Gewichte, `MaxWidth`, Hit-Testing auf Glyphen.
