# harmonia

**Functional-harmony recipes and voice leading for PureScript.**

Describe a chord by what it *means* — a scale degree, a quality, some tensions,
maybe a borrowed mode — and `harmonia` resolves it against a key into pitch
classes, lifts it to concrete MIDI, and threads a whole progression together by
smallest motion. Two small pure modules, no `Effect`, no FFI, dependencies
limited to `prelude` / `arrays` / `foldable-traversable` / `maybe` / `tuples` —
so the same code compiles unchanged under the JavaScript backend **and**
[purerl](https://github.com/purerl/purescript) (Erlang).

It is the reusable core extracted from a live-coding music stack, where it had
been vendored verbatim across four projects.

```
DegreeChord ──realize──▶ Chord ──closeVoicing──▶ Voicing ──voiceLead──▶ Voicing ──▶ …
 (a recipe)            (pitch-class set)        (sorted MIDI)         (least motion)
   what it means          which notes            where they sit       how they move
```

## Install

```
spago install harmonia
```

## Quick start

```purescript
import Harmonia.Chord (Numeral(..), Quality(..), cMajorKey, deg, realize)
import Harmonia.Voicing (closeVoicing, play)

-- A ii–V–I, as recipes:
cadence = [ deg II Min7 [], deg V Dom7 [], deg I Maj7 [] ]

-- Resolve one chord to its pitch-class set (0..11):
realize cMajorKey (deg V Dom7 [])         -- Chord [2, 5, 7, 11]   (G B D F)

-- Voice-lead the whole progression from a close voicing:
play cMajorKey identity cadence            -- Array Voicing, each near the last
```

Realizing each chord, then taking a close voicing:

```
chord   pitch classes   close voicing
ii7     C D F A         C4 D4 F4 A4
V7      D F G B         D4 F4 G4 B4
Imaj7   C E G B         C4 E4 G4 B4
```

## Voice leading is the point

`play` (and the `voiceLead` underneath it) move from each chord to the next by
the *least total semitone motion* — common tones stay on the same MIDI note,
everything else takes the nearest octave. The same ii–V–I, voice-led:

```
chord   voicing (common tones stay put)
ii7     C4 D4 F4 A4
V7      B3 D4 F4 G4      ← F4 held; A→G, C→B, D stays
Imaj7   B3 C4 E4 G4      ← B3 and G4 held
```

`enumerateVoicings` exposes the whole ranked field (every candidate paired with
its motion score, smallest first), not just the winner — useful when you want to
*offer* voice-leading choices rather than pick one.

## The recipe vocabulary — `Harmonia.Chord`

A `DegreeChord` is scale-degree-relative, so a progression transposes by
changing one `Key`:

| Piece | What it is |
|---|---|
| `Numeral` | `I`…`VII` — scale-degree position |
| `Quality` | `Maj` `Min` `Dim` `Aug` + sevenths (`Maj7` `Min7` `Dom7` `HalfDim` `FullyDim` `MinMaj7`) |
| `Tension` | `Add` / `Sharp` / `Flat` / `Sus` / `NoFifth` / `NoThird` — absolute-interval semantics |
| `Mode` | the church modes, harmonic & melodic minor and *their* modes, plus `Custom [Int]` |
| `DegreeChord` | a recipe — built with `deg`, refined with `slashed` and `borrow` |
| `realize` | `Key -> DegreeChord -> Chord` — the one reduction to a pitch-class set |

```purescript
-- "VII major borrowed from Aeolian" — modal interchange, root drops B→Bb:
borrow Aeolian (deg VII Maj [Add 6])

-- "I maj7sus4 over its leading tone" — a slash bass:
slashed (deg I Maj7 [Sus 4]) VII
```

Tensions are absolute intervals from the root: `Add 7` is the *major* 7th (use
the `Min7` quality for a dominant-style flat 7), `Sharp 11` / `Flat 9` shift a
degree by a semitone, `Sus n` replaces the 3rd.

## The voicing layer — `Harmonia.Voicing`

A `Voicing` is a sorted-ascending array of MIDI notes. One lift in, then
everything composes:

| Function | Role |
|---|---|
| `closeVoicing { centre }` | `Chord -> Voicing` — the single lift to concrete pitch |
| `openTriad` `rootless` `drop2` `drop2and4` `quartal` `cluster` `spread` | `Voicing -> Voicing` strategies, composed with `<<<` |
| `Selector` + `takeChord` / `takeVoicing` | carve sub-chords (`TakeLow` / `TakeHigh` / `TakeRange` / `TakeIndices` / `TakeEvery` / `DropS`) |
| `voiceLead` | `Voicing -> Chord -> Voicing` — minimum-motion next voicing |
| `enumerateVoicings` | every candidate, ranked by motion |
| `play` / `playFrom` | realize + voice-lead a whole `Progression` |

```purescript
-- Strategies compose like any function:
(rootless <<< drop2) (closeVoicing { centre: 4 } cmaj7)
```

## A worked example — McMullen's "Yellow"

`mcmullenYellow` is Joe McMullen's 18-chord table (see [References](#references))
encoded as recipes, with `mcmullenYellowNames` for labels, golden-tested against
C major. In McMullen's scheme the *key* is fixed and each step picks a *scale
position*, so the chord root walks as you move through the table — exactly what
`realize` + `play` reproduce below:

```
#   name        pitch classes     voicing
1   iv6/9       C D F G G#        C4 D4 F4 G4 G#4
2   iiø         C D G G#          C4 D4 G4 G#4
3   ♭VII6       D F G A#          A#3 D4 F4 G4
…
13  Imaj7       C E G B           C3 E3 G3 B3
14  vi9         C E G A B         C3 E3 G3 A3 B3
15  IVmaj9      C E F G A         C3 E3 F3 G3 A3
16  ii7         C D F A           C3 D3 F3 A3
17  Imaj7/7     C F G B           B2 C3 F3 G3
18  V7sus4      C D F G           C3 D3 F3 G3
```

The full tour is a runnable program:

```
spago run -p harmonia-example
```

## Quantisation — `Harmonia.PitchSet` & `Harmonia.Quantise`

A `PitchSet` is a quantisation target: literal semitone offsets from a root, with
an optional tiling `period`. It has two readings — `realize` maps an *index* to a
pitch (a scale is the degenerate octave-periodic set), and `quantiseNearest` snaps
a *pitch* to the nearest member, register following the input. After the
quantisation options Jason Lim built into the Instruo Dáil, three orthogonal axes:

- **pure vs extended** — `chord [0,4,7]` tiles every octave (all D's are the 9th);
  `extendedChord root offsets octaves` spans multiple octaves, so a 9th an octave
  up is a member but a home-octave D is not.
- **nearest vs equal** — `quantiseNearest` snaps a pitch by true pitch distance;
  `quantiseEqual` divides an input range into equal slots per degree, ignoring the
  degrees' pitch spacing.

Where does the 9th live? (`extendedChord 60 [0,4,7,14] 2`, a two-octave span):

```
input   pure add9   extended add9
D4      D4          E4      ← a home-octave D has no 9th here; snaps to E
A#4     C5          G4
B4      C5          D5
D5      D5          D5      ← the 9th, an octave up, is a member
```

Nearest vs equal over an uneven scale (C pentatonic, gaps 2-2-3-2-3):

```
NEAREST  chromatic → nearest tone       EQUAL  knob 0-255 → even slots
  D#4 → E4    F4 → E4    B4 → C5           0→C4   64→E4   128→C5   192→E5   255→A5
```

The full tour — all three axes, plus a preview of the Odonus voice pipeline
(`knob → equal(scale) → nearest(chord)`) — is a runnable program that prints
worked tables to stdout:

```
spago run -p harmonia-example --main Example.Quantise
```

## Relationship to the School of Music

`harmonia` is **complementary** to
[`purescript-school-of-music`](https://github.com/newlandsvalley/purescript-school-of-music)
(John Watson — [@newlandsvalley](https://github.com/newlandsvalley) — and his
port of the Haskell School of Music / Euterpea), not a competitor. PSoM models *time, performance and notation* — a `Music` algebra
with sequential (`:+:`) and parallel (`:=:`) composition, spelled pitches,
tempo, instruments, ABC and MIDI — but it has no functional-harmony or
voice-leading layer. `harmonia` is exactly that missing layer, one level below:
it answers *which notes form this chord and how to move between them*, leaving
*when they play* to a score algebra above. The two meet cleanly at one type — a
`Voicing` (sorted MIDI) maps directly onto a PSoM parallel stack of notes — so a
thin adapter could hand `harmonia`'s output to PSoM for notation and playback.

## References

- **Joe McMullen** — designer of the chord tables `mcmullenYellow` encodes.
  They are credited as "alt chord tables by Joe McMullen" in the alternative
  Mutable Instruments firmware below, and the same 18-chord table also appears
  in stock Plaits' Easter-egg chord mode.
- **Lyle Mills** ([@lylepmills](https://github.com/lylepmills)) — alternative
  firmware for Mutable Instruments modules (Plaits, Rings, …) that ships
  McMullen's chord tables:
  [`lylepmills/eurorack` › `alt_firmwares`](https://github.com/lylepmills/eurorack/tree/master/alt_firmwares).
- **Mutable Instruments Plaits** — the module whose chord engine this vocabulary
  echoes:
  [documentation](https://pichenettes.github.io/mutable-instruments-documentation/modules/plaits/).
- **John Watson** ([@newlandsvalley](https://github.com/newlandsvalley)) —
  [`purescript-school-of-music`](https://github.com/newlandsvalley/purescript-school-of-music),
  the PureScript port of the Haskell School of Music; the score / notation layer
  that sits *above* `harmonia` (see above).
- **The Haskell School of Music** — Paul Hudak & Donya Quick, *The Haskell
  School of Music: From Signals to Symphonies* — the lineage behind PSoM's
  `Music` / Euterpea algebra.

## Develop

```
spago build              # the library
spago test               # golden tests (McMullen Yellow + voice leading)
spago run -p harmonia-example
```

## License

MIT © Andrew Condon
