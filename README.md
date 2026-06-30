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

`mcmullenYellow` is Joe McMullen's 18-chord Plaits table encoded as recipes
(with `mcmullenYellowNames` for labels), golden-tested against C major. Realized
and voice-led, it walks like this:

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

## Relationship to the School of Music

`harmonia` is **complementary** to
[`purescript-school-of-music`](https://github.com/newlandsvalley/purescript-school-of-music)
(John Watson's port of the Haskell School of Music / Euterpea), not a
competitor. PSoM models *time, performance and notation* — a `Music` algebra
with sequential (`:+:`) and parallel (`:=:`) composition, spelled pitches,
tempo, instruments, ABC and MIDI — but it has no functional-harmony or
voice-leading layer. `harmonia` is exactly that missing layer, one level below:
it answers *which notes form this chord and how to move between them*, leaving
*when they play* to a score algebra above. The two meet cleanly at one type — a
`Voicing` (sorted MIDI) maps directly onto a PSoM parallel stack of notes — so a
thin adapter could hand `harmonia`'s output to PSoM for notation and playback.

## Develop

```
spago build              # the library
spago test               # golden tests (McMullen Yellow + voice leading)
spago run -p harmonia-example
```

## License

MIT © Andrew Condon
