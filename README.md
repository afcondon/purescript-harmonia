# purescript-harmonia

Functional-harmony **recipes** and **voice leading** for PureScript — the
reusable core extracted from the [Vetula](https://github.com/afcondon) /
purerl-tidal music stack, where it had been vendored verbatim across four
projects.

Two pure modules, no `Effect`, no FFI, deps limited to
`prelude`/`arrays`/`foldable-traversable`/`maybe`/`tuples` — so it compiles
unchanged under both the JavaScript backend and **purerl** (Erlang).

## `Harmonia.Chord` — the recipe layer

A scale-degree-relative chord vocabulary that resolves against a key into an
unordered pitch-class set (0–11).

```purescript
realize :: Key -> DegreeChord -> Chord
```

- `Numeral` (I…VII), `Quality` (triads + sevenths), `Tension`
  (`Add`/`Sharp`/`Flat`/`Sus`/`NoFifth`/`NoThird`, absolute-interval semantics),
  `Mode` (church modes + harmonic/melodic minor + their modes + `Custom`).
- `DegreeChord` with smart constructors `deg`, `slashed`, `borrow` (modal
  interchange — `borrow Aeolian (deg VII Maj [Add 6])`).
- `chordRoot` / `chordBass` projections (ground a chord, label it by root).
- `mcmullenYellow` + `mcmullenYellowNames` — Joe McMullen's 18-chord "Yellow"
  Plaits table as a worked example, golden-tested against C major.

## `Harmonia.Voicing` — concrete pitch & voice leading

Lifts the pitch-class set to sorted MIDI notes and moves between chords
smoothly.

```purescript
closeVoicing  :: { centre :: Int } -> Chord -> Voicing
voiceLead     :: Voicing -> Chord -> Voicing
enumerateVoicings :: Voicing -> Chord -> Array (Tuple Voicing Int)  -- ranked by total motion
play          :: Key -> VoicingStrategy -> Progression -> Array Voicing
```

- Composable voicing strategies: `openTriad`, `rootless`, `drop2`, `drop2and4`,
  `quartal`, `cluster`, `spread`.
- A small `Selector` algebra (`TakeLow`/`TakeHigh`/`TakeRange`/`TakeIndices`/
  `TakeEvery`/`DropS`) for carving sub-chords.

## Relationship to the School of Music

This is **complementary** to
[`purescript-school-of-music`](https://github.com/newlandsvalley/purescript-school-of-music)
(John Watson's port of the Haskell School of Music / Euterpea). PSoM models
*time, performance and notation* (`Music` with sequential `:+:` / parallel
`:=:` composition, spelled pitches, tempo, instruments) but has no
functional-harmony or voice-leading layer. Harmonia is exactly that missing
layer, one level below: it answers *which notes form this chord and how to
move between them*, leaving *when they play* to a score algebra above. The two
meet at one type — a `Voicing` (sorted MIDI) maps directly onto a PSoM
parallel stack of notes.

## Build

```bash
spago build
spago test
```

## License

MIT
