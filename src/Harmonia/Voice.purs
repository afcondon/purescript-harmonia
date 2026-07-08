-- | One quantised voice — the two-stage pipeline an index-driven instrument uses
-- | to turn a raw control value into a sounding pitch. It is pure composition of
-- | the two `Harmonia.Quantise` modes over `Harmonia.PitchSet`, and it is the
-- | shape the reef "Odonus" voice ports onto (see the reef spec
-- | `docs/PLAN-odonus-pitch-pipeline.md`).
-- |
-- | The gesture, in two quantisations:
-- |
-- |   1. EQUAL over a fixed `scale` maps the raw knob to a scale tone at real
-- |      register — the `home` note. This is the STABLE MELODIC SHAPE: it does not
-- |      move when the harmony moves, so a melody keeps its contour and range.
-- |   2. NEAREST over the `activeSet` (a firing chord, else the scale itself)
-- |      snaps `home + offset` to the current harmony. This is the CONSTRAINT: it
-- |      colours the melody at the very end without replacing it.
-- |
-- | Two properties are load-bearing and come straight from the quantiser laws:
-- |
-- |   * REGISTER FOLLOWS THE MELODY. `quantiseNearest` is local
-- |     (`2·|q n − n| ≤ period`), so the snapped note sits within half an octave
-- |     of `home + offset` — the chord recolours the melody in place, never yanks
-- |     it to the chord's own register.
-- |   * SIMPLE MODE IS TRANSPARENT. When `activeSet == scale`, `home` is already a
-- |     member, so by the fixed-point law `voiceLabel == home` and the sounding
-- |     note is just `home + globalOct`. A scale with no chord passes through.
-- |
-- | A spread of per-voice `offset`s (chromatic, ±12) makes several voices land on
-- | DIFFERENT chord tones — `home = C`, `offset = +3` snaps to the chord's `E` —
-- | so a small ensemble sounds the whole chord with no special-casing. That is the
-- | "voice-spread" axis, left out of the theory (`Harmonia.Quantise`) on purpose
-- | and recovered here for free by nearest-snap over spread offsets.
module Harmonia.Voice
  ( VoiceContext
  , VoiceControl
  , home
  , voiceLabel
  , voiceNote
  ) where

import Prelude

import Harmonia.PitchSet (PitchSet)
import Harmonia.Quantise (quantiseEqual, quantiseNearest)

-- | The instrument-global harmonic context, shared by every voice on one pass.
-- | `scale` is the stable index source (the key); `activeSet` is the current
-- | constraint — the firing chord, or `scale` itself when nothing fires.
type VoiceContext =
  { scale :: PitchSet     -- ^ stable melodic home; equal-mapping indexes THIS
  , activeSet :: PitchSet  -- ^ the constraint: a firing chord, else == scale
  , span :: Int            -- ^ how many periods of `scale` the knob sweeps
  , knobMax :: Int         -- ^ raw knob ceiling (e.g. 255)
  , globalOct :: Int       -- ^ ±12·k semitones, added to every voice's output
  }

-- | Per-voice control: the raw knob (`0 .. knobMax`, shown on the face) and a
-- | chromatic offset (`-12 .. 12`) fired into the constraint before it snaps.
type VoiceControl =
  { knob :: Int
  , offset :: Int
  }

-- | Stage 1 (equal): the raw knob → a `scale` tone at real register. The melodic
-- | shape, independent of the harmony.
home :: VoiceContext -> Int -> Int
home ctx knob = quantiseEqual ctx.scale ctx.span ctx.knobMax knob

-- | The knob's live label: what `home` becomes under the current constraint,
-- | WITHOUT the per-voice offset. In simple mode this is just `home`; while a
-- | chord fires it re-colours as the harmony moves. Offset voices sound different
-- | from their label — the label is the note "absent downstream offsets".
voiceLabel :: VoiceContext -> Int -> Int
voiceLabel ctx knob = quantiseNearest ctx.activeSet (home ctx knob)

-- | The full pipeline: equal-home, chromatic offset, nearest-snap to the active
-- | set, then the global octave shift. The sounding MIDI note for one voice.
voiceNote :: VoiceContext -> VoiceControl -> Int
voiceNote ctx { knob, offset } =
  quantiseNearest ctx.activeSet (home ctx knob + offset) + ctx.globalOct
