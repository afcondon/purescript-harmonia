-- | A runnable tour of `Harmonia.PitchSet` + `Harmonia.Quantise` — tables of
-- | worked examples to stdout. Run from the package root with:
-- |
-- |     spago run -p harmonia-example --main Example.Quantise
-- |
-- | It walks the three Instruo Dáil axes (pure vs extended sets; nearest vs equal
-- | quantising) and finishes with a preview of the Odonus voice pipeline
-- | (knob → equal-spacing over the scale → nearest to the Vetula chord).
module Example.Quantise (main) where

import Prelude

import Data.Array (mapWithIndex, range, (!!))
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String (length) as Str
import Effect (Effect)
import Effect.Console (log)
import Harmonia.PitchSet (chord, extendedChord, scale)
import Harmonia.Quantise (quantiseEqual, quantiseNearest)
import Harmonia.Voice (home, voiceLabel, voiceNote)

-- ---------------------------------------------------------------------------
-- Pretty-printing
-- ---------------------------------------------------------------------------

noteNames :: Array String
noteNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

pcName :: Int -> String
pcName pc = fromMaybe "?" (noteNames !! (pc `mod` 12))

-- | MIDI note → spelled note with octave (middle C = C4 = 60).
midiName :: Int -> String
midiName n = pcName n <> show ((n `div` 12) - 1)

pad :: Int -> String -> String
pad w s = s <> power " " (max 0 (w - Str.length s))

rule :: String -> Effect Unit
rule title = do
  log ""
  log ("── " <> title <> " " <> power "─" (max 0 (66 - Str.length title)))

-- ---------------------------------------------------------------------------
-- 1. Nearest-interval over a few chords (Dáil axis 2 = nearest; axis 1 = pure)
-- ---------------------------------------------------------------------------

demoNearest :: Effect Unit
demoNearest = do
  rule "1 · NEAREST-INTERVAL, pure chords (every octave the same)"
  log "A chromatic input snaps to the nearest chord tone; register follows the input."
  log ""
  let cMaj = chord [ 0, 4, 7 ]       -- C E G
      cMaj7 = chord [ 0, 4, 7, 11 ]  -- C E G B
      fs9 = chord [ 6, 9, 1, 4, 8 ]  -- F# A C# E G#  (F#min9)
  log (pad 10 "input" <> pad 12 "C major" <> pad 12 "Cmaj7" <> "F#min9")
  for_ (range 60 72) \n ->
    log ( pad 10 (midiName n)
        <> pad 12 (midiName (quantiseNearest cMaj n))
        <> pad 12 (midiName (quantiseNearest cMaj7 n))
        <> midiName (quantiseNearest fs9 n) )

-- ---------------------------------------------------------------------------
-- 2. Pure vs extended (Dáil axis 1) — the octave-dependent colour tone
-- ---------------------------------------------------------------------------

demoExtended :: Effect Unit
demoExtended = do
  rule "2 · PURE vs EXTENDED — where does the 9th live?"
  log "C-add9. PURE: D is a chord tone in EVERY octave. EXTENDED (2-octave span):"
  log "the 9th (D) sits only an octave up — a home-octave D snaps to E, not D."
  log ""
  let pureAdd9 = chord [ 0, 4, 7, 2 ]              -- C E G D, every octave
      extAdd9 = extendedChord 60 [ 0, 4, 7, 14 ] 2 -- C4 E4 G4 + D5, 2-octave span
  log (pad 10 "input" <> pad 14 "pure add9" <> "extended add9")
  for_ (range 60 74) \n ->
    log ( pad 10 (midiName n)
        <> pad 14 (midiName (quantiseNearest pureAdd9 n))
        <> midiName (quantiseNearest extAdd9 n) )

-- ---------------------------------------------------------------------------
-- 3. Nearest vs equal (Dáil axis 2) over an uneven scale
-- ---------------------------------------------------------------------------

demoNearestVsEqual :: Effect Unit
demoNearestVsEqual = do
  rule "3 · NEAREST vs EQUAL spacing — C pentatonic (gaps 2-2-3-2-3)"
  log "NEAREST: a chromatic input; wide gaps capture more input (pitch-proportional)."
  log "EQUAL:   a linear 0..255 input; each degree owns an EQUAL slice, gaps ignored."
  log ""
  let penta = scale 60 [ 0, 2, 4, 7, 9 ]
  log (pad 10 "chromatic" <> pad 10 "nearest")
  for_ (range 60 72) \n ->
    log (pad 10 (midiName n) <> midiName (quantiseNearest penta n))
  log ""
  log (pad 10 "knob0-255" <> pad 10 "equal")
  for_ [ 0, 32, 64, 96, 128, 160, 192, 224, 255 ] \v ->
    log (pad 10 (show v) <> midiName (quantiseEqual penta 2 255 v))

-- ---------------------------------------------------------------------------
-- 4. The Odonus voice pipeline (the locked spec, decision A)
-- ---------------------------------------------------------------------------

demoPipeline :: Effect Unit
demoPipeline = do
  rule "4 · ODONUS PIPELINE — Harmonia.Voice (knob → equal(scale) → nearest(activeSet))"
  log "q1 = equal-spacing over the scale (the stable melodic shape). The knob LABEL is"
  log "q2 = nearest to the active set: the scale (simple mode) OR the Vetula chord."
  log ""
  let cMajor = scale 60 [ 0, 2, 4, 5, 7, 9, 11 ]   -- the base scale / key
      fMaj7 = chord [ 5, 9, 0, 4 ]                  -- a firing Vetula chord (Fmaj7)
      simple = { scale: cMajor, activeSet: cMajor, span: 2, knobMax: 255, globalOct: 0 }
      follow = simple { activeSet = fMaj7 }
  log (pad 10 "knob" <> pad 14 "home (q1)" <> pad 16 "simple label" <> "Vetula label →Fmaj7")
  for_ [ 0, 36, 72, 108, 144, 180, 216, 255 ] \v ->
    log ( pad 10 (show v)
        <> pad 14 (midiName (home simple v))
        <> pad 16 (midiName (voiceLabel simple v))   -- == home (a scale tone is its own label)
        <> midiName (voiceLabel follow v) )           -- the offset-free chord label
  log ""
  log "Axis 3 (free): four voices SHARE a home, fan the chromatic OFFSET (±12), and"
  log "collectively sound the chord — nearest-snap over spread offsets, no special-casing."
  log ""
  let hk = 130   -- a knob that homes to C5 (verified below)
  log ("shared home = " <> midiName (home follow hk) <> "  (knob " <> show hk <> ")")
  log (pad 10 "voice" <> pad 12 "offset" <> pad 12 "sounds" <> "pc")
  for_ (mapWithIndex (\i o -> { i: i + 1, o }) [ -1, 2, 5, 9 ]) \{ i, o } ->
    let n = voiceNote follow { knob: hk, offset: o }
    in log ( pad 10 ("v" <> show i)
           <> pad 12 (show o)
           <> pad 12 (midiName n)
           <> pcName n )

main :: Effect Unit
main = do
  log "HARMONIA QUANTISER — worked tables (Instruo Dáil)"
  demoNearest
  demoExtended
  demoNearestVsEqual
  demoPipeline
  log ""
