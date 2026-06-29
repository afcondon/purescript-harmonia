-- | Tests for `Harmonia.Voicing` — voicing primitives,
-- | transformations, composition, and selectors.
module Test.VoicingSpec
  ( runVoicingTests
  ) where

import Prelude

import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert', assertEqual')

import Data.Array as Array
import Data.Tuple (Tuple(..))

import Harmonia.Chord
  ( Chord(..)
  , Numeral(..)
  , Quality(..)
  , cMajorKey
  , deg
  , mcmullenYellow
  , realize
  )
import Harmonia.Voicing
  ( Selector(..)
  , Voicing(..)
  , closeVoicing
  , cluster
  , drop2
  , drop2and4
  , enumerateVoicings
  , nearestNote
  , openTriad
  , play
  , quartal
  , rootless
  , spread
  , takeChord
  , takeVoicing
  , voiceLead
  )

runVoicingTests :: Effect Unit
runVoicingTests = do
  log ""
  log "--- Harmonia.Voicing ---"

  -- ------------------------------------------------------------------
  -- closeVoicing — the one Chord → Voicing lift
  -- ------------------------------------------------------------------
  log ""
  log "  closeVoicing:"
  expectVoicing
    "C major triad at centre 4 → [60, 64, 67]"
    [60, 64, 67]
    (closeVoicing { centre: 4 } (Chord [0, 4, 7]))
  expectVoicing
    "Cmaj7 at centre 4 → [60, 64, 67, 71]"
    [60, 64, 67, 71]
    (closeVoicing { centre: 4 } (Chord [0, 4, 7, 11]))
  expectVoicing
    "Cmaj7 at centre 5 → [72, 76, 79, 83]"
    [72, 76, 79, 83]
    (closeVoicing { centre: 5 } (Chord [0, 4, 7, 11]))
  expectVoicing
    "PCs [0, 5, 7, 10] at centre 3 → [48, 53, 55, 58]"
    [48, 53, 55, 58]
    (closeVoicing { centre: 3 } (Chord [0, 5, 7, 10]))

  -- ------------------------------------------------------------------
  -- openTriad — lift the 2nd-from-bottom note up an octave
  -- ------------------------------------------------------------------
  log ""
  log "  openTriad:"
  expectVoicing
    "C major triad opened → [60, 67, 76] (C G E')"
    [60, 67, 76]
    (openTriad (Voicing [60, 64, 67]))
  expectVoicing
    "Cmaj7 opened: index-1 lifted → [60, 67, 71, 76]"
    [60, 67, 71, 76]
    (openTriad (Voicing [60, 64, 67, 71]))
  expectVoicing
    "openTriad on 1-note voicing is identity"
    [60]
    (openTriad (Voicing [60]))

  -- ------------------------------------------------------------------
  -- rootless — drop the bottom note
  -- ------------------------------------------------------------------
  log ""
  log "  rootless:"
  expectVoicing
    "Cmaj7 rootless → [64, 67, 71]"
    [64, 67, 71]
    (rootless (Voicing [60, 64, 67, 71]))
  expectVoicing
    "rootless on empty voicing is identity"
    []
    (rootless (Voicing []))

  -- ------------------------------------------------------------------
  -- drop2 — lower the 2nd-from-top note an octave
  -- ------------------------------------------------------------------
  log ""
  log "  drop2:"
  expectVoicing
    "Cmaj7 close → drop2: [55, 60, 64, 71]"
    [55, 60, 64, 71]
    (drop2 (Voicing [60, 64, 67, 71]))
  expectVoicing
    "G7 close → drop2: [50, 55, 59, 65]"
    [50, 55, 59, 65]
    (drop2 (Voicing [55, 59, 62, 65]))

  -- ------------------------------------------------------------------
  -- drop2and4 — lower 2nd and 4th from top each an octave
  -- ------------------------------------------------------------------
  log ""
  log "  drop2and4:"
  expectVoicing
    "Cmaj7 close → drop2and4: [48, 55, 64, 71]"
    [48, 55, 64, 71]
    (drop2and4 (Voicing [60, 64, 67, 71]))
  expectVoicing
    "drop2and4 on 3-note voicing falls back to drop2"
    [52, 60, 67]
    (drop2and4 (Voicing [60, 64, 67]))

  -- ------------------------------------------------------------------
  -- quartal — restack in 4ths from the bottom
  -- ------------------------------------------------------------------
  log ""
  log "  quartal:"
  expectVoicing
    "C F Bb already-4ths → [60, 65, 70]"
    [60, 65, 70]
    (quartal (Voicing [60, 65, 70]))
  expectVoicing
    "C F G Bb sus → cycle C F Bb G → [60, 65, 70, 79]"
    [60, 65, 70, 79]
    (quartal (Voicing [60, 65, 67, 70]))

  -- ------------------------------------------------------------------
  -- cluster — compress to smallest octave window
  -- ------------------------------------------------------------------
  log ""
  log "  cluster:"
  expectVoicing
    "[60, 76, 91] (C4 E5 G6) clustered → [60, 64, 67]"
    [60, 64, 67]
    (cluster (Voicing [60, 76, 91]))
  expectVoicing
    "[60, 60, 64] (dup PC) clustered → [60, 64]"
    [60, 64]
    (cluster (Voicing [60, 60, 64]))

  -- ------------------------------------------------------------------
  -- spread — distribute across octave range
  -- ------------------------------------------------------------------
  log ""
  log "  spread:"
  expectVoicing
    "1-note voicing spread {3, 5} → [48]"
    [48]
    (spread { low: 3, high: 5 } (Voicing [60]))
  -- For multi-note spread, just sanity-check the range bounds
  expectInRange
    "spread Cmaj7 across octaves 3..5 keeps notes in (loose) range"
    { lo: 36, hi: 83 }
    (spread { low: 3, high: 5 } (Voicing [60, 64, 67, 71]))

  -- ------------------------------------------------------------------
  -- Composition through (<<<)
  -- ------------------------------------------------------------------
  log ""
  log "  Composition through (<<<):"
  let
    cmaj7 = closeVoicing { centre: 4 } (Chord [0, 4, 7, 11])

    pianoStyle :: Voicing -> Voicing
    pianoStyle = drop2

    rootlessDrop2 :: Voicing -> Voicing
    rootlessDrop2 = rootless <<< drop2
  expectVoicing
    "pianoStyle (closeVoicing Cmaj7) → [55, 60, 64, 71]"
    [55, 60, 64, 71]
    (pianoStyle cmaj7)
  expectVoicing
    "(rootless <<< drop2) Cmaj7 → [60, 64, 71]"
    [60, 64, 71]
    (rootlessDrop2 cmaj7)

  -- ------------------------------------------------------------------
  -- Selectors on Chord
  -- ------------------------------------------------------------------
  log ""
  log "  Selectors on Chord (sorted pitch-class array):"
  let cmaj9 = Chord [0, 2, 4, 7, 11]
  expectChord "TakeLow 2 → [0, 2]"
    [0, 2] (takeChord (TakeLow 2) cmaj9)
  expectChord "TakeHigh 2 → [7, 11]"
    [7, 11] (takeChord (TakeHigh 2) cmaj9)
  expectChord "TakeRange 1 4 → [2, 4, 7]"
    [2, 4, 7] (takeChord (TakeRange 1 4) cmaj9)
  expectChord "TakeIndices [0, 2, 4] → [0, 4, 11]"
    [0, 4, 11] (takeChord (TakeIndices [0, 2, 4]) cmaj9)
  expectChord "TakeEvery 0 2 → [0, 4, 11]"
    [0, 4, 11] (takeChord (TakeEvery 0 2) cmaj9)
  expectChord "DropS (TakeLow 1) → [2, 4, 7, 11]"
    [2, 4, 7, 11] (takeChord (DropS (TakeLow 1)) cmaj9)

  -- ------------------------------------------------------------------
  -- Selectors on Voicing
  -- ------------------------------------------------------------------
  log ""
  log "  Selectors on Voicing (low-to-high voice positions):"
  let voicing4 = Voicing [60, 64, 67, 71]
  expectVoicing "TakeLow 1 → [60]"
    [60] (takeVoicing (TakeLow 1) voicing4)
  expectVoicing "TakeHigh 2 → [67, 71]"
    [67, 71] (takeVoicing (TakeHigh 2) voicing4)
  expectVoicing "DropS (TakeLow 1) → [64, 67, 71]"
    [64, 67, 71] (takeVoicing (DropS (TakeLow 1)) voicing4)

  -- ------------------------------------------------------------------
  -- End-to-end realize → voice
  -- ------------------------------------------------------------------
  log ""
  log "  End-to-end realize → voice:"
  let
    iMaj7 = realize cMajorKey (deg I Maj7 [])
    iMaj7Voiced = drop2 (closeVoicing { centre: 4 } iMaj7)
  expectVoicing
    "realize I Maj7 → closeVoicing centre 4 → drop2 → [55, 60, 64, 71]"
    [55, 60, 64, 71]
    iMaj7Voiced

  -- ------------------------------------------------------------------
  -- V-C: voice leading
  -- ------------------------------------------------------------------
  log ""
  log "  nearestNote (place a PC at the nearest octave to a target):"
  expectInt "nearestNote 71 9 → 69 (B4→A4 just below)" 69 (nearestNote 71 9)
  expectInt "nearestNote 60 0 → 60 (identity)"          60 (nearestNote 60 0)
  expectInt "nearestNote 60 7 → 55 (G3 closer than G4)" 55 (nearestNote 60 7)
  expectInt "nearestNote 64 5 → 65 (F4 closer than F3)" 65 (nearestNote 64 5)

  log ""
  log "  voiceLead:"
  expectVoicing
    "voiceLead Cmaj [60 64 67] → Chord [0 4 7] is identity"
    [60, 64, 67]
    (voiceLead (Voicing [60, 64, 67]) (Chord [0, 4, 7]))
  expectVoicing
    "voiceLead Cmaj7 → Am7: common tones preserved, B→A"
    [60, 64, 67, 69]
    (voiceLead (Voicing [60, 64, 67, 71]) (Chord [0, 4, 7, 9]))
  expectVoicing
    "voiceLead Cmaj → F (different roots, IV cadence)"
    -- Cmaj voicing [60, 64, 67] (C E G) → F chord [0, 5, 9].
    -- Best perm: 60→0 (move 0), 64→5 (nearest F: 65, move 1), 67→9 (nearest A: 69, move 2). Total 3.
    -- Or:        60→5 (nearest F: 65? 65-60=5, 53-60=7. So 65, move 5), 64→9 (69, move 5), 67→0 (72, move 5). Total 15. Worse.
    -- So voicing [60, 65, 69].
    [60, 65, 69]
    (voiceLead (Voicing [60, 64, 67]) (Chord [0, 5, 9]))
  expectVoicing
    "voiceLead size mismatch: 3 voices → 4 PCs → fallback closeVoicing"
    -- bottomOctave of [60, 64, 67] = 60/12 - 1 = 5 - 1 = 4. centre = 4.
    -- closeVoicing { centre: 4 } (Chord [0, 4, 7, 11]) = [60, 64, 67, 71].
    [60, 64, 67, 71]
    (voiceLead (Voicing [60, 64, 67]) (Chord [0, 4, 7, 11]))

  log ""
  log "  enumerateVoicings:"
  let
    candidates = enumerateVoicings (Voicing [60, 64, 67, 71]) (Chord [0, 4, 7, 9])
    -- First entry should be the voiceLead result (lowest motion)
    firstCandidate = Array.head candidates
  case firstCandidate of
    Just (Tuple v score) -> do
      expectVoicing "head of enumerate matches voiceLead output" [60, 64, 67, 69] v
      expectInt "head score = 2 (B→A motion)" 2 score
    Nothing -> assert' "enumerateVoicings returned empty" false
  let scores = map (\(Tuple _ s) -> s) candidates
  assert' ("enumerate scores out of order: " <> show scores) (scoresAscending scores)

  log ""
  log "  play (key + strategy + progression):"
  let
    chords = [ deg I Maj7 [], deg VI Min7 [], deg IV Maj7 [], deg V Dom7 [] ]
    voicings = play cMajorKey identity chords
  expectInt "play returns one voicing per chord (4 chords → 4 voicings)" 4 (Array.length voicings)
  case Array.head voicings of
    Just (Voicing vs) -> expectInt "first voicing of I Maj7 has 4 notes" 4 (Array.length vs)
    Nothing           -> assert' "play returned no voicings for non-empty progression" false

  let
    mcVoicings = play cMajorKey identity mcmullenYellow
  expectInt "play mcmullenYellow → 18 voicings" 18 (Array.length mcVoicings)

  log ""

-- ---------------------------------------------------------------------------
-- Spec helpers (continued)
-- ---------------------------------------------------------------------------

expectInt :: String -> Int -> Int -> Effect Unit
expectInt label expected actual =
  assertEqual' label { actual, expected }

scoresAscending :: Array Int -> Boolean
scoresAscending xs = case Array.uncons xs of
  Nothing -> true
  Just { head, tail } -> walk head tail
  where
    walk prev arr = case Array.uncons arr of
      Nothing -> true
      Just { head: next, tail: more } ->
        if next >= prev then walk next more else false

-- ---------------------------------------------------------------------------
-- Spec helpers
-- ---------------------------------------------------------------------------

expectVoicing :: String -> Array Int -> Voicing -> Effect Unit
expectVoicing label expected (Voicing actual) =
  assertEqual' label { actual, expected }

expectChord :: String -> Array Int -> Chord -> Effect Unit
expectChord label expected (Chord actual) =
  assertEqual' label { actual, expected }

-- | Sanity check that a voicing's notes all fall within an inclusive
-- | MIDI range.  Used for `spread` where exact placement depends on
-- | rounding decisions and we just want to assert the bounds hold.
expectInRange :: String -> { lo :: Int, hi :: Int } -> Voicing -> Effect Unit
expectInRange label { lo, hi } (Voicing xs) =
  let
    lowestNote = fromMaybe 0 (minimum xs)
    highestNote = fromMaybe 0 (maximum xs)
  in
    assert'
      (label <> " — expected notes in [" <> show lo <> ", " <> show hi <> "]"
        <> " (min=" <> show lowestNote <> ", max=" <> show highestNote <> ")")
      (lowestNote >= lo && highestNote <= hi)
