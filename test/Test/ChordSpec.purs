-- | Tests for `Harmonia.Chord` — realize correctness.
-- |
-- | Golden test: realise `mcmullenYellow` against `cMajorKey` and
-- | check the pitch-class set of each chord against the expected
-- | output computed by hand from the McMullen Yellow column.
-- | See the McMullen Yellow column for the chord
-- | derivations.
module Test.ChordSpec
  ( runChordTests
  ) where

import Prelude

import Data.Array (zip)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual')

import Harmonia.Chord
  ( Chord(..)
  , Numeral(..)
  , Quality(..)
  , Tension(..)
  , Mode(..)
  , borrow
  , cMajorKey
  , deg
  , mcmullenYellow
  , realize
  )

runChordTests :: Effect Unit
runChordTests = do
  log ""
  log "--- Harmonia.Chord — realize correctness ---"

  -- ------------------------------------------------------------------
  -- Simple sanity: I, IV, V triads in C major.
  -- ------------------------------------------------------------------
  expectChord
    "I  in C major (Maj) → [0, 4, 7] (C E G)"
    [0, 4, 7]
    (realize cMajorKey (deg I Maj []))

  expectChord
    "IV in C major (Maj) → [0, 5, 9] (F A C)"
    [0, 5, 9]
    (realize cMajorKey (deg IV Maj []))

  expectChord
    "V  in C major (Maj) → [2, 7, 11] (G B D)"
    [2, 7, 11]
    (realize cMajorKey (deg V Maj []))

  -- ------------------------------------------------------------------
  -- Seventh chords
  -- ------------------------------------------------------------------
  expectChord
    "I  Maj7 in C major → [0, 4, 7, 11] (C E G B)"
    [0, 4, 7, 11]
    (realize cMajorKey (deg I Maj7 []))

  expectChord
    "V  Dom7 in C major → [2, 5, 7, 11] (G B D F)"
    [2, 5, 7, 11]
    (realize cMajorKey (deg V Dom7 []))

  -- ------------------------------------------------------------------
  -- McMullen Yellow column — full 18-chord golden output.
  -- Expected pitch-class sets sorted ascending.
  -- ------------------------------------------------------------------
  log ""
  log "  McMullen Yellow column (against cMajorKey):"
  let expected =
        [ Tuple "iv 6/9"          [0, 2, 5, 7, 8]      -- F Ab C D G
        , Tuple "iio 7sus4"       [0, 2, 7, 8]         -- D G Ab C
        , Tuple "VII 6"           [2, 5, 7, 10]        -- Bb D F G
        , Tuple "v m11"           [0, 2, 7, 10]        -- G Bb D C
        , Tuple "III add4"        [3, 7, 8, 10]        -- Eb G Ab Bb
        , Tuple "i addb13"        [0, 3, 7, 8]         -- C Eb G Ab
        , Tuple "VI add#11"       [0, 2, 3, 8]         -- Ab C Eb D
        , Tuple "iv m6"           [0, 2, 5, 8]         -- F Ab C D
        , Tuple "iio"             [2, 5, 8]            -- D F Ab
        , Tuple "viio"            [2, 5, 11]           -- B D F
        , Tuple "V7"              [2, 5, 7, 11]        -- G B D F
        , Tuple "iii addb9"       [4, 5, 7, 11]        -- E G B F
        , Tuple "I maj7"          [0, 4, 7, 11]        -- C E G B
        , Tuple "vi m9"           [0, 4, 7, 9, 11]     -- A C E G B
        , Tuple "IV maj9"         [0, 4, 5, 7, 9]      -- F A C E G
        , Tuple "ii m7"           [0, 2, 5, 9]         -- D F A C
        , Tuple "I maj7sus4/vii"  [0, 5, 7, 11]        -- C F G B (bass=B already in)
        , Tuple "V 7sus4"         [0, 2, 5, 7]         -- G C D F
        ]
  for_ (zip expected (map (realize cMajorKey) mcmullenYellow)) \(Tuple (Tuple label pcs) actual) ->
    expectChord ("    " <> label) pcs actual

  -- ------------------------------------------------------------------
  -- Modal borrowing — VII Maj borrowed from Aeolian → root shifts
  -- from B (Ionian VII = +11) to Bb (Aeolian VII = +10).
  -- ------------------------------------------------------------------
  log ""
  log "  Modal borrowing:"
  expectChord
    "VII Maj borrowed from Aeolian → root Bb (not B)"
    [2, 5, 10]                                  -- Bb D F (no tensions)
    (realize cMajorKey (borrow Aeolian (deg VII Maj [])))

  -- ------------------------------------------------------------------
  -- Tensions
  -- ------------------------------------------------------------------
  log ""
  log "  Tensions:"
  expectChord
    "I Maj [Sus 4] → [0, 5, 7] (C F G — 3rd suspended)"
    [0, 5, 7]
    (realize cMajorKey (deg I Maj [Sus 4]))

  expectChord
    "V Dom7 [Sus 4] → [0, 2, 5, 7] (G C D F)"
    [0, 2, 5, 7]
    (realize cMajorKey (deg V Dom7 [Sus 4]))

  expectChord
    "I Maj [Flat 9] → [0, 1, 4, 7] (C C# E G)"
    [0, 1, 4, 7]
    (realize cMajorKey (deg I Maj [Flat 9]))

  expectChord
    "I Maj [Sharp 11] → [0, 4, 6, 7] (C E F# G)"
    [0, 4, 6, 7]
    (realize cMajorKey (deg I Maj [Sharp 11]))

  expectChord
    "I Maj [NoFifth] → [0, 4] (C E)"
    [0, 4]
    (realize cMajorKey (deg I Maj [NoFifth]))

  expectChord
    "I Maj [NoThird] → [0, 7] (C G)"
    [0, 7]
    (realize cMajorKey (deg I Maj [NoThird]))

  log ""

expectChord :: String -> Array Int -> Chord -> Effect Unit
expectChord label expected (Chord actual) =
  assertEqual' label { actual, expected }
