-- | Tests for `Harmonia.Anchor` — the classification and, above all, the
-- | diatonic-quality tables that make reflavouring music-theoretically sound.
-- |
-- | The `diatonicQuality` tables below are the **oracle**: they are the standard
-- | diatonic seventh/triad qualities of each mode, verified by hand. If the code
-- | ever disagrees with these, it is the code that is wrong. The seven church
-- | modes are also cross-checked by the rotation law (a mode's chord qualities
-- | are its parent major scale's, rotated) so the table cannot quietly drift.
module Test.AnchorSpec
  ( runAnchorTests
  ) where

import Prelude

import Data.Array (all, elem, zip)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert', assertEqual')

import Harmonia.Chord
  ( Mode(..), Numeral(..), Quality(..), Tension(..)
  , aMinorKey, borrow, cMajorKey, deg
  )
import Harmonia.Anchor
  ( Anchor(..), Grade(..), Op(..)
  , chordInScale, diatonicQuality, gradeAnchor, isSeventh, permits, permitted, scalePCs
  )

-- The seven scale degrees, in order.
numerals :: Array Numeral
numerals = [ I, II, III, IV, V, VI, VII ]

-- Every mode Harmonia names (all except open-ended `Custom`).
allNamedModes :: Array Mode
allNamedModes =
  [ Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian
  , HarmonicMinor, MelodicMinor
  , LocrianNat6, IonianSharp5, DorianSharp4, PhrygianDominant, LydianSharp2, Ultralocrian
  , DorianFlat2, LydianAugmented, LydianDominant, MixolydianFlat6, LocrianNat2, Altered
  ]

runAnchorTests :: Effect Unit
runAnchorTests = do
  log ""
  log "--- Harmonia.Anchor — grade classification ---"

  -- ------------------------------------------------------------------
  -- gradeAnchor / classify — the grade judgment.
  -- ------------------------------------------------------------------

  -- Every DIATONIC chord of C major (correct diatonic quality) → Diatonic.
  expectGrade "I  (C E G)  in C major → Diatonic"  Diatonic (Located cMajorKey (deg I   Maj  []))
  expectGrade "ii (D F A)  in C major → Diatonic"  Diatonic (Located cMajorKey (deg II  Min  []))
  expectGrade "V7 (G B D F) in C major → Diatonic" Diatonic (Located cMajorKey (deg V   Dom7 []))
  expectGrade "viiø (B D F A) in C major → Diatonic" Diatonic (Located cMajorKey (deg VII HalfDim []))
  expectGrade "i  (A C E)  in A minor → Diatonic"  Diatonic (Located aMinorKey (deg I   Min  []))

  -- Chords that LEAVE the scale (wrong quality for the degree, or an
  -- out-of-scale tension) still have a tonic → Keyed, not Diatonic.
  expectGrade "II Maj (D F# A) in C major → Keyed (F# out of scale)"
    Keyed (Located cMajorKey (deg II Maj []))
  expectGrade "I Maj [Sharp 11] (C E F# G) in C major → Keyed"
    Keyed (Located cMajorKey (deg I Maj [ Sharp 11 ]))
  expectGrade "V Maj [Flat 9] (G Ab B D) in C major → Keyed"
    Keyed (Located cMajorKey (deg V Maj [ Flat 9 ]))

  -- Explicit modal interchange → Keyed regardless of pitches.
  expectGrade "VI Maj borrowed from Aeolian → Keyed (override present)"
    Keyed (Located cMajorKey (borrow Aeolian (deg VI Maj [])))

  -- A borrow override EQUAL to the key's own mode is not really borrowing;
  -- it falls through to the in-scale test → Diatonic.
  expectGrade "I Maj borrowed from Ionian, in C major → Diatonic (no real borrow)"
    Diatonic (Located cMajorKey (borrow Ionian (deg I Maj [])))

  -- Free has no reading at all.
  expectGrade "Free → ShiftOnly" ShiftOnly Free

  -- ------------------------------------------------------------------
  -- permitted / permits — the capability ladder.
  -- ------------------------------------------------------------------
  log ""
  log "  Capability ladder (permitted):"
  assertEqual' "permitted ShiftOnly = [Shift]"
    { actual: permitted ShiftOnly, expected: [ Shift ] }
  assertEqual' "permitted Keyed = [Shift, Modulate]"
    { actual: permitted Keyed, expected: [ Shift, Modulate ] }
  assertEqual' "permitted Diatonic = [Shift, Modulate, Reflavour, Substitute]"
    { actual: permitted Diatonic, expected: [ Shift, Modulate, Reflavour, Substitute ] }

  -- Nesting: each grade's ops are a superset of the lower grade's.
  assert' "permitted ShiftOnly ⊆ permitted Keyed"
    (subset (permitted ShiftOnly) (permitted Keyed))
  assert' "permitted Keyed ⊆ permitted Diatonic"
    (subset (permitted Keyed) (permitted Diatonic))

  -- permits spot checks.
  assert' "Diatonic permits Reflavour" (permits Reflavour Diatonic)
  assert' "Keyed does NOT permit Reflavour" (not (permits Reflavour Keyed))
  assert' "Keyed permits Modulate" (permits Modulate Keyed)
  assert' "ShiftOnly does NOT permit Modulate" (not (permits Modulate ShiftOnly))
  assert' "every grade permits Shift"
    (permits Shift ShiftOnly && permits Shift Keyed && permits Shift Diatonic)

  -- ------------------------------------------------------------------
  -- scalePCs / chordInScale.
  -- ------------------------------------------------------------------
  log ""
  log "  Scale membership:"
  assertEqual' "scalePCs C Ionian = C D E F G A B"
    { actual: scalePCs cMajorKey, expected: [ 0, 2, 4, 5, 7, 9, 11 ] }
  assertEqual' "scalePCs A Aeolian = A B C D E F G"
    { actual: scalePCs aMinorKey, expected: [ 9, 11, 0, 2, 4, 5, 7 ] }
  assert' "I Maj is in C major" (chordInScale cMajorKey (deg I Maj []))
  assert' "II Maj is NOT in C major" (not (chordInScale cMajorKey (deg II Maj [])))

  -- ------------------------------------------------------------------
  -- isSeventh — all ten qualities.
  -- ------------------------------------------------------------------
  log ""
  log "  isSeventh:"
  for_ [ Maj7, Min7, Dom7, HalfDim, FullyDim, MinMaj7, AugMaj7 ] \q ->
    assert' ("isSeventh " <> show q <> " = true") (isSeventh q)
  for_ [ Maj, Min, Dim, Aug ] \q ->
    assert' ("isSeventh " <> show q <> " = false") (not (isSeventh q))

  -- ------------------------------------------------------------------
  -- diatonicQuality — THE ORACLE. Standard diatonic chord qualities.
  -- ------------------------------------------------------------------
  log ""
  log "--- Harmonia.Anchor — diatonic-quality oracle ---"

  log ""
  log "  Church-mode TRIADS:"
  expectMode "Ionian"     false [ Maj, Min, Min, Maj, Maj, Min, Dim ] Ionian
  expectMode "Dorian"     false [ Min, Min, Maj, Maj, Min, Dim, Maj ] Dorian
  expectMode "Phrygian"   false [ Min, Maj, Maj, Min, Dim, Maj, Min ] Phrygian
  expectMode "Lydian"     false [ Maj, Maj, Min, Dim, Maj, Min, Min ] Lydian
  expectMode "Mixolydian" false [ Maj, Min, Dim, Maj, Min, Min, Maj ] Mixolydian
  expectMode "Aeolian"    false [ Min, Dim, Maj, Min, Min, Maj, Maj ] Aeolian
  expectMode "Locrian"    false [ Dim, Maj, Min, Min, Maj, Maj, Min ] Locrian

  log ""
  log "  Church-mode SEVENTHS:"
  expectMode "Ionian"     true [ Maj7, Min7, Min7, Maj7, Dom7, Min7, HalfDim ] Ionian
  expectMode "Dorian"     true [ Min7, Min7, Maj7, Dom7, Min7, HalfDim, Maj7 ] Dorian
  expectMode "Phrygian"   true [ Min7, Maj7, Dom7, Min7, HalfDim, Maj7, Min7 ] Phrygian
  expectMode "Lydian"     true [ Maj7, Dom7, Min7, HalfDim, Maj7, Min7, Min7 ] Lydian
  expectMode "Mixolydian" true [ Dom7, Min7, HalfDim, Maj7, Min7, Min7, Maj7 ] Mixolydian
  expectMode "Aeolian"    true [ Min7, HalfDim, Maj7, Min7, Min7, Maj7, Dom7 ] Aeolian
  expectMode "Locrian"    true [ HalfDim, Maj7, Min7, Min7, Maj7, Dom7, Min7 ] Locrian

  -- Harmonic & melodic minor exercise the exotic qualities: MinMaj7 on the
  -- tonic, FullyDim on the leading tone, and the augmented-major-7th (AugMaj7)
  -- on ♭III — which the enum now names fully. The ♭III *triad* is still Aug.
  log ""
  log "  Harmonic minor (exotic qualities incl. AugMaj7 on ♭III):"
  expectMode "HarmonicMinor" false [ Min, Dim, Aug, Min, Maj, Maj, Dim ] HarmonicMinor
  expectMode "HarmonicMinor" true  [ MinMaj7, HalfDim, AugMaj7, Min7, Dom7, Maj7, FullyDim ] HarmonicMinor

  log ""
  log "  Melodic minor:"
  expectMode "MelodicMinor" false [ Min, Min, Aug, Maj, Maj, Dim, Dim ] MelodicMinor
  expectMode "MelodicMinor" true  [ MinMaj7, Min7, AugMaj7, Dom7, Dom7, HalfDim, HalfDim ] MelodicMinor

  -- ------------------------------------------------------------------
  -- Rotation law — the church modes ARE rotations of each other, so the
  -- table above cannot be internally inconsistent. diatonicQuality of mode
  -- k's degree d must equal Ionian's degree (d + k).  Checks the CODE, not
  -- the hand table.
  -- ------------------------------------------------------------------
  log ""
  log "  Rotation law (church modes are rotations of the major scale):"
  let churchModes = [ Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian ]
  for_ (zip churchModes [ 0, 1, 2, 3, 4, 5, 6 ]) \(Tuple m offset) ->
    for_ (zip numerals [ 0, 1, 2, 3, 4, 5, 6 ]) \(Tuple n idx) ->
      assertEqual' (show m <> " degree " <> show n <> " seventh = Ionian rotated")
        { actual: diatonicQuality m n true
        , expected: diatonicQuality Ionian (numeralAt ((idx + offset) `mod` 7)) true
        }

  -- ------------------------------------------------------------------
  -- Completeness — every named mode's diatonic seventh resolves to a real
  -- seventh chord (never the triad fallback). This is the guarantee AugMaj7
  -- closed: before it, ♭III of the harmonic/melodic-minor families fell back
  -- to an Aug triad and this failed. The fallback now fires only for `Custom`.
  -- ------------------------------------------------------------------
  log ""
  log "  Completeness (every named mode's diatonic 7th is a true seventh chord):"
  for_ allNamedModes \m ->
    for_ numerals \n ->
      assert' (show m <> " degree " <> show n <> " seventh is a real 7th (no triad fallback)")
        (isSeventh (diatonicQuality m n true))

  log ""

-- | The numeral at a raw 0..6 index — the mirror of `numeralIndex`, so the
-- | rotation law can name a rotated degree.
numeralAt :: Int -> Numeral
numeralAt = case _ of
  0 -> I
  1 -> II
  2 -> III
  3 -> IV
  4 -> V
  5 -> VI
  _ -> VII

-- Helpers ------------------------------------------------------------------

expectGrade :: String -> Grade -> Anchor -> Effect Unit
expectGrade label expected anchor =
  assertEqual' label { actual: gradeAnchor anchor, expected }

-- | Assert a mode's whole diatonic quality row (triads if `seventh` is false).
expectMode :: String -> Boolean -> Array Quality -> Mode -> Effect Unit
expectMode name seventh expected mode =
  for_ (zip (zip numerals expected) [ 0, 1, 2, 3, 4, 5, 6 ]) \(Tuple (Tuple n want) _) ->
    assertEqual' (name <> " " <> (if seventh then "7th" else "triad") <> " on " <> show n)
      { actual: diatonicQuality mode n seventh, expected: want }

subset :: Array Op -> Array Op -> Boolean
subset small big = all (\x -> elem x big) small
