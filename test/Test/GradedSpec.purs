-- | Tests for `Harmonia.Graded` — the grade-gated operations, verified through
-- | the *realized pitch classes*, so the harmony is checked and not merely the
-- | quality label. Covers transpose / modulate / reflavour on a single anchor,
-- | the `Graded` result plumbing, and the fractal `Phrase` instances.
module Test.GradedSpec
  ( runGradedTests
  ) where

import Prelude

import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert', assertEqual')

import Harmonia.Chord
  ( Chord(..), Mode(..), Numeral(..), Quality(..)
  , borrow, cMajorKey, deg, realize
  )
import Harmonia.Anchor (Anchor(..), Grade(..), gradeAnchor)
import Harmonia.Graded
  ( BlameReason(..), Graded, Phrase(..)
  , blame, clean, grade, modulate, reflavour, transpose, value
  )

runGradedTests :: Effect Unit
runGradedTests = do
  log ""
  log "--- Harmonia.Graded — transpose ---"

  let iMajC     = Located cMajorKey (deg I   Maj  [])
      iMaj7C    = Located cMajorKey (deg I   Maj7 [])
      ivMaj7C   = Located cMajorKey (deg IV  Maj7 [])
      vDom7C    = Located cMajorKey (deg V   Dom7 [])
      iiMinC    = Located cMajorKey (deg II  Min  [])
      borrowedVI = Located cMajorKey (borrow Aeolian (deg VI Maj []))

  -- transpose moves the tonic; the realized pitches shift by n (mod 12).
  expectPcs "transpose 2 of I/C = I/D (D F# A)" (Just [ 2, 6, 9 ]) (transpose 2 iMajC)
  expectPcs "transpose 0 is identity"           (pcsOf iMajC)      (transpose 0 iMajC)
  expectPcs "transpose 12 is identity (mod 12)" (pcsOf iMajC)      (transpose 12 iMajC)
  expectPcs "transpose -1 of I/C = I/B (B D# F#)" (Just [ 3, 6, 11 ]) (transpose (-1) iMajC)
  -- transpose preserves grade (the whole key moves; the reading is intact).
  assert' "transpose preserves grade (Diatonic stays Diatonic)"
    (gradeAnchor (transpose 5 iMajC) == Diatonic)
  assert' "transpose preserves grade (Keyed stays Keyed)"
    (gradeAnchor (transpose 5 borrowedVI) == Keyed)
  -- Free has no reading to move.
  assertEqual' "transpose n Free = Free" { actual: transpose 7 Free, expected: Free }

  log ""
  log "--- Harmonia.Graded — modulate ---"

  -- Modulate re-tonics a located chord; applies cleanly (blame []).
  let modG = modulate 7 iMajC
  expectBlame "modulate a located chord: no blame" [] modG
  expectPcs "modulate to G: I/G = G B D" (Just [ 2, 7, 11 ]) (value modG)
  assert' "modulate preserves grade" (gradeAnchor (value modG) == Diatonic)

  -- Modulate on Free cannot apply: NoReading blame, value unchanged.
  let modFree = modulate 7 Free
  expectBlame "modulate Free → NoReading blame" [ Tuple 0 NoReading ] modFree
  assertEqual' "modulate Free leaves value = Free" { actual: value modFree, expected: Free }

  log ""
  log "--- Harmonia.Graded — reflavour ---"

  -- Reflavour re-reads the same degree in a new mode and re-derives quality.
  let refA = reflavour Aeolian iMaj7C
  expectBlame "reflavour a diatonic chord: no blame" [] refA
  expectPcs "I maj7 reflavoured to Aeolian = i m7 (C Eb G Bb)" (Just [ 0, 3, 7, 10 ]) (value refA)

  -- IV maj7 into Dorian becomes IV7 (F A C Eb).
  expectPcs "IV maj7 reflavoured to Dorian = IV7 (F A C Eb)"
    (Just [ 0, 3, 5, 9 ]) (value (reflavour Dorian ivMaj7C))

  -- Reflavouring to the SAME mode leaves the chord where it was.
  expectPcs "I maj7 reflavoured to Ionian = itself" (pcsOf iMaj7C) (value (reflavour Ionian iMaj7C))

  -- Closure: reflavouring a Diatonic chord yields a Diatonic chord.
  assert' "reflavour of a Diatonic chord is still Diatonic"
    (gradeAnchor (value (reflavour Dorian iMaj7C)) == Diatonic)

  -- Round-trip through Aeolian and back restores the pitches.
  expectPcs "reflavour Aeolian then Ionian round-trips I maj7"
    (pcsOf iMaj7C) (value (reflavour Ionian (value (reflavour Aeolian iMaj7C))))

  -- A Keyed (borrowed) chord cannot be reflavoured: Borrowed blame, passed through.
  let refKeyed = reflavour Lydian borrowedVI
  expectBlame "reflavour a borrowed chord → Borrowed blame" [ Tuple 0 Borrowed ] refKeyed
  assertEqual' "reflavour leaves the borrowed chord unchanged"
    { actual: value refKeyed, expected: borrowedVI }

  -- Free cannot be reflavoured either.
  expectBlame "reflavour Free → NoReading blame" [ Tuple 0 NoReading ] (reflavour Dorian Free)

  log ""
  log "--- Harmonia.Graded — Graded plumbing ---"

  assertEqual' "value (clean x) = x" { actual: value (clean 42), expected: 42 }
  assert' "blame (clean x) = []" (blame (clean 42) == [])
  -- Functor identity law on a real result.
  assert' "map identity g = g (functor identity)"
    (map identity modG == modG)
  -- Functor maps the value, keeps the blame.
  assertEqual' "map over blamed result maps value, keeps blame"
    { actual: Tuple (value (map (_ + 1) (clean 10))) (blame (map (_ + 1) (clean 10)))
    , expected: Tuple 11 [] }

  log ""
  log "--- Harmonia.Graded — Phrase (the fractal) ---"

  -- Emergent grade = the meet of member grades.
  assert' "phrase [Diatonic, Diatonic] → Diatonic"
    (grade (Phrase [ iMajC, iiMinC ]) == Diatonic)
  assert' "phrase [Diatonic, Keyed] → Keyed (meet)"
    (grade (Phrase [ iMajC, borrowedVI ]) == Keyed)
  assert' "phrase [Diatonic, Free] → ShiftOnly (meet)"
    (grade (Phrase [ iMajC, Free ]) == ShiftOnly)
  assert' "empty phrase → Diatonic (vacuous meet)"
    (grade (Phrase []) == Diatonic)

  -- Reflavour a heterogeneous phrase: the located spine follows, the one
  -- foreign chord is blamed AT ITS INDEX and passed through untouched.
  let ph  = Phrase [ iMaj7C, borrowedVI, vDom7C ]
      rPh = reflavour Dorian ph
  expectBlame "phrase reflavour blames only the foreign chord, at index 1"
    [ Tuple 1 Borrowed ] rPh
  case value rPh of
    Phrase [ a0, a1, a2 ] -> do
      assert' "phrase member 0 reflavoured, still Diatonic" (gradeAnchor a0 == Diatonic)
      assertEqual' "phrase member 1 (foreign) passed through unchanged"
        { actual: a1, expected: borrowedVI }
      assert' "phrase member 2 reflavoured, still Diatonic" (gradeAnchor a2 == Diatonic)
    _ -> assert' "phrase should still have 3 members" false

  -- Transpose distributes over a phrase.
  case transpose 2 (Phrase [ iMajC ]) of
    Phrase [ a0 ] -> expectPcs "transpose 2 over a phrase member" (Just [ 2, 6, 9 ]) a0
    _ -> assert' "transposed phrase keeps its shape" false

  log ""

-- Helpers ------------------------------------------------------------------

-- | The realized, sorted pitch classes of an anchor (Nothing for Free).
pcsOf :: Anchor -> Maybe (Array Int)
pcsOf = case _ of
  Located key dc -> case realize key dc of Chord pcs -> Just (sort pcs)
  Free           -> Nothing

expectPcs :: String -> Maybe (Array Int) -> Anchor -> Effect Unit
expectPcs label expected anchor =
  assertEqual' label { actual: pcsOf anchor, expected }

-- | A showable, comparable signature of a result's blame.
blameSig :: forall a. Graded a -> Array (Tuple Int BlameReason)
blameSig g = map (\b -> Tuple b.index b.reason) (blame g)

expectBlame :: forall a. String -> Array (Tuple Int BlameReason) -> Graded a -> Effect Unit
expectBlame label expected g =
  assertEqual' label { actual: blameSig g, expected }
