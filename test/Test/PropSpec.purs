-- | Property-based tests for `Harmonia.Anchor` / `Harmonia.Graded`.
-- |
-- | The example tables prove specific chords; these prove the *laws* over
-- | randomly generated anchors and phrases. The centrepiece is the
-- | **capability↔behaviour** law: `permitted` is a declarative table, and it
-- | must never lie about what the operations actually do — an operation applies
-- | cleanly (no blame) exactly when the grade permits it. If the table and the
-- | operations ever drift apart, these fail.
module Test.PropSpec
  ( runPropTests
  ) where

import Prelude

import Data.Array (all, elem, mapMaybe, mapWithIndex, null, sort)
import Data.Array.NonEmpty (cons')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (class Testable, quickCheck')
import Test.QuickCheck.Gen (Gen, chooseInt, elements, oneOf, vectorOf)

import Harmonia.Chord
  ( Chord(..), DegreeChord(..), Key, Mode(..), Numeral(..), Quality(..), Tension(..)
  , realize
  )
import Harmonia.Anchor (Anchor(..), Grade(..), Op(..), gradeAnchor, permits)
import Harmonia.Graded (Phrase(..), blame, grade, modulate, reflavour, transpose, value)

runPropTests :: Effect Unit
runPropTests = do
  log ""
  log "--- Harmonia — property laws (QuickCheck) ---"

  -- === The capability↔behaviour law ======================================
  check "reflavour applies cleanly ⟺ grade permits Reflavour" \_ ->
    forA \a -> forM \m ->
      null (blame (reflavour m a)) == permits Reflavour (gradeAnchor a)

  check "modulate applies cleanly ⟺ grade permits Modulate" \_ ->
    forA \a -> forT \t ->
      null (blame (modulate t a)) == permits Modulate (gradeAnchor a)

  -- === Transpose is a lawful action of the integers (mod 12) =============
  check "transpose 0 is identity (on pitch classes)" \_ ->
    forA \a -> pcsOf (transpose 0 a) == pcsOf a

  check "transpose 12 is identity (mod 12)" \_ ->
    forA \a -> pcsOf (transpose 12 a) == pcsOf a

  check "transpose m ∘ transpose n = transpose (m+n)" \_ ->
    forA \a -> forI \m -> forI \n ->
      pcsOf (transpose m (transpose n a)) == pcsOf (transpose (m + n) a)

  -- transpose agrees with realize: shifting the anchor == shifting its pitches.
  check "transpose agrees with realize (shift then realize = realize then shift)" \_ ->
    forA \a -> forI \n ->
      pcsOf (transpose n a) == map (\ps -> sort (map (shift n) ps)) (pcsOf a)

  -- === Grade is preserved by the tonic-moving operations =================
  check "transpose preserves grade" \_ ->
    forA \a -> forI \n -> gradeAnchor (transpose n a) == gradeAnchor a

  check "modulate preserves grade" \_ ->
    forA \a -> forT \t -> gradeAnchor (value (modulate t a)) == gradeAnchor a

  -- === Reflavour closure =================================================
  check "reflavour of a Diatonic chord stays Diatonic" \_ ->
    forA \a -> forM \m ->
      (gradeAnchor a == Diatonic) `implies` (gradeAnchor (value (reflavour m a)) == Diatonic)

  check "a clean reflavour result is always Diatonic" \_ ->
    forA \a -> forM \m ->
      null (blame (reflavour m a)) `implies` (gradeAnchor (value (reflavour m a)) == Diatonic)

  -- === Phrase grade is the meet ==========================================
  check "phrase grade ≤ every member's grade" \_ ->
    forP \ph -> all (\a -> grade ph <= gradeAnchor a) (members ph)

  check "phrase grade is achieved by some member (or empty)" \_ ->
    forP \ph ->
      let ms = members ph
      in null ms || elem (grade ph) (map gradeAnchor ms)

  -- === Phrase reflavour blames exactly the non-Diatonic members ==========
  check "phrase reflavour blames exactly (and only) the non-Diatonic members" \_ ->
    forP \ph -> forM \m ->
      sort (map _.index (blame (reflavour m ph)))
        == nonDiatonicIndices (members ph)

  -- === Graded functor laws ===============================================
  check "Graded functor identity: map identity r = r" \_ ->
    forA \a -> forM \m -> map identity (reflavour m a) == reflavour m a

  check "Graded functor composition: map (f∘g) = map f ∘ map g" \_ ->
    forA \a -> forM \m ->
      let r = reflavour m a
      in map (transpose 1 <<< transpose 2) r
           == (map (transpose 1) <<< map (transpose 2)) r

  log ""

-- Runner -------------------------------------------------------------------

-- | Run a property 400 times. The `\_ ->` lets each `check` share one clean
-- | call shape while re-seeding the generators per invocation.
check :: forall p. Testable p => String -> (Unit -> p) -> Effect Unit
check label p = do
  log ("  • " <> label)
  quickCheck' 400 (p unit)

-- forAll wrappers, one per generator, to keep the properties readable.
forA :: forall p. Testable p => (Anchor -> p) -> Gen p
forA f = f <$> genAnchor

forP :: forall p. Testable p => (Phrase -> p) -> Gen p
forP f = f <$> genPhrase

forM :: forall p. Testable p => (Mode -> p) -> Gen p
forM f = f <$> genMode

forT :: forall p. Testable p => (Int -> p) -> Gen p
forT f = f <$> chooseInt 0 11

forI :: forall p. Testable p => (Int -> p) -> Gen p
forI f = f <$> chooseInt (-24) 24

-- Helpers ------------------------------------------------------------------

implies :: Boolean -> Boolean -> Boolean
implies p q = not p || q

members :: Phrase -> Array Anchor
members (Phrase as) = as

shift :: Int -> Int -> Int
shift n p = ((p + n) `mod` 12 + 12) `mod` 12

pcsOf :: Anchor -> Maybe (Array Int)
pcsOf = case _ of
  Located key dc -> case realize key dc of Chord pcs -> Just (sort pcs)
  Free           -> Nothing

nonDiatonicIndices :: Array Anchor -> Array Int
nonDiatonicIndices as =
  mapMaybe (\(Tuple i a) -> if gradeAnchor a == Diatonic then Nothing else Just i)
    (mapWithIndex Tuple as)

-- Generators ---------------------------------------------------------------

genMode :: Gen Mode
genMode = elements (cons' Ionian
  [ Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian
  , HarmonicMinor, MelodicMinor
  , LocrianNat6, IonianSharp5, DorianSharp4, PhrygianDominant, LydianSharp2, Ultralocrian
  , DorianFlat2, LydianAugmented, LydianDominant, MixolydianFlat6, LocrianNat2, Altered
  ])

genNumeral :: Gen Numeral
genNumeral = elements (cons' I [ II, III, IV, V, VI, VII ])

genQuality :: Gen Quality
genQuality = elements (cons' Maj
  [ Min, Dim, Aug, Maj7, Min7, Dom7, HalfDim, FullyDim, MinMaj7, AugMaj7 ])

genDeg :: Gen Int
genDeg = elements (cons' 2 [ 3, 4, 5, 6, 7, 9, 11, 13 ])

genTension :: Gen Tension
genTension = oneOf (cons'
  (Add <$> genDeg)
  [ Sharp <$> genDeg
  , Flat <$> genDeg
  , Sus <$> elements (cons' 2 [ 4 ])
  , pure NoFifth
  , pure NoThird
  ])

-- Mostly Nothing, so plenty of generated chords are genuinely diatonic.
genMaybe :: forall a. Gen a -> Gen (Maybe a)
genMaybe g = do
  b <- chooseInt 0 2
  if b == 0 then Just <$> g else pure Nothing

genKey :: Gen Key
genKey = do
  tonic <- chooseInt 0 11
  mode <- genMode
  pure { tonic, mode }

genDegreeChord :: Gen DegreeChord
genDegreeChord = do
  numeral <- genNumeral
  quality <- genQuality
  nt <- chooseInt 0 3
  tensions <- vectorOf nt genTension
  slash <- genMaybe genNumeral
  mode <- genMaybe genMode
  pure (DegreeChord { numeral, quality, tensions, slash, mode })

genAnchor :: Gen Anchor
genAnchor = do
  b <- chooseInt 0 4
  if b == 0 then pure Free else Located <$> genKey <*> genDegreeChord

genPhrase :: Gen Phrase
genPhrase = do
  n <- chooseInt 0 5
  Phrase <$> vectorOf n genAnchor
