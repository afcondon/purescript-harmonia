-- | Tests for `Harmonia.PitchSet` + `Harmonia.Quantise` — and, together, the
-- | DEMONSTRATION suite for the quantiser, after the Instruo Dáil (Jason Lim).
-- |
-- | Three orthogonal axes are exercised:
-- |
-- |   * AXIS 1 — the set's period: PURE (`chord`, every octave identical, all D's
-- |     are the 9th) vs EXTENDED (`extendedChord`, a multi-octave span so a 9th an
-- |     octave up is a member but a low D is not).
-- |   * AXIS 2 — the quantiser mode: NEAREST-INTERVAL (`quantiseNearest`, snap a
-- |     pitch by true pitch distance) vs EQUAL-SPACING (`quantiseEqual`, divide the
-- |     input range into equal slots per degree).
-- |   * (AXIS 3 — voice-spread across the set — lives in the Odonus layer, not the
-- |     theory; not modelled here.)
-- |
-- | Laws (QuickCheck) pin the arithmetic; hand-verified goldens + logged sweeps
-- | make the behaviour visible.
module Test.QuantiseSpec
  ( runQuantiseTests
  ) where

import Prelude

import Data.Array (range, sort)
import Data.Array.NonEmpty (cons')
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual)
import Test.QuickCheck (class Testable, quickCheck')
import Test.QuickCheck.Gen (Gen, chooseInt, elements, oneOf, vectorOf)

import Harmonia.PitchSet (PitchSet, chord, extendedChord, finite, isMember, period, periodic, realize, scale)
import Harmonia.Quantise (quantiseEqual, quantiseNearest)

runQuantiseTests :: Effect Unit
runQuantiseTests = do
  log ""
  log "--- Harmonia — quantiser examples (golden) ---"

  -- AXIS 1 PURE + AXIS 2 NEAREST. A C-major triad tiling every octave. The
  -- register follows the input: each chromatic step snaps to its nearest tone.
  let cMaj = chord [ 0, 4, 7 ]
  assertEqual { actual: quantiseNearest cMaj 60, expected: 60 } -- C is a member
  assertEqual { actual: quantiseNearest cMaj 61, expected: 60 } -- C# → C
  assertEqual { actual: quantiseNearest cMaj 63, expected: 64 } -- D# → E
  assertEqual { actual: quantiseNearest cMaj 66, expected: 67 } -- F# → G
  assertEqual { actual: quantiseNearest cMaj 73, expected: 72 } -- C#5 → C5 (same in every octave)
  log ("  pure C-major triad, chromatic 60..72 → " <> show (sweep cMaj 60 72))

  -- AXIS 1 EXTENDED. C-E-G with the 9th (D) an octave up: offsets [0,4,7,14] from
  -- root 60, so the span is two octaves (period 24). The 9th is honoured only where
  -- it sits — the D an octave up snaps to the 9th; a D in the home octave does NOT
  -- (it snaps to a nearer chord tone). This is the octave-dependent colour.
  let ext = extendedChord 60 [ 0, 4, 7, 14 ] 2
  assertEqual { actual: quantiseNearest ext 74, expected: 74 }   -- D5 IS the 9th (a member)
  assert (mod (quantiseNearest ext 62) 12 /= 2)                  -- D4 does NOT snap to any D
  log ("  extended C-add9 (9th up an 8ve), chromatic 60..74 → " <> show (sweep ext 60 74))

  -- AXIS 2 CONTRAST — nearest vs equal, over an UNEVEN scale (C pentatonic, gaps
  -- 2-2-3-2-3). Nearest: a chromatic input snaps by pitch distance (the 3-semitone
  -- gaps capture more input). Equal: a linear input divides EQUALLY across the five
  -- degrees — each degree owns 20 units of a 0..99 input regardless of pitch gap.
  let penta = scale 60 [ 0, 2, 4, 7, 9 ]
  assertEqual { actual: quantiseEqual penta 1 99 0, expected: 60 }  -- slot 0 → C
  assertEqual { actual: quantiseEqual penta 1 99 40, expected: 64 } -- slot 2 → E
  assertEqual { actual: quantiseEqual penta 1 99 99, expected: 69 } -- slot 4 → A
  log ("  pentatonic NEAREST, chromatic 60..72   → " <> show (sweep penta 60 72))
  log ("  pentatonic EQUAL,   input 0,20,40,60,80,99 → "
        <> show (map (quantiseEqual penta 1 99) [ 0, 20, 40, 60, 80, 99 ]))

  -- A finite set (no tiling): snap to the nearest literal member; clamp past the top.
  let fin = finite 60 [ 0, 4, 7, 12 ]
  assertEqual { actual: quantiseNearest fin 61, expected: 60 }
  assertEqual { actual: quantiseNearest fin 80, expected: 72 }
  log ("  finite {60,64,67,72}, input 58..78 → " <> show (sweep fin 58 78))

  log ""
  log "--- Harmonia — quantiser laws (QuickCheck) ---"

  check "nearest: idempotence — q (q n) == q n" \_ ->
    forSet \ps -> forNote \n ->
      quantiseNearest ps (quantiseNearest ps n) == quantiseNearest ps n

  check "nearest: membership — q n is always a member of the set" \_ ->
    forSet \ps -> forNote \n ->
      isMember ps (quantiseNearest ps n)

  check "nearest: a member is a fixed point — q (realize ps i) == realize ps i" \_ ->
    forSet \ps -> forIdx \i ->
      quantiseNearest ps (realize ps i) == realize ps i

  check "nearest: locality — the register follows the note (within half a period)" \_ ->
    forPeriodic \ps -> forNote \n ->
      2 * abs (quantiseNearest ps n - n) <= fromMaybe 0 (period ps)

  check "nearest: octave-equivariance — q (n + period) == q n + period" \_ ->
    forPeriodic \ps -> forNote \n ->
      let p = fromMaybe 0 (period ps)
      in quantiseNearest ps (n + p) == quantiseNearest ps n + p

  check "equal: monotone (nondecreasing) in the input" \_ ->
    forPeriodic \ps -> forSpan \sp -> forVal \a -> forVal \b ->
      quantiseEqual ps sp 127 (min a b) <= quantiseEqual ps sp 127 (max a b)

  check "equal: always lands on a set member" \_ ->
    forPeriodic \ps -> forSpan \sp -> forVal \v ->
      isMember ps (quantiseEqual ps sp 127 v)

  log ""

-- Helpers --------------------------------------------------------------------

sweep :: PitchSet -> Int -> Int -> Array Int
sweep ps lo hi = map (quantiseNearest ps) (range lo hi)

-- | Run a property 400 times, sharing one clean call shape (per PropSpec).
check :: forall p. Testable p => String -> (Unit -> p) -> Effect Unit
check label p = do
  log ("  • " <> label)
  quickCheck' 400 (p unit)

-- forAll wrappers, one per generator, to keep the properties readable.
forSet :: forall p. Testable p => (PitchSet -> p) -> Gen p
forSet f = f <$> genAnySet

forPeriodic :: forall p. Testable p => (PitchSet -> p) -> Gen p
forPeriodic f = f <$> genPeriodicSet

forNote :: forall p. Testable p => (Int -> p) -> Gen p
forNote f = f <$> chooseInt 24 96

forIdx :: forall p. Testable p => (Int -> p) -> Gen p
forIdx f = f <$> chooseInt (-20) 20

forSpan :: forall p. Testable p => (Int -> p) -> Gen p
forSpan f = f <$> chooseInt 1 3

forVal :: forall p. Testable p => (Int -> p) -> Gen p
forVal f = f <$> chooseInt 0 127

-- Generators -----------------------------------------------------------------

genAnySet :: Gen PitchSet
genAnySet = oneOf (cons' genPeriodicSet [ genFiniteSet ])

-- Periodic sets over musical + exotic periods. Offsets ASCENDING (the type's
-- invariant, and what the equal-spacing monotone law needs) and residues in
-- `[0, p)`.
genPeriodicSet :: Gen PitchSet
genPeriodicSet = do
  p <- elements (cons' 12 [ 7, 19, 24 ])
  r <- chooseInt 48 72
  k <- chooseInt 1 p
  offs <- vectorOf k (chooseInt 0 (p - 1))
  pure (periodic r (sort offs) p)

-- Finite sets: a handful of ascending literal offsets, no tiling.
genFiniteSet :: Gen PitchSet
genFiniteSet = do
  r <- chooseInt 48 72
  k <- chooseInt 1 6
  offs <- vectorOf k (chooseInt 0 36)
  pure (finite r (sort offs))
