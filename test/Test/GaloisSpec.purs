-- | The quantiser's policy family under the order-theory lens: `snapDown` and
-- | `snapUp` are the two GALOIS ADJOINTS of a pitch-set's inclusion into all
-- | pitches, and one tested biconditional per side replaces a handful of
-- | hand-maintained companion laws (membership, fixed points, bracketing all
-- | FOLLOW from the adjunction; they are re-checked here only as diagnostics).
-- |
-- | `quantiseNearest` deliberately sits outside the law: it is the compromise
-- | BETWEEN the adjoints, which is why it (and only it) carries a tie-break
-- | policy. The one theorem it does obey — it always agrees with one of the
-- | two adjoints — is tested last: the lawless member is bounded by the
-- | lawful ones.
-- |
-- | Periodic sets only for the adjunction proper: a periodic tiling is total
-- | (every pitch has members on both sides), while a finite set clamps at its
-- | edges — the adjoint is partial there, pinned by goldens instead.
module Test.GaloisSpec
  ( runGaloisTests
  ) where

import Prelude

import Data.Array (sort)
import Data.Array.NonEmpty (cons')
import Data.Newtype (over)
import Data.Order.Galois (GaloisConnection(..), kernelOf)
import Data.Order.Laws (galoisAdjunction, galoisCounit, galoisUnit)
import Data.Order.Semilattice (Ordered(..), leq)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual)
import Test.QuickCheck (class Testable, quickCheck')
import Test.QuickCheck.Gen (Gen, chooseInt, elements, vectorOf)

import Harmonia.PitchSet (PitchSet, chord, finite, isMember, periodic, realize)
import Harmonia.Quantise (quantiseNearest, snapDown, snapUp)

-- | Inclusion of a set's members into all pitches, with `snapDown` as its
-- | right adjoint: `m <= n ⟺ m <= snapDown n`. The `Ordered Int` on the
-- | left is understood as ranging over MEMBERS (the generators enforce it) —
-- | the member sub-order is the restriction of the pitch order.
downConnection :: PitchSet -> GaloisConnection (Ordered Int) (Ordered Int)
downConnection ps = GaloisConnection
  { lower: identity
  , upper: over Ordered (snapDown ps)
  }

-- | The mirror: `snapUp` as the LEFT adjoint, `snapUp n <= m ⟺ n <= m`.
upConnection :: PitchSet -> GaloisConnection (Ordered Int) (Ordered Int)
upConnection ps = GaloisConnection
  { lower: over Ordered (snapUp ps)
  , upper: identity
  }

runGaloisTests :: Effect Unit
runGaloisTests = do
  log ""
  log "--- Harmonia — snapDown/snapUp as Galois adjoints (order-theory) ---"

  check "snapDown: the adjunction — (m <= n) == (m <= snapDown n)" \_ ->
    forPeriodic \ps -> forMember ps \m -> forNote \n ->
      galoisAdjunction (downConnection ps) (Ordered m) (Ordered n)

  check "snapUp: the adjunction — (snapUp n <= m) == (n <= m)" \_ ->
    forPeriodic \ps -> forMember ps \m -> forNote \n ->
      galoisAdjunction (upConnection ps) (Ordered n) (Ordered m)

  check "units/counits: snapDown n <= n <= snapUp n" \_ ->
    forPeriodic \ps -> forNote \n ->
      galoisCounit (downConnection ps) (Ordered n)
        && galoisUnit (upConnection ps) (Ordered n)

  -- Free theorems of the adjunctions, re-tested as diagnostics:
  check "membership — both snaps land in the set" \_ ->
    forPeriodic \ps -> forNote \n ->
      isMember ps (snapDown ps n) && isMember ps (snapUp ps n)

  check "fixed points — a member snaps to itself, both directions" \_ ->
    forPeriodic \ps -> forMember ps \m ->
      snapDown ps m == m && snapUp ps m == m

  check "kernel idempotence — snapping a snapped note is a no-op" \_ ->
    forPeriodic \ps -> forNote \n ->
      let k = kernelOf (downConnection ps)
      in k (k (Ordered n)) == k (Ordered n)

  check "the lawless member is bracketed — nearest is snapDown or snapUp" \_ ->
    forPeriodic \ps -> forNote \n ->
      let q = quantiseNearest ps n
      in (q == snapDown ps n || q == snapUp ps n)
        && leq (Ordered (snapDown ps n)) (Ordered q)
        && leq (Ordered q) (Ordered (snapUp ps n))

  log "  goldens: C-major triad and a clamped finite set"
  let cMaj = chord [ 0, 4, 7 ]
  assertEqual { actual: snapDown cMaj 66, expected: 64 } -- F# floors to E
  assertEqual { actual: snapUp cMaj 66, expected: 67 } -- F# ceils to G
  assertEqual { actual: quantiseNearest cMaj 66, expected: 67 } -- nearest picks a side
  let fin = finite 60 [ 0, 4, 7, 12 ]
  assertEqual { actual: snapDown fin 61, expected: 60 }
  assertEqual { actual: snapUp fin 61, expected: 64 }
  assertEqual { actual: snapDown fin 58, expected: 60 } -- CLAMP below the floor
  assertEqual { actual: snapUp fin 80, expected: 72 } -- CLAMP above the top
  log ""

-- Helpers (shared shape with QuantiseSpec) ------------------------------------

check :: forall p. Testable p => String -> (Unit -> p) -> Effect Unit
check label p = do
  log ("  • " <> label)
  quickCheck' 400 (p unit)

forPeriodic :: forall p. Testable p => (PitchSet -> p) -> Gen p
forPeriodic f = f <$> genPeriodicSet

forMember :: forall p. Testable p => PitchSet -> (Int -> p) -> Gen p
forMember ps f = f <<< realize ps <$> chooseInt (-20) 20

forNote :: forall p. Testable p => (Int -> p) -> Gen p
forNote f = f <$> chooseInt 24 96

genPeriodicSet :: Gen PitchSet
genPeriodicSet = do
  p <- elements (cons' 12 [ 7, 19, 24 ])
  r <- chooseInt 48 72
  k <- chooseInt 1 p
  offs <- vectorOf k (chooseInt 0 (p - 1))
  pure (periodic r (sort offs) p)
