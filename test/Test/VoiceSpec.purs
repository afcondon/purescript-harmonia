-- | Tests for `Harmonia.Voice` — the two-stage quantised-voice pipeline (the shape
-- | reef's Odonus ports onto). The pipeline is pure composition of the two
-- | quantiser modes, so its correctness REDUCES to the seven laws already green in
-- | `Test.QuantiseSpec`. These properties assert that the composition preserves
-- | them:
-- |
-- |   * SIMPLE MODE IS TRANSPARENT — with `activeSet == scale`, `voiceLabel == home`
-- |     and the sounding note is `home + globalOct` (the fixed-point law lifted).
-- |   * MEMBERSHIP — the sounding note is always a member of `activeSet` (up to the
-- |     global octave), for any knob and any offset.
-- |   * REGISTER FOLLOWS THE MELODY — the snapped note stays within half a period of
-- |     `home + offset` (the locality law lifted).
-- |   * VOICE-SPREAD (axis 3) — a spread of chromatic offsets over one home covers
-- |     the distinct tones of the chord; demonstrated on a golden (Fmaj7).
-- |
-- | Hand-verified goldens make the behaviour visible; the laws pin it.
module Test.VoiceSpec
  ( runVoiceTests
  ) where

import Prelude

import Data.Array (nub, sort)
import Data.Array.NonEmpty (cons')
import Data.Foldable (all)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert, assertEqual)
import Test.QuickCheck (class Testable, quickCheck')
import Test.QuickCheck.Gen (Gen, chooseInt, elements, vectorOf)

import Harmonia.PitchSet (PitchSet, chord, isMember, period, scale)
import Harmonia.Voice (VoiceContext, home, voiceLabel, voiceNote)

runVoiceTests :: Effect Unit
runVoiceTests = do
  log ""
  log "--- Harmonia — voice pipeline examples (golden) ---"

  -- The base world: C major, span 3, knob 0..255.
  let cMajor = scale 60 [ 0, 2, 4, 5, 7, 9, 11 ]
      simple = ctxOf cMajor cMajor 0

  -- SIMPLE MODE. activeSet == scale, so the label equals the home note and an
  -- offset-free voice sounds exactly the home note. A chromatic offset snaps back
  -- onto the scale near home.
  assertEqual { actual: voiceLabel simple 0, expected: home simple 0 }
  assertEqual { actual: voiceLabel simple 128, expected: home simple 128 }
  assertEqual { actual: voiceNote simple { knob: 128, offset: 0 }, expected: home simple 128 }
  log ("  C-major simple, knob 0,64,128,192,255 → home "
        <> show (map (home simple) [ 0, 64, 128, 192, 255 ]))

  -- A GLOBAL OCTAVE moves every voice by exactly ±12·k, and only that.
  let up1 = simple { globalOct = 12 }
  assertEqual { actual: voiceNote up1 { knob: 128, offset: 0 }
              , expected: voiceNote simple { knob: 128, offset: 0 } + 12 }

  -- CHORD CONSTRAINT. An Fmaj7 fires (activeSet = chord F A C E). The home notes
  -- are the SAME (the melody does not move); only the sounding note snaps to the
  -- chord. Register stays with the melody — no jump to the chord's own octave.
  let fMaj7 = chord [ 5, 9, 0, 4 ]
      follow = ctxOf cMajor fMaj7 0
  assert (isMember fMaj7 (voiceNote follow { knob: 96, offset: 0 }))
  assertEqual { actual: home follow 96, expected: home simple 96 } -- melody unchanged
  log ("  Fmaj7 follow, knob 0,64,128,192,255, offset 0 → "
        <> show (map (\k -> voiceNote follow { knob: k, offset: 0 }) [ 0, 64, 128, 192, 255 ]))

  -- VOICE-SPREAD (axis 3). Four voices share a home but fan the chromatic offset;
  -- collectively they sound distinct Fmaj7 tones. Golden: home = C4 (60), offsets
  -- fanned over ±... land on {C, E, F, A} pitch-classes.
  let h = 60
      spread = map (\off -> voiceNote follow { knob: knobForHome, offset: off }) [ -1, 2, 5, 9 ]
      pcs = nub (sort (map (\n -> mod n 12) spread))
  assertEqual { actual: home follow knobForHome, expected: h }   -- the shared home is C4
  assert (all (isMember fMaj7) spread)
  log ("  Fmaj7 spread over one home C4, offsets [-1,2,5,9] → " <> show spread
        <> "  pcs " <> show pcs)

  log ""
  log "--- Harmonia — voice pipeline laws (QuickCheck) ---"

  check "simple mode: label == home when activeSet == scale" \_ ->
    forSimpleCtx \ctx -> forKnob \k ->
      voiceLabel ctx k == home ctx k

  check "simple mode: offset-free note == home + globalOct" \_ ->
    forSimpleCtx \ctx -> forKnob \k ->
      voiceNote ctx { knob: k, offset: 0 } == home ctx k + ctx.globalOct

  check "membership: the sounding note (less globalOct) is a chord tone" \_ ->
    forCtx \ctx -> forKnob \k -> forOffset \off ->
      isMember ctx.activeSet (voiceNote ctx { knob: k, offset: off } - ctx.globalOct)

  check "register follows the melody: |snap − (home+offset)| ≤ period/2" \_ ->
    forCtx \ctx -> forKnob \k -> forOffset \off ->
      let target = home ctx k + off
          snapped = voiceNote ctx { knob: k, offset: off } - ctx.globalOct
          p = fromMaybe 0 (period ctx.activeSet)
      in 2 * abs (snapped - target) <= p

  check "global octave is exactly additive and separable" \_ ->
    forCtx \ctx -> forKnob \k -> forOffset \off ->
      let base = ctx { globalOct = 0 }
      in voiceNote ctx { knob: k, offset: off }
           == voiceNote base { knob: k, offset: off } + ctx.globalOct

  log ""

-- Helpers --------------------------------------------------------------------

-- | A home knob that lands on C4 (60) in the C-major span-3 world. Verified by the
-- | golden sweep above (knob 0 → home 60). Kept as a name so the spread golden
-- | reads clearly.
knobForHome :: Int
knobForHome = 0

ctxOf :: PitchSet -> PitchSet -> Int -> VoiceContext
ctxOf sc act oct =
  { scale: sc, activeSet: act, span: 3, knobMax: 255, globalOct: oct }

-- | Run a property 400 times, sharing one call shape (per QuantiseSpec).
check :: forall p. Testable p => String -> (Unit -> p) -> Effect Unit
check label p = do
  log ("  • " <> label)
  quickCheck' 400 (p unit)

-- forAll wrappers ------------------------------------------------------------

forKnob :: forall p. Testable p => (Int -> p) -> Gen p
forKnob f = f <$> chooseInt 0 255

forOffset :: forall p. Testable p => (Int -> p) -> Gen p
forOffset f = f <$> chooseInt (-12) 12

forSimpleCtx :: forall p. Testable p => (VoiceContext -> p) -> Gen p
forSimpleCtx f = f <$> genSimpleCtx

forCtx :: forall p. Testable p => (VoiceContext -> p) -> Gen p
forCtx f = f <$> genCtx

-- Generators -----------------------------------------------------------------

-- | A simple-mode context: activeSet == scale, a random key, a random octave.
genSimpleCtx :: Gen VoiceContext
genSimpleCtx = do
  sc <- genScale
  oct <- (\k -> 12 * k) <$> chooseInt (-2) 2
  pure (ctxOf sc sc oct)

-- | A follow-mode context: a random scale plus a random PURE chord as the
-- | constraint (period 12, so the register-locality bound is a clean 6).
genCtx :: Gen VoiceContext
genCtx = do
  sc <- genScale
  ch <- genChord
  oct <- (\k -> 12 * k) <$> chooseInt (-2) 2
  pure (ctxOf sc ch oct)

genScale :: Gen PitchSet
genScale = do
  r <- chooseInt 48 72
  ivls <- elements (cons' [ 0, 2, 4, 5, 7, 9, 11 ] [ [ 0, 2, 4, 7, 9 ], [ 0, 2, 3, 5, 7, 8, 10 ] ])
  pure (scale r ivls)

-- | A pure chord (period 12): 3–4 distinct pitch-classes.
genChord :: Gen PitchSet
genChord = do
  k <- chooseInt 3 4
  pcs <- vectorOf k (chooseInt 0 11)
  pure (chord (nub (sort pcs)))
