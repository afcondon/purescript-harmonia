-- | Quantisers: map an input onto a `PitchSet`. Two modes, after the quantisation
-- | options Jason Lim built into the Instruo Dáil (the design we drew Odonus from):
-- |
-- |   * `quantiseNearest :: PitchSet -> Note -> Note` — NEAREST-INTERVAL. The input
-- |     is a PITCH; it snaps to the nearest set member at its true pitch distance.
-- |     A note with a wide gap to its neighbours captures a wide band of input. The
-- |     REGISTER of the result follows the input note — the set chooses the pitch
-- |     class, never the octave — so a melodic line keeps its shape and range while
-- |     being coloured by a chord.
-- |
-- |   * `quantiseEqual :: PitchSet -> …` — EQUAL-SPACING. The input is a POSITION in
-- |     a range; the range divides into EQUAL slots, one per degree, regardless of
-- |     the degrees' pitch spacing. A smoothly rising LFO then steps through the set
-- |     at even intervals of INPUT even when the scale's pitch intervals are uneven.
-- |     (This is `realize ∘ equalIndex` — the flat Dáil mapping.)
-- |
-- | These pair with the two READINGS of the set itself (see `Harmonia.PitchSet`):
-- | pure (`period 12`, every octave identical) vs extended (a multi-octave period
-- | with literal offsets, so a 9th an octave up is a member but a low D is not).
-- | Nearest × {pure, extended} covers the musically interesting corners.
-- |
-- | For the nearest mode, "repeating periods across the octave" is handled by
-- | folding each offset by `period` before finding the nearest member — so a PURE
-- | chord with offset 26 matches any D, while an EXTENDED chord (period 24) matches
-- | only the D's that actually sit in the set. The period is the whole story.
-- |
-- | POLICY FAMILY: `quantiseNearest` is the round-to-nearest policy; `snapDown`
-- | (floor) and `snapUp` (ceil) are the two LAWFUL members — the right and left
-- | Galois adjoints of the set's inclusion into all pitches
-- | (`m <= n ⟺ m <= snapDown n`, and dually). The adjunction law pins them
-- | completely — no tie-break rule needed — and nearest always agrees with one
-- | of them (it is the compromise between the adjoints, hence its documented
-- | upward tie-break). All three share the tiling arithmetic. "Stay within the
-- | input's own octave" remains a future member.
module Harmonia.Quantise
  ( quantiseNearest
  , quantiseEqual
  , snapDown
  , snapUp
  ) where

import Prelude

import Data.Array (filter, null)
import Data.Foldable (maximum, minimum, minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Harmonia.PitchSet (PitchSet(..), realizeEqual)

-- | Snap `note` to the nearest member of the pitch-set's tiling. The register
-- | follows `note`; the set chooses the pitch class. An empty set is identity.
-- |
-- | Periodic: for each offset, the member of that residue class nearest `note`,
-- | then the nearest across offsets. This generalises the old `quantiseToChordPCs`
-- | (root 0, period 12) and `quantiseToScale` (the scale's own period) into one
-- | function. Finite: the nearest literal member (no tiling).
quantiseNearest :: PitchSet -> Int -> Int
quantiseNearest (PitchSet s) note
  | null s.offsets = note
  | otherwise =
      let
        cands = case s.period of
          Just p | p > 0 -> map (\off -> residueMember p (s.root + off) note) s.offsets
          _ -> map (\off -> s.root + off) s.offsets
      in
        fromMaybe note (minimumBy (comparing \c -> abs (c - note)) cands)

-- | The member `≡ target (mod period)` nearest to `note`. Ties (`note` exactly
-- | half a period from two members) resolve UPWARD, matching the historical
-- | `nearestWithPc` (`r <= 6` for period 12). Backend-stable positive modulo.
residueMember :: Int -> Int -> Int -> Int
residueMember p target note =
  let r = (((target - note) `mod` p) + p) `mod` p
  in if 2 * r <= p then note + r else note + r - p

-- | Snap DOWN: the greatest member of the tiling `<= note` — the right Galois
-- | adjoint of the set's inclusion into all pitches (`m <= n ⟺ m <= snapDown n`
-- | for every member `m`). Members are fixed points; an empty set is identity.
-- | A finite set has no member below its floor, so inputs beneath it CLAMP to
-- | the lowest member — the adjoint is partial there and the law holds only
-- | in range (periodic tilings are total, no caveat).
snapDown :: PitchSet -> Int -> Int
snapDown (PitchSet s) note
  | null s.offsets = note
  | otherwise = case s.period of
      Just p | p > 0 ->
        fromMaybe note
          (maximum (map (\off -> note - pmod (note - (s.root + off)) p) s.offsets))
      _ ->
        let members = map (s.root + _) s.offsets
        in case maximum (filter (_ <= note) members) of
          Just m -> m
          Nothing -> fromMaybe note (minimum members)

-- | Snap UP: the least member of the tiling `>= note` — the left Galois adjoint
-- | of the inclusion (`snapUp n <= m ⟺ n <= m`). Dual of `snapDown`, including
-- | the finite-set clamp (inputs above the top member clamp down to it).
snapUp :: PitchSet -> Int -> Int
snapUp (PitchSet s) note
  | null s.offsets = note
  | otherwise = case s.period of
      Just p | p > 0 ->
        fromMaybe note
          (minimum (map (\off -> note + pmod ((s.root + off) - note) p) s.offsets))
      _ ->
        let members = map (s.root + _) s.offsets
        in case minimum (filter (_ >= note) members) of
          Just m -> m
          Nothing -> fromMaybe note (maximum members)

-- | Positive modulo (result in `[0, m)` for `m > 0`), backend-stable.
pmod :: Int -> Int -> Int
pmod a m = ((a `mod` m) + m) `mod` m

-- | EQUAL-SPACING quantisation (the Dáil's other mode): map an input value
-- | `v ∈ [0, inMax]` across `spanPeriods` periods of the set, giving each degree an
-- | EQUAL slice of the input range, then realize to a pitch. Unlike `quantiseNearest`
-- | (where the input is a pitch and pitch-distance decides), here a linear input
-- | produces evenly-spaced steps regardless of the set's pitch intervals. The
-- | result is always a member of the set (it comes out of `realize`).
quantiseEqual :: PitchSet -> Int -> Int -> Int -> Int
quantiseEqual = realizeEqual
