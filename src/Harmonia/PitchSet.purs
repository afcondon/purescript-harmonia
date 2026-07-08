-- | A `PitchSet` is the general quantisation target: an ascending list of
-- | LITERAL semitone offsets from a root, plus an optional tiling period. It is
-- | the thing an engine indexes into (`realize :: index -> MIDI`) and the thing a
-- | quantiser snaps onto (see `Harmonia.Quantise`). An ordinary scale is just the
-- | degenerate octave-periodic case; a chord is a set whose tones repeat every
-- | octave; an extended voicing is a FINITE set with wide literal offsets.
-- |
-- | Two design commitments:
-- |
-- |   * Offsets are LITERAL, never folded to pitch-classes (`mod 12`). A 9th at
-- |     offset 26 stays a high D; it never collapses into the base octave. That is
-- |     what lets an extended chord across three octaves be honoured when it is
-- |     used as an index source (the "don't lop the ninth" guarantee). When the
-- |     same set is used as a QUANTISE target the tiling still folds each offset by
-- |     `period` to find the nearest member — the two readings coexist cleanly.
-- |
-- |   * `period` is SEPARATE from the offsets' extent: 12 for ordinary octaves, 19
-- |     for a tritave-ish world, `Nothing` for a one-shot finite set with no
-- |     repetition. Non-octave repetition is just `period /= Just 12`.
-- |
-- | Pure Prelude/Data — no serialization here (the wire codec is a comms concern
-- | that lives with the transport layer, not the theory). Builds on JS and purerl.
module Harmonia.PitchSet
  ( PitchSet(..)
  , periodic
  , finite
  , chord
  , extendedChord
  , scale
  , offsets
  , root
  , period
  , cardinality
  , realize
  , isMember
  , equalIndex
  , realizeEqual
  , floorDiv
  ) where

import Prelude

import Data.Array (any, length, (!!))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)

-- | `offsets` ascending, literal semitones from `root` (the MIDI note of offset
-- | 0); `period` = semitones until the whole list tiles (`Nothing` = finite, no
-- | tiling).
newtype PitchSet = PitchSet
  { offsets :: Array Int
  , root :: Int
  , period :: Maybe Int
  }

derive instance eqPitchSet :: Eq PitchSet
derive newtype instance showPitchSet :: Show PitchSet

-- | A periodic set: `periodic root offsets period` (a scale or an octave-tiling
-- | chord).
periodic :: Int -> Array Int -> Int -> PitchSet
periodic r os p = PitchSet { offsets: os, root: r, period: Just p }

-- | A finite set: `finite root offsets` — no tiling, indices clamp at the ends.
finite :: Int -> Array Int -> PitchSet
finite r os = PitchSet { offsets: os, root: r, period: Nothing }

-- | A PURE chord (Dáil axis 1, pure): pitch-classes tiling every octave. EVERY
-- | octave carries every chord tone — all D's are the 9th. `chord [0, 4, 7]`.
chord :: Array Int -> PitchSet
chord pcs = periodic 0 pcs 12

-- | An EXTENDED chord (Dáil axis 1, extended): `extendedChord root offsets octaves`
-- | — LITERAL offsets from `root`, tiling over an EXPLICIT span of `octaves` (period
-- | = `12 * octaves`). A colour tone that sits more than an octave up — a 9th at
-- | offset 14, a 13th at 21 — then appears only in SOME octaves, not all: near a low
-- | D you snap to a chord tone that is NOT the 9th; near the D an octave up you snap
-- | to the 9th. That octave-dependent colour is the whole point of the extended
-- | mode, and the caller decides how wide the repeat is (a chord that fits in one
-- | octave can still be given a two-octave span deliberately).
extendedChord :: Int -> Array Int -> Int -> PitchSet
extendedChord r os octaves = periodic r os (12 * octaves)

-- | A scale as a periodic set: `scale root intervals` over one octave (period 12).
-- | The degenerate PitchSet that started the generalisation — a scale is just a
-- | chord you index rather than snap to.
scale :: Int -> Array Int -> PitchSet
scale r ivls = periodic r ivls 12

offsets :: PitchSet -> Array Int
offsets (PitchSet s) = s.offsets

root :: PitchSet -> Int
root (PitchSet s) = s.root

period :: PitchSet -> Maybe Int
period (PitchSet s) = s.period

-- | Notes per period (the `N` that "octave = +N indices" refers to).
cardinality :: PitchSet -> Int
cardinality (PitchSet s) = length s.offsets

-- | Realize an integer INDEX to a MIDI pitch (the "index source" reading).
-- | Periodic sets tile infinitely in both directions (so voice offsets never
-- | clip); finite sets clamp at the ends.
realize :: PitchSet -> Int -> Int
realize (PitchSet s) i =
  let n = length s.offsets in
  if n <= 0 then s.root
  else case s.period of
    Just p ->
      let oct = floorDiv i n
          pos = i - oct * n
      in s.root + p * oct + fromMaybe 0 (s.offsets !! pos)
    Nothing ->
      s.root + fromMaybe 0 (s.offsets !! clamp 0 (n - 1) i)

-- | Is `note` a member of the set's tiling? For a periodic set the offsets fold
-- | by `period` (so an offset of 26 with period 12 matches any D); for a finite
-- | set membership is literal. The companion law to `quantiseNearest`: a quantised
-- | note is always a member.
isMember :: PitchSet -> Int -> Boolean
isMember (PitchSet s) note =
  case s.period of
    Just p | p > 0 ->
      let res = pmod (note - s.root) p
      in any (\off -> pmod off p == res) s.offsets
    _ -> any (\off -> s.root + off == note) s.offsets

-- | Flat-equal mapping: an input `v` in `[0, inMax]` divides equally across
-- | `slots` indices (Instruo Dáil-style, not hierarchical). `slots` is typically
-- | `spanPeriods * cardinality`.
equalIndex :: Int -> Int -> Int -> Int
equalIndex slots inMax v =
  clamp 0 (slots - 1) (floor (toNumber v * toNumber slots / toNumber (inMax + 1)))

-- | The whole front mapping: input value -> index (flat-equal over `spanPeriods`
-- | periods) -> realized MIDI pitch.
realizeEqual :: PitchSet -> Int -> Int -> Int -> Int
realizeEqual ps spanPeriods inMax v =
  realize ps (equalIndex (spanPeriods * cardinality ps) inMax v)

-- | Floor division — explicit (not Int `div`) so negative indices behave
-- | identically on the JS and Erlang backends.
floorDiv :: Int -> Int -> Int
floorDiv a b = floor (toNumber a / toNumber b)

-- | Positive modulo (result in `[0, m)` for `m > 0`), backend-stable.
pmod :: Int -> Int -> Int
pmod a m = ((a `mod` m) + m) `mod` m
