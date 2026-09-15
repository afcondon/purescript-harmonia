-- | `Harmonia.Trellis` — **choosing a path when every chord has alternatives.**
-- |
-- | A progression whose slots each hold several candidate voicings is no longer
-- | a list, it is a lattice: four options at each of three chords is sixty-four
-- | progressions. Playing it means choosing a path, and the interesting question
-- | is not *which option* but *how the choice is made*.
-- |
-- | There are two ends to that, and they are the two things this library already
-- | knows how to do:
-- |
-- |   * draw at each slot independently — pure surprise, and consecutive chords
-- |     may leap, because nothing is looking across the joins;
-- |   * take the path of least motion — at each slot, the option nearest what
-- |     was just played, which is voice leading applied to a choice rather than
-- |     to a construction.
-- |
-- | `Pull` is the dial between them, and `path` walks it.
-- |
-- | ## Smooth does not mean fixed
-- |
-- | The first slot has nothing to be smooth *from*, so it is always drawn from
-- | the seed. A `Smooth` pass therefore still differs from the last one — it is
-- | the JOINS that are minimal, not the outcome. That is what makes the setting
-- | usable: were it deterministic it would give one answer and never be worth
-- | pressing twice.
-- |
-- | ## Locking needs no concept here
-- |
-- | A slot the player has settled is a slot with one option. Narrowing the array
-- | is all locking is, and a locked slot then anchors the smoothness of its
-- | neighbours for free — so nothing about settling a choice reaches this module.
module Harmonia.Trellis
  ( Pull(..)
  , pulls
  , pullLabel
  , pullBlurb
  , path
  , pathMotion
  , size
  ) where

import Prelude

import Data.Array (index, length, mapWithIndex, range, snoc, sortBy, take, zipWith)
import Data.Array as Array
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..), fromMaybe)

import Harmonia.Voicing (Voicing, motionBetween)
import Harmonia.Walk (Seed, nextSeed, pick)

-- | **How hard the previous chord pulls on the next choice.**
data Pull
  -- | Draw uniformly. Every option at every slot is equally likely, and the
  -- | joins look after themselves.
  = Loose
  -- | Draw from the nearer half. Still a surprise, but one that has been asked
  -- | to stay in the neighbourhood of what came before.
  | Mid
  -- | Take the nearest. The smoothest path through the lattice from whatever the
  -- | first slot happened to draw.
  | Smooth

derive instance eqPull :: Eq Pull
derive instance ordPull :: Ord Pull

pulls :: Array Pull
pulls = [ Loose, Mid, Smooth ]

pullLabel :: Pull -> String
pullLabel = case _ of
  Loose -> "loose"
  Mid -> "mid"
  Smooth -> "smooth"

pullBlurb :: Pull -> String
pullBlurb = case _ of
  Loose -> "draw freely — every option equally likely"
  Mid -> "draw from the nearer half of each slot"
  Smooth -> "take the nearest option at every join"

-- | How many distinct paths the lattice holds: the product of the slot widths.
size :: forall a. Array (Array a) -> Int
size = foldl (\n opts -> n * max 1 (length opts)) 1

-- | **One pass: an option index per slot.**
-- |
-- | `Loose` and `Mid` are draws, so they run left to right — there is nothing to
-- | look ahead for when the next choice is a throw of the dice either way.
-- |
-- | `Smooth` is not a draw and must not be greedy. Measured first as greedy, it
-- | came out at mean joint motion 68 against `Mid`'s 66 — WORSE than the setting
-- | that is only half trying, because taking the nearest option at one slot can
-- | strand the walk far from every option at the next. So it runs the real thing
-- | over the lattice instead (see `smoothPath`), which is the algorithm the shape
-- | was asking for all along.
-- |
-- | An empty slot yields index 0 and is skipped by the caller; a slot of one is
-- | that one, whatever the pull.
path :: Pull -> Seed -> Array (Array Voicing) -> Array Int
path pull s0 slots = case pull of
  Smooth -> smoothPath s0 slots
  _ -> drawn pull s0 slots

-- | The two DRAWN settings, left to right.
drawn :: Pull -> Seed -> Array (Array Voicing) -> Array Int
drawn pull s0 slots = (foldl step { picks: [], prev: Nothing, seed: s0 } slots).picks
  where
  step acc opts = case length opts of
    0 -> acc { picks = snoc acc.picks 0, seed = nextSeed acc.seed }
    1 -> acc { picks = snoc acc.picks 0, prev = index opts 0, seed = nextSeed acc.seed }
    _ ->
      let
        chosen = case acc.prev of
          -- Nothing to be smooth from: the first slot is always a draw, which is
          -- what keeps a Smooth pass from being the same pass every time.
          Nothing -> drawFrom acc.seed (indices opts)
          Just p -> case pull of
            Mid -> drawFrom acc.seed (nearerHalf p opts)
            _ -> drawFrom acc.seed (indices opts)
      in
        acc { picks = snoc acc.picks chosen.value
            , prev = index opts chosen.value
            , seed = chosen.seed
            }

  indices opts = range 0 (length opts - 1)

  drawFrom s xs = case pick s xs of
    Just x -> { value: x.value, seed: x.seed }
    Nothing -> { value: 0, seed: nextSeed s }

  byMotion p opts =
    map _.i (sortBy (comparing _.d) (mapWithIndex (\i v -> { i, d: motionBetween p v }) opts))

  -- At least one, so the half never empties on a two-option slot.
  nearerHalf p opts = take (max 1 (length opts / 2)) (byMotion p opts)

-- | **The minimum-motion path, given where the first slot landed.**
-- |
-- | One pass of dynamic programming over the lattice: carry, for every option of
-- | the slot just placed, the cheapest way to have reached it and the trail that
-- | did. Each slot costs (options × previous options) comparisons, which for a
-- | progression is nothing.
-- |
-- | The FIRST slot is still drawn from the seed, deliberately. Optimising it too
-- | would make `Smooth` deterministic — one answer, never worth pressing twice —
-- | whereas drawing it keeps every pass different and makes the setting mean what
-- | it says: not "the smoothest progression" but "smoothly, from here".
smoothPath :: Seed -> Array (Array Voicing) -> Array Int
smoothPath s0 slots = case Array.uncons slots of
  Nothing -> []
  Just { head: first, tail: rest } ->
    let
      i0 = case pick s0 (range 0 (length first - 1)) of
        Just x -> x.value
        Nothing -> 0
    in
      case index first i0 of
        Nothing -> Array.replicate (length slots) 0
        Just v0 -> [ i0 ] <> best (foldl step [ { cost: 0, trail: [], last: v0 } ] rest)
  where
  step rows opts
    | length opts == 0 = map (\r -> r { trail = snoc r.trail 0 }) rows
    | otherwise =
        mapWithIndex
          (\j v ->
            let reached = map (\r -> { cost: r.cost + motionBetween r.last v, trail: snoc r.trail j, last: v }) rows
            in fromMaybe { cost: 0, trail: [ j ], last: v }
                 (Array.head (sortBy (comparing _.cost) reached)))
          opts

  best rows = case Array.head (sortBy (comparing _.cost) rows) of
    Just r -> r.trail
    Nothing -> []

-- | **What a chosen path costs at its joins** — the sum of the motion between
-- | consecutive chords. The number a `Pull` setting is trying to move, so it is
-- | worth showing next to the dial: it turns a claim about smoothness into a
-- | measurement the player can watch change.
pathMotion :: Array Voicing -> Int
pathMotion vs = sum (zipWith motionBetween vs (fromMaybe [] (Array.tail vs)))
