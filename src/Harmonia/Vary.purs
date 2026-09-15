-- | `Harmonia.Vary` — **the neighbourhood of one chord, on two axes.**
-- |
-- | The other two lenses in this library answer *which chord*. `Freedom` places a
-- | successor on the circle of fifths; the walk chooses between them. This one
-- | holds the chord still and varies how it is said — which is a different
-- | question, and the one the teachers are usually asking when they take a blues
-- | progression apart and put it back together as something else.
-- |
-- | ## Why the Progressions axes carry over
-- |
-- | In the chord-choosing model, **freedom** is how far from home the content may
-- | go and **complexity** is how deep into the tone vocabulary it may reach. Both
-- | survive the move down a level, with home redefined from "the tonic" to "this
-- | chord's own pitch classes":
-- |
-- |   * `Drift` — how far the CONTENT may move. `Held` is pure revoicing, the
-- |     pitch-class set fixed. `Thinned` may drop a tone. `Swapped` may move one
-- |     a semitone, which is where revoicing stops and substitution begins.
-- |   * `Density` — how elaborate the ARRANGEMENT may be. Octave span and how
-- |     many tones may be doubled.
-- |
-- | They stay independent, which is what makes the product worth laying out as a
-- | grid: omitting the fifth has nothing to do with spreading over three octaves,
-- | and either is interesting without the other.
-- |
-- | ## Catalogue at one end, lottery at the other
-- |
-- | No cell is enumerated and no cell is sampled — both fall out of one loop.
-- | Candidates are drawn from the seed and kept only if distinct, with a draw
-- | budget; near the chord the space is smaller than `want` and the cell fills
-- | with everything there is, and far from it the same code returns a handful out
-- | of an unbounded set. A short cell is therefore information rather than a
-- | failure: it is telling you that you have seen the whole neighbourhood.
-- |
-- | Seeded, for the same reason `Walk` is: a surprise you cannot get back to is a
-- | bad surprise, so a roll is an address and not an event.
module Harmonia.Vary
  ( Drift(..)
  , drifts
  , driftLabel
  , driftBlurb
  , Density(..)
  , densities
  , densityLabel
  , densityBlurb
  , Variation
  , variations
  ) where

import Prelude

import Data.Array (cons, filter, length, nub, range, snoc, sort)
import Data.Array as Array
import Data.Foldable (any, foldl)
import Data.Maybe (Maybe(..), fromMaybe)

import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (Open, Place(..), Rooted, Spread, applySpread, tonesOf)
import Harmonia.Voicing (Voicing, invert, voicingMidi)
import Harmonia.Walk (Seed, nextSeed, pick)

-- | **How far the pitch-class content may move.** The revoicing ↔ substitution
-- | axis, in three steps that are each a different musical claim.
data Drift
  -- | The same notes, rearranged. Every candidate is unarguably the same chord.
  = Held
  -- | A tone may be dropped. The root is never the one dropped — a rootless
  -- | voicing is a fine thing but it is a decision about the bass, which the
  -- | slash control already makes, and making it here too would confuse the axis.
  | Thinned
  -- | One tone may move a semitone: major ↔ minor third, ♮7 ↔ ♭7, ♮5 ↔ ♭5/♯5.
  -- | Chosen over drawing a new tone from a scale because it needs no key and is
  -- | always a sentence about THIS chord — which is what makes it a substitution
  -- | rather than a different chord that happens to be nearby.
  | Swapped

derive instance eqDrift :: Eq Drift
derive instance ordDrift :: Ord Drift

drifts :: Array Drift
drifts = [ Held, Thinned, Swapped ]

driftLabel :: Drift -> String
driftLabel = case _ of
  Held -> "held"
  Thinned -> "thinned"
  Swapped -> "swapped"

driftBlurb :: Drift -> String
driftBlurb = case _ of
  Held -> "the same notes, rearranged"
  Thinned -> "a tone may be dropped"
  Swapped -> "a tone may move a semitone"

-- | **How elaborate the arrangement may be.** Octave span and doubling budget;
-- | nothing here touches the content, which is `Drift`'s job.
data Density = Close | Spaced | Wide

derive instance eqDensity :: Eq Density
derive instance ordDensity :: Ord Density

densities :: Array Density
densities = [ Close, Spaced, Wide ]

densityLabel :: Density -> String
densityLabel = case _ of
  Close -> "close"
  Spaced -> "spaced"
  Wide -> "wide"

densityBlurb :: Density -> String
densityBlurb = case _ of
  Close -> "inside two octaves, one voice a tone"
  Spaced -> "three octaves, a tone may double"
  Wide -> "four octaves, freely doubled"

-- | `reach` is the most octaves one tone may be lifted; `doublings` how many
-- | tones may sound at two octaves at once.
shape :: Density -> { reach :: Int, doublings :: Int }
shape = case _ of
  Close -> { reach: 1, doublings: 0 }
  Spaced -> { reach: 2, doublings: 1 }
  Wide -> { reach: 3, doublings: 2 }

-- | One candidate. `chord` is what it actually sounds — under `Thinned` or
-- | `Swapped` that is NOT the chord you started from, and `moved` says so, so a
-- | view can mark the ones that have left.
type Variation =
  { voicing :: Voicing
  , chord :: Chord
  , moved :: Boolean
  }

-- | **`want` distinct voicings from this cell, or as many as it holds.**
-- |
-- | The budget is what makes a small neighbourhood terminate: eight draws per
-- | wanted candidate is generous where the space is large and cheap where it is
-- | small, and a cell that comes back short has been exhausted rather than gone
-- | wrong.
variations :: Open -> Rooted -> Drift -> Density -> Seed -> Int -> Array Variation
variations o r d dens s0 want = go s0 (want * 8) []
  where
  go s budget acc
    | length acc >= want = acc
    | budget <= 0 = acc
    | otherwise =
        let step = draw s
        in if any (\v -> notes v.voicing == notes step.value.voicing) acc
             then go step.seed (budget - 1) acc
             else go step.seed (budget - 1) (snoc acc step.value)

  notes = sort <<< voicingMidi

  draw s =
    let
      c = content d r s
      tones = tonesOf (chordSize c.rooted) c.rooted
      sp = spreadFor (shape dens) (length tones - 1) c.seed
      o' = o { reach = (shape dens).reach, minTones = chordSize c.rooted }
      base = applySpread o' c.rooted sp.spread
      rot = rotations sp.seed (length tones)
      Chord pcs = c.rooted.chord
    in
      { value:
          { voicing: foldl (\v _ -> invert 1 v) base (range 1 rot.value)
          , chord: Chord pcs
          , moved: c.moved
          }
      , seed: rot.seed
      }

  rotations s n =
    case pick s (range 0 (max 1 n - 1)) of
      Just x -> { value: x.value, seed: x.seed }
      Nothing -> { value: 0, seed: nextSeed s }

chordSize :: Rooted -> Int
chordSize r = let Chord pcs = r.chord in length (nub (map (\p -> mod p 12) pcs))

-- | The content half of a draw: the chord this candidate actually uses.
content :: Drift -> Rooted -> Seed -> { rooted :: Rooted, moved :: Boolean, seed :: Seed }
content d r s =
  let
    rootPc = mod r.root 12
    Chord raw = r.chord
    pcs = sort (nub (map (\p -> mod p 12) raw))
    others = filter (_ /= rootPc) pcs
  in
    case d of
      Held -> { rooted: r, moved: false, seed: nextSeed s }
      Thinned ->
        -- Below four tones there is nothing to spare: a triad minus a tone is an
        -- interval, which is a different kind of object and not a variation of
        -- this chord.
        if length pcs < 4 then { rooted: r, moved: false, seed: nextSeed s }
        else case pick s others of
          Nothing -> { rooted: r, moved: false, seed: nextSeed s }
          Just x ->
            { rooted: r { chord = Chord (filter (_ /= x.value) pcs) }
            , moved: true
            , seed: x.seed
            }
      Swapped -> case pick s others of
        Nothing -> { rooted: r, moved: false, seed: nextSeed s }
        Just x ->
          let
            sw = nextSeed x.seed
            up = case pick sw [ 1, -1 ] of
              Just y -> y
              Nothing -> { value: 1, seed: sw }
            moved' = mod (x.value + up.value + 12) 12
            kept = filter (_ /= x.value) pcs
          in
            -- A move that lands on a tone already present would silently thin the
            -- chord instead of colouring it, which belongs to the other axis.
            if moved' == rootPc || Array.elem moved' kept
              then { rooted: r, moved: false, seed: up.seed }
              else { rooted: r { chord = Chord (sort (cons moved' kept)) }
                   , moved: true
                   , seed: up.seed
                   }

-- | The arrangement half: one `Place` per non-bass tone, lifted within `reach`
-- | and doubled up to the budget.
spreadFor :: { reach :: Int, doublings :: Int } -> Int -> Seed -> { spread :: Spread, seed :: Seed }
spreadFor sh n s0 = foldl place { spread: [], seed: s0 } (range 1 (max 0 n))
  where
  place acc i =
    let
      lift = fromMaybe { value: 0, seed: nextSeed acc.seed } (pick acc.seed (range 0 sh.reach))
      double = i <= sh.doublings && odd' lift.seed
      k = lift.value
      ks = if double && k < sh.reach then [ k, k + 1 ] else [ k ]
    in
      { spread: snoc acc.spread (Place ks), seed: nextSeed lift.seed }
  odd' s = case pick s [ true, false ] of
    Just x -> x.value
    Nothing -> false
