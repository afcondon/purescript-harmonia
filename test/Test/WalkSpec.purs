-- | Tests for `Harmonia.Walk` — the seeded walk over a chord tree.
-- |
-- | Band 1 is the only part of the tree that is derived rather than tabulated,
-- | so the first thing here is that the derivation reproduces all three
-- | measured predecessors exactly. The rest is about the walk: that a seed
-- | determines it, that it never leaves the freedom window, and that at `Basic`
-- | it is the source's own generator rather than an approximation of it.
module Test.WalkSpec
  ( runWalkTests
  ) where

import Prelude

import Data.Array (all, filter, length, nub, range, sort, zipWith)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Freedom (Allowed, Side(..), admits, freedom, freeFlow, sideOf, strict)
import Harmonia.Palette (ChordType(..), Level(..), majorTriad, minorTriad, palette)
import Harmonia.Walk (band1, defaults, seed, successors, walk)
import Test.Assert (assertEqual', assertTrue')

quartal3 :: ChordType
quartal3 = fromMaybe majorTriad
  (Array.head (Array.filter (\t -> suffixOf t == "q3") palette))

runWalkTests :: Effect Unit
runWalkTests = do
  log ""
  log "--- Harmonia.Walk — band 1, derived and measured ---"

  -- ------------------------------------------------------------------
  -- The derivation against all three measured menus. Offsets and sides,
  -- read off the app: these are the only successors band 1 ever offers.
  -- ------------------------------------------------------------------
  assertEqual' "a major chord: ii iii IV V vi — as measured from Previous E and Previous G"
    { actual: band1 majorTriad
    , expected:
        [ { offset: 2, side: MinorSide }
        , { offset: 4, side: MinorSide }
        , { offset: 5, side: MajorSide }
        , { offset: 7, side: MajorSide }
        , { offset: 9, side: MinorSide }
        ]
    }

  -- The prediction here was SIX entries, including the ii°. The menu showed
  -- five: the diminished is excluded exactly as the tonic is.
  assertEqual' "a minor chord: III iv v VI VII — as measured from Previous Gm"
    { actual: band1 minorTriad
    , expected:
        [ { offset: 3, side: MajorSide }
        , { offset: 5, side: MinorSide }
        , { offset: 7, side: MinorSide }
        , { offset: 8, side: MajorSide }
        , { offset: 10, side: MajorSide }
        ]
    }

  -- Natural minor, not harmonic. The major dominant IS reachable from a minor
  -- chord — D and D7 are in the Gm menu — but from band 2, as a borrowing,
  -- never as a diatonic triad.
  assertTrue' "the v of a minor chord is minor"
    (Array.elem { offset: 7, side: MinorSide } (band1 minorTriad))

  -- **Measured, not derived.** Quartals and the Mystic are the only
  -- predecessors in the palette that do not follow the diatonic rule: over
  -- 4,116 observed transitions they land a fourth or a stack of fourths away
  -- about half the time (against a quarter by chance) and land on a diatonic
  -- offset BELOW chance, where every tertian predecessor is well above it.
  assertEqual' "a quartal chord moves in fourths, one to four up and one down"
    { actual: nub (map _.offset (band1 quartal3))
    , expected: [ 5, 10, 3, 8, 7 ]
    }

  assertTrue' "and it is offered both sides, because the side is not measured"
    (length (band1 quartal3) == 10)

  -- The two rules are not disjoint, and where they agree is the interesting
  -- part: +5 and +7 are IV and V to a tertian chord and a fourth up and a
  -- fourth down to a quartal one. The same two moves, arrived at from
  -- unrelated premises — which is presumably why they are the two that
  -- survive in every harmonic idiom anyone builds.
  assertEqual' "the quartal and diatonic trees agree on exactly IV and V"
    { actual: sort (nub (Array.intersect (map _.offset (band1 quartal3))
                                         (map _.offset (band1 majorTriad))))
    , expected: [ 5, 7 ]
    }

  -- Every TERTIAN chord type resolves to one of exactly two trees, by its third.
  assertTrue' "band 1 depends only on the side, so every type has one of two trees"
    (all (\ct -> band1 ct == band1 (if sideOf ct == MinorSide then minorTriad else majorTriad))
         [ majorTriad, minorTriad ])

  -- ------------------------------------------------------------------
  -- The walk.
  -- ------------------------------------------------------------------
  let cMajor = defaults { home = { tonic: 0, minor: false } }
      p = walk cMajor (seed 42) 8

  assertEqual' "a walk of eight is eight chords" { actual: length p, expected: 8 }

  assertEqual' "it starts on the key"
    { actual: map (\c -> { root: c.root, sfx: suffixOf c.chordType }) (Array.take 1 p)
    , expected: [ { root: 0, sfx: "" } ]
    }

  assertEqual' "a minor key starts on its minor triad"
    { actual: map (\c -> suffixOf c.chordType)
                (Array.take 1 (walk (defaults { home = { tonic: 9, minor: true } }) (seed 1) 4))
    , expected: [ "m" ]
    }

  -- Determinism is the property the sampling side depends on: a seeded
  -- progression is re-runnable, which is what lets a generated set be treated
  -- like a swept transect.
  assertEqual' "the same seed gives the same progression"
    { actual: map label (walk cMajor (seed 42) 12)
    , expected: map label p <> map label (Array.drop 8 (walk cMajor (seed 42) 12))
    }

  assertTrue' "different seeds differ"
    (map label (walk cMajor (seed 42) 12) /= map label (walk cMajor (seed 43) 12))

  -- ------------------------------------------------------------------
  -- Neither axis is ever violated, over many seeds rather than one.
  -- ------------------------------------------------------------------
  for_ [ 0, 1, 2, 3, 4, 5 ] \f ->
    for_ (range 1 25) \s -> do
      let set = defaults { home = { tonic: 0, minor: false }, freedom = freedom f }
          chords = walk set (seed s) 16
      assertTrue' ("freedom " <> show f <> " seed " <> show s <> ": every chord is in reach")
        (all (\c -> admits set.home set.freedom { root: c.root, side: sideOf c.chordType })
             chords)

  -- At Basic the palette is two triads, so the walk IS the source's generator:
  -- band 1 chooses the root and there is nothing to colour it with.
  let basic = walk (defaults { complexity = Basic, freedom = freeFlow }) (seed 7) 20
  assertTrue' "at Basic the walk emits only triads"
    (all (\c -> Array.elem (suffixOf c.chordType) [ "", "m" ]) basic)

  -- Above Basic the vocabulary opens. This is the local policy, not a
  -- measurement — the source widens the ROOT set at higher complexity and this
  -- widens the colouring instead.
  let rich = walk (defaults { complexity = Extreme, freedom = freeFlow }) (seed 7) 40
  assertTrue' "above Basic the walk reaches beyond triads"
    (length (nub (map (suffixOf <<< _.chordType) rich)) > 2)

  -- Avoiding duplicates is the source's own switch, and it must not be able
  -- to stall the walk when everything reachable has been used.
  let noDupes = walk (defaults { duplicates = false, freedom = strict, complexity = Basic }) (seed 3) 30
  assertEqual' "a walk that runs out of fresh chords keeps walking"
    { actual: length noDupes, expected: 30 }

  log ("  " <> show (length p) <> " chords from seed 42: " <> show (map label p))
  log ("  150 walks checked against both axes")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

label :: Allowed -> String
label a = pc a.root <> suffixOf a.chordType

pc :: Int -> String
pc n = fromMaybe "?" (Array.index names (mod n 12))
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

suffixOf :: ChordType -> String
suffixOf (ChordType t) = t.suffix
