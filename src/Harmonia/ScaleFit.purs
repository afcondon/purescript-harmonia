-- | **The scales that fit a group of chords.**
-- |
-- | A starting point for improvising over a progression: take the pitch
-- | classes a run of chords uses and find the scales that hold them, without
-- | reference to the key the chords were written in. A progression built in C
-- | with a borrowed ♭VII comes back as C mixolydian, not as "C major, one
-- | wrong note".
-- |
-- | ## One answer a set of notes
-- |
-- | The modes of a scale are the same notes from a different start, so on an
-- | instrument they are the same shapes: D dorian and C major are one answer,
-- | named twice. A `Fit` is therefore one pitch-class set, with every name it
-- | goes by, the names on the chords' own roots first (over Dm7 G7 the set is
-- | "D dorian" before it is "C major").
-- |
-- | ## Ranking
-- |
-- | Fewest of the chords' notes left out first. Among complete fits, the
-- | smaller scale first: a pentatonic that holds every chord tone leaves the
-- | fewest notes to land on wrongly. Then the families in the order a player
-- | is likely to know them.
module Harmonia.ScaleFit
  ( Family
  , families
  , Name
  , Fit
  , fitsFor
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, elem, elemIndex, filter, findIndex, head, index, length, mapWithIndex, nub, nubByEq, range, sort, sortBy)
import Data.Maybe (Maybe(..), fromMaybe)

-- | A scale family: its intervals from its first degree, and the name of the
-- | mode starting on each degree (`Nothing`: a mode nobody calls anything).
type Family =
  { family :: String
  , intervals :: Array Int
  , modes :: Array (Maybe String)
  }

-- | The families, in the order a player is likely to know them.
families :: Array Family
families =
  [ { family: "pentatonic", intervals: [ 0, 2, 4, 7, 9 ]
    , modes: [ Just "major pentatonic", Just "suspended pentatonic", Nothing, Nothing, Just "minor pentatonic" ] }
  , { family: "major", intervals: [ 0, 2, 4, 5, 7, 9, 11 ]
    , modes: map Just [ "major", "dorian", "phrygian", "lydian", "mixolydian", "minor", "locrian" ] }
  , { family: "blues", intervals: [ 0, 3, 5, 6, 7, 10 ]
    , modes: [ Just "minor blues", Just "major blues", Nothing, Nothing, Nothing, Nothing ] }
  , { family: "melodic minor", intervals: [ 0, 2, 3, 5, 7, 9, 11 ]
    , modes: map Just [ "melodic minor", "dorian ♭2", "lydian augmented", "lydian dominant", "mixolydian ♭6", "locrian ♮2", "altered" ] }
  , { family: "harmonic minor", intervals: [ 0, 2, 3, 5, 7, 8, 11 ]
    , modes: map Just [ "harmonic minor", "locrian ♮6", "ionian ♯5", "dorian ♯4", "phrygian dominant", "lydian ♯2", "ultralocrian" ] }
  , { family: "harmonic major", intervals: [ 0, 2, 4, 5, 7, 8, 11 ]
    , modes: [ Just "harmonic major", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ] }
  , { family: "diminished", intervals: [ 0, 2, 3, 5, 6, 8, 9, 11 ]
    , modes: [ Just "diminished (whole-half)", Just "half-whole", Just "diminished (whole-half)", Just "half-whole"
             , Just "diminished (whole-half)", Just "half-whole", Just "diminished (whole-half)", Just "half-whole" ] }
  , { family: "whole tone", intervals: [ 0, 2, 4, 6, 8, 10 ]
    , modes: map (const (Just "whole tone")) (range 0 5) }
  ]

-- | One name a set goes by: its root (a pitch class) and the mode.
type Name = { root :: Int, mode :: String }

-- | One set of notes that fits: its pitch classes, its family, every name it
-- | goes by (the chords' own roots first), and the chords' notes it leaves out.
type Fit =
  { pcs :: Array Int
  , family :: String
  , names :: Array Name
  , outside :: Array Int
  }

-- | **The scales for a group of chords**, best first: every set of notes
-- | that leaves out at most `slack` of `notes`' pitch classes. `roots`: the
-- | chords' roots, the first chord's first, which order each set's names.
fitsFor :: Int -> Array Int -> Array Int -> Array Fit
fitsFor slack roots notes = sortBy order (filter (\f -> length f.outside <= slack) sets)
  where
  want = nub (sort (map pc notes))
  -- every family on every root, one entry a distinct set (a whole-tone scale
  -- has two transpositions, not twelve)
  sets = nubByEq (\a b -> a.pcs == b.pcs) (concatMap (\fam -> map (fitOn fam) (range 0 11)) families)
  fitOn fam t =
    let ps = sort (map (\iv -> pc (t + iv)) fam.intervals)
        names = catMaybes (mapWithIndex (\i m -> m <#> \mode -> { root: pc (t + fromMaybe 0 (index fam.intervals i)), mode }) fam.modes)
    in { pcs: ps, family: fam.family, names: sortBy byRoot (nubByEq (\a b -> a.root == b.root) (sortBy byRoot names))
       , outside: filter (\p -> not (elem p ps)) want }
  byRoot a b = compare (rootRank a.root) (rootRank b.root)
  rootRank r = fromMaybe 99 (elemIndex r (nub (map pc roots)))
  order a b =
    compare (length a.outside) (length b.outside)
      <> compare (length a.pcs) (length b.pcs)
      <> compare (familyRank a.family) (familyRank b.family)
      <> compare (bestRoot a) (bestRoot b)
  familyRank f = fromMaybe 99 (findIndex (\x -> x.family == f) families)
  bestRoot f = fromMaybe 99 (map (\n -> rootRank n.root) (head f.names))
  pc n = ((n `mod` 12) + 12) `mod` 12
