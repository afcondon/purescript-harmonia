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
  , reading
  ) where

import Prelude


import Data.Array (catMaybes, concatMap, elem, elemIndex, filter, findIndex, head, index, length, mapWithIndex, nub, nubByEq, range, sort, sortBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Harmonia.Chord (Mode(..))

-- | A scale family: its intervals from its first degree, and the name of the
-- | mode starting on each degree (`Nothing`: a mode nobody calls anything).
-- | `keys`: each degree's mode as a `Mode`, where it is one a key can be in
-- | (the seven-note families harmonia has modes for).
type Family =
  { family :: String
  , intervals :: Array Int
  , modes :: Array (Maybe String)
  , keys :: Array (Maybe Mode)
  }

-- | The families, in the order a player is likely to know them.
families :: Array Family
families =
  [ { family: "pentatonic", intervals: [ 0, 2, 4, 7, 9 ]
    , modes: [ Just "major pentatonic", Just "suspended pentatonic", Nothing, Nothing, Just "minor pentatonic" ], keys: [] }
  , { family: "major", intervals: [ 0, 2, 4, 5, 7, 9, 11 ]
    , modes: map Just [ "major", "dorian", "phrygian", "lydian", "mixolydian", "minor", "locrian" ]
    , keys: map Just [ Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian ] }
  , { family: "blues", intervals: [ 0, 3, 5, 6, 7, 10 ]
    , modes: [ Just "minor blues", Just "major blues", Nothing, Nothing, Nothing, Nothing ], keys: [] }
  , { family: "melodic minor", intervals: [ 0, 2, 3, 5, 7, 9, 11 ]
    , modes: map Just [ "melodic minor", "dorian ♭2", "lydian augmented", "lydian dominant", "mixolydian ♭6", "locrian ♮2", "altered" ]
    , keys: map Just [ MelodicMinor, DorianFlat2, LydianAugmented, LydianDominant, MixolydianFlat6, LocrianNat2, Altered ] }
  , { family: "harmonic minor", intervals: [ 0, 2, 3, 5, 7, 8, 11 ]
    , modes: map Just [ "harmonic minor", "locrian ♮6", "ionian ♯5", "dorian ♯4", "phrygian dominant", "lydian ♯2", "ultralocrian" ]
    , keys: map Just [ HarmonicMinor, LocrianNat6, IonianSharp5, DorianSharp4, PhrygianDominant, LydianSharp2, Ultralocrian ] }
  , { family: "harmonic major", intervals: [ 0, 2, 4, 5, 7, 8, 11 ]
    , modes: [ Just "harmonic major", Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
    , keys: [ Just (Custom [ 0, 2, 4, 5, 7, 8, 11 ]) ] }
  , { family: "diminished", intervals: [ 0, 2, 3, 5, 6, 8, 9, 11 ]
    , modes: [ Just "diminished (whole-half)", Just "half-whole", Just "diminished (whole-half)", Just "half-whole"
             , Just "diminished (whole-half)", Just "half-whole", Just "diminished (whole-half)", Just "half-whole" ], keys: [] }
  , { family: "whole tone", intervals: [ 0, 2, 4, 6, 8, 10 ]
    , modes: map (const (Just "whole tone")) (range 0 5), keys: [] }
  ]

-- | One name a set goes by: its root (a pitch class) and the mode, and the
-- | mode as a key, where a key can be in it.
type Name = { root :: Int, mode :: String, key :: Maybe Mode }

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
        names = catMaybes (mapWithIndex (\i m -> m <#> \mode -> { root: pc (t + fromMaybe 0 (index fam.intervals i)), mode, key: join (index fam.keys i) }) fam.modes)
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

-- | **The key a progression reads best in**: of the scales a key can be in,
-- | the one that leaves out fewest of its notes, named from the chords' own
-- | roots first. The answer to "what is this progression in", when the key
-- | it was written in is no answer at all.
reading :: Array Int -> Array Int -> Maybe { root :: Int, mode :: String, key :: Mode, outside :: Array Int }
reading roots notes = head (catMaybes (map keyed (fitsFor 12 roots notes)))
  where
  -- a set's name as a key: a common one if it has one (the major modes, plain
  -- harmonic and melodic minor), the chords' roots choosing among them; so
  -- C♯° E°7 D♭ B♭m is F harmonic minor, not D♭ lydian ♯2, and a C–B♭ vamp
  -- still reads C mixolydian
  keyed f = do
    n <- head (sortBy (\a b -> compare (tier a.key) (tier b.key)) (filter (\nm -> nm.key /= Nothing) f.names))
    k <- n.key
    pure { root: n.root, mode: n.mode, key: k, outside: f.outside }
  tier = case _ of
    Just m | elem m [ Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian, HarmonicMinor, MelodicMinor ] -> 0
    _ -> 1
