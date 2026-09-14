-- | Tests for `Harmonia.Freedom` — the distance axis.
-- |
-- | **The centrepiece is a measurement, not an opinion.** `measured` below is a
-- | real Replace menu, transcribed chord by chord: Key C major, Freedom 1,
-- | Complexity Extreme, Previous G, ninety-four chords each recorded as offered
-- | or greyed out. The library has to reproduce all ninety-four.
-- |
-- | That golden exists because the reading it replaced was wrong and looked
-- | right. Collett's paper walks outward from the tonic — relative minors, then
-- | majors on their roots, then parallel minors — and it was taken here for a
-- | description of the freedom levels. It is compositional advice, and it
-- | predicts D, A and E all arriving at level 1. The menu says otherwise: at
-- | freedom 1 in C, D is offered and A is refused. **Prose lost to data.**
module Test.FreedomSpec
  ( runFreedomTests
  ) where

import Prelude

import Data.Array (elem, filter, length, nub, range, sort)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Freedom (Area, Side(..), admits, allowed, areasUpTo, fifths, freedom, freeFlow, position, ring, sideOf, strict)
import Harmonia.Palette (ChordType(..), Level(..), palette)
import Test.Assert (assertEqual', assertTrue')

runFreedomTests :: Effect Unit
runFreedomTests = do
  log ""
  log "--- Harmonia.Freedom — a window on the circle of fifths ---"

  let cMajor = { tonic: 0, minor: false }
      aMinor = { tonic: 9, minor: true }

  -- ------------------------------------------------------------------
  -- The measurement. Ninety-four real observations at freedom 1 in C.
  -- ------------------------------------------------------------------
  let wrong = filter
        (\m -> admits cMajor (freedom 1) { root: m.root, side: m.side } /= m.offered)
        measured

  assertEqual' "every chord in the measured menu is admitted or refused as observed"
    { actual: map _.sym wrong, expected: [] }

  -- ------------------------------------------------------------------
  -- The four observations that pin `sideOf`, called out by name because
  -- each of them contradicts something or settles something.
  --
  -- At freedom 1 in C, the A segment is split: A minor sits at C's own
  -- position (0) and is offered, while A major sits at +3 and is refused.
  -- So any chord rooted on A tells us which ring it belongs to.
  -- ------------------------------------------------------------------
  assertEqual' "a dominant is a MAJOR-side chord — A7 is refused where Am7 is offered"
    { actual: map (sideOf >>> show) (bySuffix "7"), expected: [ "MajorSide" ] }

  assertEqual' "a suspension is major-side — Asus4 refused"
    { actual: map (sideOf >>> show) (bySuffix "sus4"), expected: [ "MajorSide" ] }

  assertEqual' "a quartal is major-side — Aq3 refused, despite [0,5,10,15] reducing a 15 to a 3"
    { actual: map (sideOf >>> show) (bySuffix "q3" <> bySuffix "q4")
    , expected: [ "MajorSide", "MajorSide" ]
    }

  assertEqual' "a diminished seventh is minor-side — A°7 and Aø7 offered"
    { actual: map (sideOf >>> show) (bySuffix "°7" <> bySuffix "ø7")
    , expected: [ "MinorSide", "MinorSide" ]
    }

  -- ------------------------------------------------------------------
  -- The source's own two sentences, executable.
  -- ------------------------------------------------------------------
  assertEqual' "freedom 0 in C is C F G major and A D E minor"
    { actual: named (areasUpTo cMajor strict)
    , expected: [ "A-", "C+", "D-", "E-", "F+", "G+" ]
    }

  assertEqual' "strict + Basic in C is exactly the six closest chords"
    { actual: sort (map label (allowed cMajor strict Basic))
    , expected: [ "Am", "C", "Dm", "Em", "F", "G" ]
    }

  assertEqual' "free flow is all twenty-four areas"
    { actual: length (areasUpTo cMajor freeFlow), expected: 24 }

  assertTrue' "free flow reaches the tritone, and nothing below it does"
    (elem { root: 6, side: MajorSide } (areasUpTo cMajor freeFlow)
       && not (elem { root: 6, side: MajorSide } (areasUpTo cMajor (freedom 4))))

  -- ------------------------------------------------------------------
  -- Shape. Four areas per ring — a major and a minor at each end of the
  -- window — except ring 0, which is the tonic segment plus one either
  -- side, and the last, where +6 and −6 are the same wedge.
  -- ------------------------------------------------------------------
  assertEqual' "ring sizes"
    { actual: map (\n -> length (ring cMajor n)) (range 0 5)
    , expected: [ 4, 4, 4, 4, 4, 2 ]
    }

  assertEqual' "the rings partition the twenty-four areas"
    { actual: length (nub (Array.concatMap (ring cMajor) (range 0 5))) + 2
    , expected: 24
    }

  for_ (range 0 4) \n ->
    assertTrue' ("freedom " <> show (n + 1) <> " contains freedom " <> show n)
      (Array.all (\a -> elem a (areasUpTo cMajor (freedom (n + 1))))
                 (areasUpTo cMajor (freedom n)))

  -- ------------------------------------------------------------------
  -- A key and its relative minor share every wedge, so they share every
  -- window. The mode picks the starting chord and nothing else — which is
  -- the SAME rule that puts a minor chord at its relative major.
  -- ------------------------------------------------------------------
  for_ (range 0 5) \n ->
    assertEqual' ("A minor's freedom " <> show n <> " is C major's")
      { actual: named (areasUpTo aMinor (freedom n))
      , expected: named (areasUpTo cMajor (freedom n))
      }

  -- Position arithmetic, spot-checked where it is easiest to get wrong.
  assertEqual' "fifths distances around C"
    { actual: map (fifths 0) [ 0, 7, 2, 5, 10, 6 ], expected: [ 0, 1, 2, -1, -2, 6 ] }

  assertEqual' "A minor sits at C's position, not A's"
    { actual: Tuple (position cMajor { root: 9, side: MinorSide })
                    (position cMajor { root: 9, side: MajorSide })
    , expected: Tuple 0 3
    }

  -- ------------------------------------------------------------------
  -- Independence of the axes, which is the whole reason there are two.
  -- ------------------------------------------------------------------
  assertEqual' "complexity does not change which roots are available"
    { actual: nub (sort (map _.root (allowed cMajor strict Extreme)))
    , expected: nub (sort (map _.root (allowed cMajor strict Basic)))
    }

  assertTrue' "freedom does not change which types are available"
    (nub (sort (map (suffixOf <<< _.chordType) (allowed cMajor freeFlow Medium)))
       == nub (sort (map (suffixOf <<< _.chordType) (allowed cMajor strict Medium))))

  log ("  " <> show (length measured) <> " measured chords reproduced; "
        <> show (length (allowed cMajor freeFlow Extreme)) <> " chords at free flow and extreme, "
        <> show (length (allowed cMajor strict Basic)) <> " at strict and basic")

-- ---------------------------------------------------------------------------
-- The measurement
-- ---------------------------------------------------------------------------

-- | **Key C major · Freedom 1 · Complexity Extreme · Previous G**, transcribed
-- | from the app's Replace menu. `offered` is white in the menu; `false` is
-- | greyed out.
-- |
-- | `side` is read off the chord symbol by the rule under test — an `m`, `°` or
-- | `ø` after the root means a minor third — so the assertion is that the
-- | library's window reproduces ninety-four real verdicts from that reading
-- | alone. The discriminating roots are A, C, E, F and B♭, where the major and
-- | minor rings of one wedge fall on opposite sides of the window; G and D are
-- | admitted both ways and carry no information.
measured :: Array { sym :: String, root :: Int, side :: Side, offered :: Boolean }
measured =
  -- band 1
  [ w "Am" 9 MinorSide, w "Bm" 11 MinorSide, w "C" 0 MajorSide
  , w "D" 2 MajorSide, w "Em" 4 MinorSide
  -- band 2
  , w "Gaug" 7 MajorSide, w "Gsus4" 7 MajorSide
  , g "Ab°" 8 MinorSide, g "A" 9 MajorSide, g "Asus4" 9 MajorSide
  , w "Bb" 10 MajorSide, g "Bb°" 10 MinorSide, w "Bm" 11 MinorSide
  , g "Cm" 0 MinorSide, g "C°" 0 MinorSide
  , w "Dm" 2 MinorSide, w "D7" 2 MajorSide, w "Dsus4" 2 MajorSide
  , g "Eb°" 3 MinorSide, g "E7" 4 MajorSide, w "E°" 4 MinorSide, w "F" 5 MajorSide
  -- band 3
  , w "G°7" 7 MinorSide, w "Gø7" 7 MinorSide
  , w "Am6" 9 MinorSide, g "A6" 9 MajorSide, g "A7" 9 MajorSide
  , w "Am7" 9 MinorSide, w "Aø7" 9 MinorSide, w "A°7" 9 MinorSide
  , g "A7sus4" 9 MajorSide, w "Bb7" 10 MajorSide, g "Bbø7" 10 MinorSide
  , w "C6" 0 MajorSide, w "CM7" 0 MajorSide, g "C°7" 0 MinorSide, g "Cø7" 0 MinorSide
  , g "Db°7" 1 MinorSide, g "Dbø7" 1 MinorSide
  , w "D7" 2 MajorSide, w "D7sus4" 2 MajorSide, w "Dm7" 2 MinorSide
  , g "Eb°7" 3 MinorSide, g "Eb6" 3 MajorSide, g "EbM7" 3 MajorSide
  , w "Eø7" 4 MinorSide, w "E°7" 4 MinorSide, g "E7" 4 MajorSide
  , w "FM7" 5 MajorSide, w "F6" 5 MajorSide
  , g "A7#5" 9 MajorSide, w "Bb7#5" 10 MajorSide, w "CM7b5" 0 MajorSide
  , w "D7#5" 2 MajorSide, g "EbM7b5" 3 MajorSide, g "E7#5" 4 MajorSide
  , w "FM7b5" 5 MajorSide
  -- band 4
  , w "Am9" 9 MinorSide, w "Am9b5" 9 MinorSide, w "Am6+9" 9 MinorSide
  , w "CM7+9" 0 MajorSide, w "C6+9" 0 MajorSide, g "Cm9" 0 MinorSide
  , g "Cm6+9" 0 MinorSide, w "Dm9b5" 2 MinorSide, w "Dm9" 2 MinorSide
  , w "Dm6+9" 2 MinorSide, g "Eb6+9" 3 MajorSide, w "Em9b5" 4 MinorSide
  , g "Fm6+9" 5 MinorSide, w "F6+9" 5 MajorSide, w "FM7+9" 5 MajorSide
  , w "FM9+13" 5 MajorSide, g "A9sus4" 9 MajorSide, w "CM9b5" 0 MajorSide
  , w "D9sus4" 2 MajorSide, g "EbM9b5" 3 MajorSide, w "FM9b5" 5 MajorSide
  -- band 5
  , w "Gq3" 7 MajorSide, g "AbM9#11" 8 MajorSide, w "Am11" 9 MinorSide
  , g "Aq3" 9 MajorSide, w "Bbq3" 10 MajorSide, g "Cm11" 0 MinorSide
  , w "CM9+13" 0 MajorSide, g "DbM9#11" 1 MajorSide, w "Dm11" 2 MinorSide
  , w "Dq3" 2 MajorSide, g "Eb9#11" 3 MajorSide, g "Eb9+13" 3 MajorSide
  , g "Eq3" 4 MajorSide, w "Em11" 4 MinorSide, w "F9#11" 5 MajorSide
  , w "F9+13" 5 MajorSide
  ]
  where
  w sym r s = { sym, root: r, side: s, offered: true }
  g sym r s = { sym, root: r, side: s, offered: false }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

named :: Array Area -> Array String
named = sort <<< map one
  where
  one a = pc a.root <> (case a.side of
    MajorSide -> "+"
    MinorSide -> "-")

label :: { root :: Int, chordType :: ChordType } -> String
label r = pc r.root <> suffixOf r.chordType

pc :: Int -> String
pc n = fromMaybe "?" (Array.index names (mod n 12))
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

suffixOf :: ChordType -> String
suffixOf (ChordType t) = t.suffix

bySuffix :: String -> Array ChordType
bySuffix sfx = filter (\ct -> suffixOf ct == sfx) palette
