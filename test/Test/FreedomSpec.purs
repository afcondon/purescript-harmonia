-- | Tests for `Harmonia.Freedom` — the distance axis.
-- |
-- | Two of these are the source's own sentences made executable, which is the
-- | strongest form of test available for a transcription: if the library
-- | disagrees with the paragraph it was transcribed from, one of them is wrong
-- | and it is not the paragraph.
module Test.FreedomSpec
  ( runFreedomTests
  ) where

import Prelude

import Data.Array (all, concatMap, elem, filter, length, nub, range, sort, sortBy)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Freedom (Area, Side(..), allowed, areasUpTo, freedom, freeFlow, ring, sideOf, strict)
import Harmonia.Palette (ChordType(..), Level(..), palette)
import Test.Assert (assertEqual', assertTrue')

runFreedomTests :: Effect Unit
runFreedomTests = do
  log ""
  log "--- Harmonia.Freedom — six rings, each one note from the last ---"

  let cMajor = { tonic: 0, minor: false }
      aMinor = { tonic: 9, minor: true }

  -- ------------------------------------------------------------------
  -- The source's opening sentence, executable.
  --
  --   "'Strict' freedom setting will include the very closest chords to C,
  --    which includes F, G and their relative minors Am, Dm and Em. These
  --    six chords are the ones most closely related to C major tonality."
  -- ------------------------------------------------------------------
  assertEqual' "freedom 0 in C is C F G major and A D E minor"
    { actual: named (areasUpTo cMajor strict)
    , expected: [ "A-", "C+", "D-", "E-", "F+", "G+" ]
    }

  -- ------------------------------------------------------------------
  -- The other end, also the source's:
  --
  --   "The highest level 5 is Free Flow, which includes everything,
  --    including whatever chord is on the oposite [sic] side of the wheel
  --    (F# in the case of C major)."
  -- ------------------------------------------------------------------
  assertEqual' "free flow is all twenty-four areas"
    { actual: length (areasUpTo cMajor freeFlow), expected: 24 }

  assertTrue' "free flow reaches the tritone"
    (elem { root: 6, side: MajorSide } (areasUpTo cMajor freeFlow))

  assertTrue' "and nothing below free flow does"
    (not (elem { root: 6, side: MajorSide } (areasUpTo cMajor (freedom 4))))

  -- ------------------------------------------------------------------
  -- Shape of the chain. Six rings of three, except the first and last
  -- which close a pair — 0 opens with its relative minors and 5 closes
  -- over the twenty-fourth area.
  -- ------------------------------------------------------------------
  assertEqual' "ring sizes"
    { actual: map (\n -> length (ring cMajor n)) (range 0 5)
    , expected: [ 6, 3, 3, 3, 3, 6 ]
    }

  assertTrue' "the rings are disjoint"
    (length (nub (concatMap (ring cMajor) (range 0 5))) == 24)

  -- Cumulative, like complexity — each level a superset of the one below.
  for_ (range 0 4) \n ->
    assertTrue' ("freedom " <> show (n + 1) <> " contains freedom " <> show n)
      (all (\a -> elem a (areasUpTo cMajor (freedom (n + 1))))
           (areasUpTo cMajor (freedom n)))

  -- ------------------------------------------------------------------
  -- A key and its relative minor share a wheel segment, so they share
  -- every ring. The mode picks the starting chord and nothing else.
  -- ------------------------------------------------------------------
  for_ (range 0 5) \n ->
    assertEqual' ("A minor's ring " <> show n <> " is C major's")
      { actual: named (ring aMinor n), expected: named (ring cMajor n) }

  -- ------------------------------------------------------------------
  -- `sideOf` against the nine examples the source gives by name.
  --
  --   "all chords that have a major or more neutral function (such as M,
  --    M6, M7, M9+13, quartal, etc.) will be shown in the respective major
  --    segment ... Those chords with a more distant or minor function are
  --    shown in the minor segments (such as m, m7, m9, 7 etc.)"
  -- ------------------------------------------------------------------
  for_ [ "", "M6", "M7", "M9+13", "q3" ] \sfx ->
    assertEqual' (show sfx <> " is a major-side chord")
      { actual: map sideOf (bySuffix sfx), expected: [ MajorSide ] }

  for_ [ "m", "m7", "m9", "7" ] \sfx ->
    assertEqual' (show sfx <> " is a minor-side chord")
      { actual: map sideOf (bySuffix sfx), expected: [ MinorSide ] }

  -- The trap that made the rule read the quality rather than the notes:
  -- q4 is [0,5,10,15] and 15 reduces to 3, which is not a minor third.
  assertEqual' "q4 is not filed as minor by its reduced ninth"
    { actual: map sideOf (bySuffix "q4"), expected: [ MajorSide ] }

  -- ------------------------------------------------------------------
  -- The axes joined — the six chords again, this time as chords.
  -- ------------------------------------------------------------------
  assertEqual' "strict + Basic in C is exactly the six closest chords"
    { actual: sort (map label (allowed cMajor strict Basic))
    , expected: [ "Am", "C", "Dm", "Em", "F", "G" ]
    }

  -- Independence of the axes, which is the whole reason there are two:
  -- widening one must not touch the other.
  assertEqual' "complexity does not change which roots are available"
    { actual: nub (sort (map _.root (allowed cMajor strict Extreme)))
    , expected: nub (sort (map _.root (allowed cMajor strict Basic)))
    }

  assertTrue' "freedom does not change which types are available"
    (nub (sort (map (suffixOf <<< _.chordType) (allowed cMajor freeFlow Medium)))
       == nub (sort (map (suffixOf <<< _.chordType) (allowed cMajor strict Medium))))

  log ("  " <> show (length (allowed cMajor freeFlow Extreme))
        <> " chords at free flow and extreme; "
        <> show (length (allowed cMajor strict Basic)) <> " at strict and basic")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | An area as a short readable token — `"C+"` major, `"A-"` minor — so a
-- | failure prints something a reader can check against the wheel.
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
