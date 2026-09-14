-- | **How far a chord is allowed to roam from home — the distance axis.**
-- |
-- | `Harmonia.Palette` says which chord TYPES are in play; this says which
-- | ROOTS they may sit on. The two are independent by design, which is the
-- | point of having both: you can wander a long way on triads, or stay on the
-- | tonic and stack thirteenths.
-- |
-- | ## The rule, measured
-- |
-- | A chord has a POSITION on the circle of fifths, and freedom is a symmetric
-- | window around the tonic:
-- |
-- | ```
-- |   position = fifths-distance from the tonic of
-- |                the root            -- for a chord with a major third or none
-- |                the root + 3        -- for a chord with a MINOR third
-- |
-- |   admitted at freedom n   iff   |position| <= n + 1
-- | ```
-- |
-- | A minor chord sits at its RELATIVE MAJOR's position, which is exactly how
-- | the circle of fifths is drawn — relative minors on the inner ring of the
-- | same wedge. So a "segment" of the wheel is a major chord and its relative
-- | minor together, and each freedom level opens one more segment either side.
-- | That is the manual's own sentence: *"Each level of freedom expands the
-- | scope of the progressions by another segment of the circle of fifths."*
-- |
-- | Freedom 0 is therefore ±1: C, F and G with Am, Dm and Em — *"these six
-- | chords are the ones most closely related to C major tonality"*. Freedom 5
-- | is ±6, which reaches the tritone and so is everything.
-- |
-- | ## An earlier reading of this was wrong
-- |
-- | Collett's paper contains a passage walking outward from the tonic — add the
-- | relative minors, then the majors on their roots, then the parallel minors,
-- | and so on — and it was read here as a description of the freedom levels. It
-- | is not; it is compositional advice about the wheel, and it predicts that D,
-- | A and E all arrive together at level 1. **Measured, they do not**: at
-- | freedom 1 in C, D is admitted and A and E are not, which is a fifths
-- | window of ±2 and nothing else. The prose cost a correct first reading.
-- |
-- | ## What the third decides
-- |
-- | Only the third. A chord carrying a minor third is a minor-area chord; every
-- | other chord — including dominants, suspensions, augmenteds and quartals —
-- | is a major-area chord.
-- |
-- | This contradicts the manual, which says of the *display* wheel that chords
-- | *"with a more distant or minor function are shown in the minor segments
-- | (such as m, m7, m9, 7 etc.)"* — filing plain `7` with the minors. For
-- | DISPLAY that may well be true. For admission it is not: at freedom 1 in C,
-- | `A7` is refused while `Am7` is offered, and A major is out of the window
-- | while A minor (at C's position) is in. Four measured discriminators settle
-- | it — `A7`, `Asus4` and `Aq3` refused, `A°7` and `Aø7` offered.
-- |
-- | All of it verified against a real Replace menu: 94 chords at Key C major,
-- | Freedom 1, Complexity Extreme, with no mismatches. See `FreedomSpec` for
-- | the golden and `docs/kb/reference/progressions-generator.md` for the
-- | derivation.
module Harmonia.Freedom
  ( Freedom
  , freedom
  , freedomIndex
  , strict
  , freeFlow
  , Side(..)
  , Area
  , Home
  , sideOf
  , fifths
  , position
  , ring
  , areasUpTo
  , admits
  , Allowed
  , allowed
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, range)
import Data.Array as Array
import Harmonia.Chord (Tension(..), qualityIntervals)
import Harmonia.Palette (ChordType(..), Level, upTo)

-- | 0 (strict) to 5 (free flow). A number rather than named levels because the
-- | source presents it as one — unlike complexity, whose levels it names.
newtype Freedom = Freedom Int

derive instance eqFreedom :: Eq Freedom
derive instance ordFreedom :: Ord Freedom

instance showFreedom :: Show Freedom where
  show (Freedom n) = "Freedom " <> show n

-- | Clamped rather than partial: a freedom of 9 is Free Flow, not an error, and
-- | there is nothing a caller could usefully do with a `Maybe` here.
freedom :: Int -> Freedom
freedom n = Freedom (clamp 0 5 n)

freedomIndex :: Freedom -> Int
freedomIndex (Freedom n) = n

strict :: Freedom
strict = Freedom 0

freeFlow :: Freedom
freeFlow = Freedom 5

-- | Which half of a wheel segment a chord sits in — the outer ring of majors
-- | or the inner ring of relative minors.
data Side = MajorSide | MinorSide

derive instance eqSide :: Eq Side
derive instance ordSide :: Ord Side

instance showSide :: Show Side where
  show = case _ of
    MajorSide -> "MajorSide"
    MinorSide -> "MinorSide"

-- | A tonal area: a root, and the side of the wheel it sits on. This is the
-- | unit freedom works in — not a chord, which is why the axes are independent.
type Area = { root :: Int, side :: Side }

-- | Where home is. `minor` selects the starting chord, not the rings.
type Home = { tonic :: Int, minor :: Boolean }

-- | **Which side of a segment a chord type belongs to. The third decides.**
-- |
-- | A minor third puts the chord in the minor ring; anything else — a major
-- | third, or no third at all — puts it in the major ring. Dominants included,
-- | which is the measured result and not the documented one: see the module
-- | header.
-- |
-- | It reads the QUALITY rather than the realised pitch classes, and has to.
-- | Three traps otherwise:
-- |
-- |   * `q4` is `[0, 5, 10, 15]`, and 15 reduces to 3 — not a minor third but
-- |     a ninth an octave up. Reading reduced pitch classes files every
-- |     four-note quartal chord as minor, and `Aq3` being REFUSED at freedom 1
-- |     in C is the measurement that says quartals are major-side.
-- |   * A `Sharp 9` reduces to 3 for the same reason.
-- |   * A `Sus` replaces the third, so a suspended chord has none — and
-- |     `Asus4` is refused where `Am` is offered, so no third means major side.
sideOf :: ChordType -> Side
sideOf (ChordType t) =
  let base = qualityIntervals t.quality
      suspended = Array.any isSus t.tensions
  in if not suspended && elem 3 base then MinorSide else MajorSide
  where
  isSus = case _ of
    Sus _ -> true
    _ -> false

-- | **Position on the circle of fifths**, as the representative of smallest
-- | magnitude: `G` is +1 from C, `F` is −1, and the tritone is +6.
fifths :: Int -> Int -> Int
fifths tonic pc =
  let d = mod ((pc - tonic) * 7) 12
  in if d > 6 then d - 12 else d

-- | Where an area sits relative to home. A minor area is read at its RELATIVE
-- | MAJOR, because that is the wedge of the wheel it shares — and a minor HOME
-- | is read the same way, for the same reason.
position :: Home -> Area -> Int
position home a =
  let t = if home.minor then mod (home.tonic + 3) 12 else mod home.tonic 12
      r = case a.side of
            MajorSide -> mod a.root 12
            MinorSide -> mod (a.root + 3) 12
  in fifths t r

-- | **What one freedom level introduces** — the segment at each end of the
-- | window. Kept separate from `areasUpTo` because it is what a wheel draws:
-- | rings of a spiral, one colour each.
ring :: Home -> Int -> Array Area
ring home n =
  filter (\a -> abs (position home a) == n + 1) everyArea
  where
  abs k = if k < 0 then negate k else k

-- | All twenty-four major and minor areas.
everyArea :: Array Area
everyArea = do
  r <- range 0 11
  side <- [ MajorSide, MinorSide ]
  pure { root: r, side }

-- | Every area available AT a freedom level — cumulative, like complexity.
areasUpTo :: Home -> Freedom -> Array Area
areasUpTo home f =
  filter (\a -> abs (position home a) <= freedomIndex f + 1) everyArea
  where
  abs k = if k < 0 then negate k else k

admits :: Home -> Freedom -> Area -> Boolean
admits home f a = abs (position home a) <= freedomIndex f + 1
  where
  abs k = if k < 0 then negate k else k

-- | A chord type on a root — what a generator picks, and what freedom filters.
type Allowed = { root :: Int, chordType :: ChordType }

-- | **The two axes, joined.** Every chord this key admits at this distance and
-- | this complexity.
-- |
-- | In C at `strict` and `Basic` this is the source's own opening sentence,
-- | executable: C, F and G major and A, D and E minor — *"these six chords are
-- | the ones most closely related to C major tonality"*.
allowed :: Home -> Freedom -> Level -> Array Allowed
allowed home f lv =
  let areas = areasUpTo home f
  in concatMap
       (\ct ->
          map (\a -> { root: a.root, chordType: ct })
            (filter (\a -> a.side == sideOf ct) areas))
       (upTo lv)
