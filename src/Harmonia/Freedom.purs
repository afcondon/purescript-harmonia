-- | **How far a chord is allowed to roam from home — the distance axis.**
-- |
-- | `Harmonia.Palette` says which chord TYPES are in play; this says which
-- | ROOTS and which side of the wheel they may sit on. The two are independent
-- | by design, which is the point of having both: you can wander a long way
-- | using only triads, or stay on the tonic and stack thirteenths on it.
-- |
-- | ## Six rings, each one note from the last
-- |
-- | The obvious reading — "allow roots within n steps on the circle of fifths"
-- | — is WRONG, and measurably so: D, A and E are at fifths-distances 2, 3 and
-- | 4 from C and all three enter at the same ring, so no distance threshold
-- | produces the ordering. What Collett's *Circle of Colors* actually describes
-- | is a chain of relative- and parallel-minor moves spiralling outward, each
-- | of which changes exactly one note:
-- |
-- | ```
-- |   0   C  F  G    +  Am Dm Em      the tonic group and its relative minors
-- |   1   A  D  E                     parallel majors of those minors
-- |   2   Cm Fm Gm                    parallel minors of the tonic group
-- |   3   E♭ A♭ B♭                    relative majors of those
-- |   4   E♭m A♭m B♭m                 parallel minors of those
-- |   5   G♭ B  D♭  +  G♭m Bm D♭m     relative majors of those, and the rest
-- | ```
-- |
-- | Written out it collapses to something much smaller than the prose: every
-- | ring is the tonic group transposed by one of four offsets — 0, −3, +3, +6 —
-- | taken on one side of the wheel or the other. Eight groups of three, which
-- | is all twenty-four major and minor areas, and ring 5 reaches the tritone
-- | exactly as the source says Free Flow must.
-- |
-- | ## A key and its relative minor have the same rings
-- |
-- | They are the same wheel segment — the inner ring of a circle of fifths is
-- | the relative minors — so A minor's rings ARE C major's rings, in the same
-- | order. The mode changes which chord the walk STARTS on and nothing else,
-- | which fits what the source says about the key: *"You select one of 12 keys
-- | then major or minor, so you have 24 choices"* for the first chord, and
-- | everything after it walks from the chord before.
-- |
-- | ## What is measured and what is read
-- |
-- | The rings are transcribed from the paper and are as solid as anything here.
-- | `sideOf` is not: the source states the PRINCIPLE — chords of *"a major or
-- | more neutral function"* sit in the major segment, those of *"a more distant
-- | or minor function"* in the minor segments — and gives nine examples, which
-- | the rule below matches nine for nine. It has never been checked against a
-- | filtered menu, because every menu captured so far was at Free Flow, where
-- | nothing is filtered. Treat it as the reading it is.
-- |
-- | Full derivation: `docs/kb/reference/progressions-generator.md`.
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
  , tonicGroup
  , ring
  , areasUpTo
  , admits
  , Allowed
  , allowed
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, nub, range)
import Data.Array as Array
import Harmonia.Chord (Chord(..), Quality(..), Tension(..), qualityIntervals)
import Harmonia.Palette (ChordType(..), Level, typeOn, upTo)

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

-- | Which half of the wheel an area or a chord belongs to.
-- |
-- | Two-valued, though the source's own wording is three: *"a major or more
-- | NEUTRAL function"*. Neutral chords — the ones with no third at all — are
-- | drawn in the major segment, so for the purpose of deciding what freedom
-- | admits there are two sides and neutral is one of them.
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

-- | **Which side of the wheel a chord type belongs to.**
-- |
-- | The third decides it, and a minor seventh over a major third decides it the
-- | other way — a dominant is a *distant* function, which is why the source
-- | files plain `7` beside `m`, `m7` and `m9` rather than beside `M7`.
-- |
-- | Three things make this fiddlier than it sounds, and all three are why the
-- | rule reads the QUALITY rather than the realised pitch classes:
-- |
-- |   * `q4` is `[0, 5, 10, 15]`, and 15 reduces to 3 — which is not a minor
-- |     third, it is a ninth an octave up. Reading reduced pitch classes files
-- |     every four-note quartal chord as minor.
-- |   * A `Sharp 9` reduces to 3 for the same reason.
-- |   * A `Sus` removes the third, so a suspended chord is neutral however its
-- |     quality began.
-- |
-- | The non-tertian qualities are neutral by construction and say so first.
sideOf :: ChordType -> Side
sideOf ct@(ChordType t) = case t.quality of
  Quartal3 -> MajorSide
  Quartal4 -> MajorSide
  Mystic -> MajorSide
  _ ->
    let
      base = qualityIntervals t.quality
      suspended = Array.any isSus t.tensions
      Chord pcs = typeOn 0 ct
    in
      if suspended || not (elem 3 base || elem 4 base) then MajorSide
      else if elem 3 base then MinorSide
      else if elem 10 pcs then MinorSide
      else MajorSide
  where
  isSus = case _ of
    Sus _ -> true
    _ -> false

-- | The tonic and the two chords either side of it on the circle of fifths —
-- | in C, that is F, C and G. Every ring is this group transposed.
tonicGroup :: Int -> Array Int
tonicGroup t = map (\i -> mod (t + i + 12) 12) [ -7, 0, 7 ]

-- | **What one ring introduces.**
-- |
-- | The chain in the module header, stated as the four transpositions it
-- | actually is. A minor home resolves to its relative major first, since the
-- | two share a wheel segment and therefore share every ring.
ring :: Home -> Int -> Array Area
ring home n =
  let
    t = if home.minor then mod (home.tonic + 3) 12 else mod home.tonic 12
    g = tonicGroup t
    at off side = map (\r -> { root: mod (r + off + 12) 12, side }) g
  in
    case n of
      0 -> at 0 MajorSide <> at (-3) MinorSide
      1 -> at (-3) MajorSide
      2 -> at 0 MinorSide
      3 -> at 3 MajorSide
      4 -> at 3 MinorSide
      -- Free Flow "includes everything", and the three relative majors alone
      -- would leave their parallel minors unreachable — so the last ring is
      -- also where the chain closes over the remaining twenty-fourth.
      5 -> at 6 MajorSide <> at 6 MinorSide
      _ -> []

-- | Every area available AT a freedom level — cumulative, like complexity.
areasUpTo :: Home -> Freedom -> Array Area
areasUpTo home f =
  nub (concatMap (ring home) (range 0 (freedomIndex f)))

admits :: Home -> Freedom -> Area -> Boolean
admits home f a = elem a (areasUpTo home f)

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
