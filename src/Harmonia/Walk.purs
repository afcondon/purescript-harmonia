-- | **Walking the chord tree — a generator in the Progressions family.**
-- |
-- | `Palette` says which chord types exist and `Freedom` says which roots are
-- | in reach. This is the part that moves: given a chord, which chords may
-- | follow it, and a seeded walk through them.
-- |
-- | ## What is exact and what is ours
-- |
-- | The source's generator is a **table**, one per chord type, stated as such by
-- | Collett: *"Each type of chord has its own tree of progressions that commonly
-- | (and sometimes not-so-commonly) follows this chord … an average of 65
-- | possible unique chords per tree."* The tables are grouped into complexity
-- | bands, and only the first band is derivable:
-- |
-- | > **Band 1 = the diatonic triads of the previous chord's own key, less the
-- | > tonic (a duplicate) and less the diminished one.** Five chords, always.
-- |
-- | Confirmed on three measured predecessors of two qualities, and it is
-- | natural minor rather than harmonic: the v of a minor chord is minor. That
-- | rule is `band1`, and it is exact.
-- |
-- | **Bands 2 to 5 are not derivable and are not implemented.** Measured
-- | 2026-09-14 over 4,116 transitions, band-1 offsets take 60–76% of the
-- | successors of a tertian chord against 41% by chance — real, and nothing
-- | like exclusive, because bands 2–5 are contributing the rest. The tables
-- | are what closes that gap. It was worth
-- | checking rather than assuming: level 2's documented contribution is
-- | *"parallel minors & majors"*, and band 2 does contain the parallels of band
-- | 1 — but only seven of ten of them, and it reaches offsets band 1 never
-- | touches. Three complete menus are not enough to fit a rule to that, and
-- | guessing would be exactly the wrong move for a project whose standing rule
-- | is to copy the software rather than the theory. They want measuring, and
-- | `Tree` below is the shape they will arrive in.
-- |
-- | ## So what does complexity do here?
-- |
-- | **A local policy, not a measurement, and flagged as such.** Band 1 fixes
-- | which ROOTS a chord may move to and whether each is major or minor; this
-- | module then lets complexity choose how richly to colour that root, from any
-- | palette type on the right side of the wheel.
-- |
-- | At `Basic` that is exactly the source — band 1 is triads and the palette
-- | offers only triads, so the walk is the app's own. Above `Basic` it is a
-- | deliberate divergence: **the app widens the root set at higher complexity
-- | and this widens the vocabulary instead.** The music is good and the roots
-- | are right; the reach is not. Replacing this with measured tables is the
-- | next real piece of work.
-- |
-- | ## Seeded, because Harmonia is stateless
-- |
-- | The randomness comes in as a `Seed` rather than out of an effect, which
-- | keeps the layer pure and buys the property the sampling side depends on:
-- | **a seeded progression is re-runnable.** The source gets that from pads
-- | that hold still; we get it from saving a number. Which is what lets a
-- | generated progression be treated exactly like a swept transect — re-run at
-- | a different resolution, recut, resampled.
module Harmonia.Walk
  ( Seed
  , seed
  , nextSeed
  , pick
  , Setting
  , defaults
  , band1
  , successors
  , walk
  , Tree
  ) where

import Prelude

import Data.Array (cons, filter, index, length, notElem)
import Data.Int.Bits (shl, xor, zshr, (.&.))
import Data.Maybe (Maybe(..))
import Harmonia.Freedom (Allowed, Freedom, Home, Side(..), admits, freedom, sideOf)
import Harmonia.Chord (Quality(..))
import Harmonia.Palette (ChordType(..), Level(..), majorTriad, minorTriad, upTo)

-- ---------------------------------------------------------------------------
-- Randomness, supplied rather than drawn
-- ---------------------------------------------------------------------------

newtype Seed = Seed Int

derive instance eqSeed :: Eq Seed

instance showSeed :: Show Seed where
  show (Seed n) = "Seed " <> show n

-- | Zero is excluded because xorshift stalls there — it is the one input that
-- | would make a "random" walk emit the same chord forever, silently.
seed :: Int -> Seed
seed n = Seed (if n == 0 then 123456789 else n)

-- | xorshift32. Chosen over a multiplicative generator because JavaScript's
-- | bitwise operators are exact on 32 bits while its multiplication is not —
-- | an LCG's `a * s` overflows the double's integer range and quietly stops
-- | being the generator it claims to be.
nextSeed :: Seed -> Seed
nextSeed (Seed s) =
  let
    a = s `xor` (s `shl` 13)
    b = a `xor` (a `zshr` 17)
    c = b `xor` (b `shl` 5)
  in
    Seed c

pick :: forall a. Seed -> Array a -> Maybe { value :: a, seed :: Seed }
pick s xs =
  let
    s'@(Seed n) = nextSeed s
  in
    if length xs == 0 then Nothing
    else map (\v -> { value: v, seed: s' })
           (index xs (mod (n .&. 2147483647) (length xs)))

-- ---------------------------------------------------------------------------
-- The tree
-- ---------------------------------------------------------------------------

-- | One edge of a chord's tree: how far the root moves, and which side of the
-- | wheel it lands on. The shape measured tables will arrive in — a band is an
-- | `Array Tree`, and a chord type's tree is five of those.
type Tree = { offset :: Int, side :: Side }

-- | **Band 1, derived and exact.**
-- |
-- | The diatonic triads of the chord's own key, less the tonic and less the
-- | diminished. For a major chord that is ii, iii, IV, V, vi; for a minor one,
-- | III, iv, v, VI, VII of the NATURAL minor — the v stays minor, which is what
-- | the measurement shows and what distinguishes this from the harmonic minor
-- | reading.
band1 :: ChordType -> Array Tree
band1 ct = case quartalish ct of
  true -> byFourths
  false -> byDegree ct

-- | **A chord built of fourths moves in fourths.**
-- |
-- | Measured over 4,116 transitions: a quartal or Mystic predecessor lands on
-- | a root one to four fourths away about half the time, against a quarter by
-- | chance — and lands on a *diatonic* offset BELOW chance, where every tertian
-- | predecessor is well above it. They are the only predecessors in the whole
-- | palette that do not follow the diatonic rule.
-- |
-- | Which is what Collett says of them, in the one part of the paper that is
-- | about behaviour rather than spelling: the Mystic's *"quartal construction
-- | creates tonal ambiguity; like other quartals and even the °7, we're never
-- | sure which of the many possible chords it will resolve to."*
-- |
-- | Offsets are one, two, three and four fourths up, and one down — the five
-- | most used, in that order, for a `q4` predecessor. **The SIDE is not
-- | measured**: quartal successors carry a minor third 24–31% of the time
-- | against 40% overall, which is a lean and not a rule, so both are offered
-- | and the choice is left downstream.
byFourths :: Array Tree
byFourths = do
  offset <- [ 5, 10, 3, 8, 7 ]
  side <- [ MajorSide, MinorSide ]
  pure { offset, side }

-- | Quartals and the Mystic: the chords with no third for a key to be built
-- | on. `sideOf` files them major, which is right for admission and wrong here.
quartalish :: ChordType -> Boolean
quartalish (ChordType t) = case t.quality of
  Quartal3 -> true
  Quartal4 -> true
  Mystic -> true
  _ -> false

byDegree :: ChordType -> Array Tree
byDegree ct = case sideOf ct of
  MinorSide ->
    [ { offset: 3, side: MajorSide }   -- III
    , { offset: 5, side: MinorSide }   -- iv
    , { offset: 7, side: MinorSide }   -- v, natural minor
    , { offset: 8, side: MajorSide }   -- VI
    , { offset: 10, side: MajorSide }  -- VII
    ]
  MajorSide ->
    [ { offset: 2, side: MinorSide }   -- ii
    , { offset: 4, side: MinorSide }   -- iii
    , { offset: 5, side: MajorSide }   -- IV
    , { offset: 7, side: MajorSide }   -- V
    , { offset: 9, side: MinorSide }   -- vi
    ]

-- ---------------------------------------------------------------------------
-- Settings and the walk
-- ---------------------------------------------------------------------------

-- | `duplicates` mirrors the source's own switch: with it off the walk avoids
-- | chords it has already used, *"without adding any duplicates to the list"*.
type Setting =
  { home :: Home
  , freedom :: Freedom
  , complexity :: Level
  , duplicates :: Boolean
  }

defaults :: Setting
defaults =
  { home: { tonic: 0, minor: false }
  , freedom: freedom 2
  , complexity: Medium
  , duplicates: false
  }

-- | **What may follow this chord**, with both axes applied: band 1 for where
-- | the root may go, freedom for whether it is in reach, complexity for how
-- | richly it may be coloured.
successors :: Setting -> Allowed -> Array Allowed
successors set current =
  let
    reachable =
      filter (\a -> admits set.home set.freedom a)
        (map (\t -> { root: mod (current.root + t.offset) 12, side: t.side })
          (band1 current.chordType))
  in
    do
      a <- reachable
      ct <- filter (\t -> sideOf t == a.side) (upTo set.complexity)
      pure { root: a.root, chordType: ct }

-- | **A progression of `n` chords**, starting on the home triad.
-- |
-- | The first chord is the key — *"You select one of 12 keys then major or
-- | minor, so you have 24 choices"* — and every chord after it is drawn from
-- | the one before. Deterministic in the seed, so the same number gives the
-- | same progression for ever.
-- |
-- | If nothing is reachable the walk returns home rather than stalling or
-- | repeating, which is also what the source does when it runs out of room:
-- | *"ping pong backwards to the root"*.
walk :: Setting -> Seed -> Int -> Array Allowed
walk set s n =
  let start = home set
  in cons start (go s start (max 0 (n - 1)) [ start ])
  where
  go s0 current left chosen =
    if left <= 0 then []
    else
      let
        options = successors set current
        fresh =
          if set.duplicates then options
          else case filter (\o -> notElem (key o) (map key chosen)) options of
            [] -> options
            ok -> ok
      in
        case pick s0 fresh of
          Nothing -> [ home set ]
          Just r ->
            cons r.value (go r.seed r.value (left - 1) (cons r.value chosen))

  key a = { root: a.root, sfx: suffixOf a.chordType }

home :: Setting -> Allowed
home set =
  { root: mod set.home.tonic 12
  , chordType: if set.home.minor then minorTriad else majorTriad
  }

suffixOf :: ChordType -> String
suffixOf (ChordType t) = t.suffix
