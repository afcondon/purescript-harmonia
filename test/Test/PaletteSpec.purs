-- | Tests for `Harmonia.Palette` — the graded chord vocabulary.
-- |
-- | **What this suite is for.** The palette is a TRANSCRIPTION: forty chord
-- | types read off a published chart, a paper and an app's own menus, then
-- | re-expressed in Harmonia's `Quality` and `Tension`. A transcription can be
-- | wrong in a way that compiles perfectly — `Dom7 <> [Sharp 5]` is a legal
-- | expression and it is not a `7#5`, because `Sharp` ADDS a tone rather than
-- | moving one. So every type is pinned to the pitch classes it must produce,
-- | and the file below is the only place the vocabulary is actually asserted.
-- |
-- | Where the source publishes a semitone formula the expectation is copied
-- | from it; the extended chords are derived from the source's own extension
-- | table (♭9 = 13, 9 = 14, ♯9 = 15, 11 = 17, ♯11 = 18, 13 = 21 semitones above
-- | the root, reduced mod 12). Both are written out as literals rather than
-- | computed, because a computed expectation would share whatever mistake the
-- | implementation has.
module Test.PaletteSpec
  ( runPaletteTests
  ) where

import Prelude

import Data.Array (concatMap, filter, head, index, length, mapMaybe, nub, range, sort)
import Data.Foldable (all, any, for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord (Chord(..))
import Harmonia.Palette (ChordType(..), Level(..), atLevel, palette, typeIntervals, typeOn, upTo)
import Test.Assert (assertEqual', assertTrue')

runPaletteTests :: Effect Unit
runPaletteTests = do
  log ""
  log "--- Harmonia.Palette — the vocabulary, pinned ---"

  -- ------------------------------------------------------------------
  -- Every type, on root 0, against the pitch classes it must produce.
  -- ------------------------------------------------------------------
  for_ expected \(Tuple sfx pcs) ->
    case find sfx of
      Nothing ->
        assertTrue' ("palette is missing a type called " <> show sfx) false
      Just t ->
        expectChord (sfx <> " on C") pcs (typeIntervals t)

  -- Nothing in the palette is left unpinned: a type added without an
  -- expectation here would otherwise be tested by nothing at all.
  assertEqual' "every palette entry has an expectation"
    { actual: length palette, expected: length expected }

  -- The source states forty chord types. Agreement is a check on the
  -- transcription, not a proof of it — two of the forty are inferred from
  -- observed app output rather than from any document.
  assertEqual' "forty chord types"
    { actual: length palette, expected: 40 }

  -- ------------------------------------------------------------------
  -- The levels are CUMULATIVE. A generator that read them as disjoint
  -- bands would emit nothing but towers at the top setting, which is not
  -- what the source does.
  -- ------------------------------------------------------------------
  assertEqual' "Basic contributes exactly the two triads"
    { actual: map suffixOf (atLevel Basic), expected: [ "", "m" ] }

  assertEqual' "upTo Basic is only Basic"
    { actual: length (upTo Basic), expected: length (atLevel Basic) }

  assertEqual' "upTo Extreme is the whole palette"
    { actual: length (upTo Extreme), expected: length palette }

  assertTrue' "upTo High still contains the plain major triad"
    (elemSuffix "" (upTo High))

  assertTrue' "upTo Medium does NOT yet contain a thirteenth"
    (not (elemSuffix "13" (upTo Medium)))

  -- Each level is a superset of the one below it, which is the whole claim
  -- of "cumulative" and the thing most likely to break silently.
  for_ [ Tuple Basic Low, Tuple Low Medium, Tuple Medium High, Tuple High Extreme ]
    \(Tuple lo hi) ->
      assertTrue'
        ("upTo " <> show hi <> " contains everything in upTo " <> show lo)
        (all (\t -> elemSuffix (suffixOf t) (upTo hi)) (upTo lo))

  -- ------------------------------------------------------------------
  -- Transposition. The palette is addressed by ROOT, not by scale degree —
  -- at full harmonic freedom there is no key left for a numeral to be
  -- relative to — so realising on root n must be realising on root 0
  -- shifted by n, for every type and every root.
  -- ------------------------------------------------------------------
  for_ palette \t ->
    for_ (range 1 11) \r ->
      assertEqual'
        (suffixOf t <> " transposes to root " <> show r)
        { actual: pcsOf (typeOn r t)
        , expected: sort (nub (map (\x -> mod (x + r) 12) (pcsOf (typeIntervals t))))
        }

  -- ------------------------------------------------------------------
  -- Distinctness, RECORDED rather than required.
  --
  -- Two chord types sharing a pitch-class set on one root is not
  -- automatically a fault — a 6th chord and its relative minor 7th are the
  -- same notes and different chords, and that ambiguity is real music
  -- rather than a bug. But an UNEXPECTED collision means a transcription
  -- slip, so the list is a golden: it may not grow without someone looking.
  -- ------------------------------------------------------------------
  assertEqual' "chord types sharing a pitch-class set on C"
    { actual: collisions, expected: knownCollisions }

  log ("  " <> show (length palette) <> " chord types pinned, "
        <> show (length palette * 11) <> " transpositions checked")

-- ---------------------------------------------------------------------------
-- The expectations
-- ---------------------------------------------------------------------------

-- | Every chord type by the suffix the SOURCE prints, and the pitch classes it
-- | must realise to on root C. Ordered as the palette is, level by level.
expected :: Array (Tuple String (Array Int))
expected =
  -- Basic
  [ Tuple "" [ 0, 4, 7 ]
  , Tuple "m" [ 0, 3, 7 ]
  -- Low. Note the shells: a seventh with the fifth removed, which is fewer
  -- notes and less certainty rather than more colour.
  , Tuple "sus2" [ 0, 2, 7 ]
  , Tuple "sus4" [ 0, 5, 7 ]
  , Tuple "aug" [ 0, 4, 8 ]
  , Tuple "dim" [ 0, 3, 6 ]
  , Tuple "7(no5)" [ 0, 4, 10 ]
  , Tuple "m7(no5)" [ 0, 3, 10 ]
  -- Medium
  , Tuple "M6" [ 0, 4, 7, 9 ]
  , Tuple "m6" [ 0, 3, 7, 9 ]
  , Tuple "7" [ 0, 4, 7, 10 ]
  , Tuple "m7" [ 0, 3, 7, 10 ]
  , Tuple "°7" [ 0, 3, 6, 9 ]
  , Tuple "ø7" [ 0, 3, 6, 10 ]
  , Tuple "7sus4" [ 0, 5, 7, 10 ]
  , Tuple "M7" [ 0, 4, 7, 11 ]
  -- The augmented fifth has to come from the QUALITY: `Sharp 5` would add an
  -- 8 beside the natural 7 rather than replacing it.
  , Tuple "7#5" [ 0, 4, 8, 10 ]
  , Tuple "M7b5" [ 0, 4, 6, 11 ]
  , Tuple "mM7" [ 0, 3, 7, 11 ]
  -- High. 9 = 14 semitones = 2.
  , Tuple "9" [ 0, 2, 4, 7, 10 ]
  , Tuple "9b5" [ 0, 2, 4, 6, 10 ]
  , Tuple "m9" [ 0, 2, 3, 7, 10 ]
  , Tuple "m9b5" [ 0, 2, 3, 6, 10 ]
  , Tuple "M7+9" [ 0, 2, 4, 7, 11 ]
  , Tuple "M6+9" [ 0, 2, 4, 7, 9 ]
  , Tuple "m6+9" [ 0, 2, 3, 7, 9 ]
  , Tuple "mM9" [ 0, 2, 3, 7, 11 ]
  , Tuple "9sus4" [ 0, 2, 5, 7, 10 ]
  , Tuple "9sus4+13" [ 0, 2, 5, 7, 9, 10 ]
  , Tuple "M9b5" [ 0, 2, 4, 6, 11 ]
  -- Extreme. 11 = 17 = 5, ♯11 = 18 = 6, 13 = 21 = 9.
  , Tuple "9#11" [ 0, 2, 4, 6, 7, 10 ]
  , Tuple "m11" [ 0, 2, 3, 5, 7, 10 ]
  , Tuple "M11" [ 0, 2, 4, 5, 7, 11 ]
  , Tuple "M9#11" [ 0, 2, 4, 6, 7, 11 ]
  , Tuple "°11" [ 0, 2, 3, 5, 6, 9 ]
  , Tuple "13" [ 0, 2, 4, 7, 9, 10 ]
  , Tuple "M9+13" [ 0, 2, 4, 7, 9, 11 ]
  -- Not tertian at all: stacked fourths, and Scriabin's six-note Mystic.
  , Tuple "q3" [ 0, 5, 10 ]
  , Tuple "q4" [ 0, 3, 5, 10 ]
  , Tuple "Mystic" [ 0, 2, 4, 6, 9, 10 ]
  ]

-- | Pairs of types that realise to identical pitch classes on C. Recorded, not
-- | forbidden — but it may not grow silently.
knownCollisions :: Array (Tuple String String)
knownCollisions = []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

suffixOf :: ChordType -> String
suffixOf (ChordType t) = t.suffix

pcsOf :: Chord -> Array Int
pcsOf (Chord pcs) = pcs

find :: String -> Maybe ChordType
find sfx = head (filter (\t -> suffixOf t == sfx) palette)

elemSuffix :: String -> Array ChordType -> Boolean
elemSuffix sfx = any (\t -> suffixOf t == sfx)

-- | Every unordered pair of palette entries whose pitch classes on C agree.
-- | Quadratic over forty entries, which is nothing, and far clearer than a
-- | grouping — the output is the thing the golden compares.
collisions :: Array (Tuple String String)
collisions =
  let ix = range 0 (length palette - 1)
  -- `filter (_ > i)` rather than `range (i + 1) n`: a PureScript range counts
  -- DOWN when its start exceeds its end, so the obvious spelling silently
  -- yields every pair twice at the last index.
  in concatMap (\i -> mapMaybe (pairAt i) (filter (_ > i) ix)) ix
  where
  pairAt i j = do
    a <- index palette i
    b <- index palette j
    if pcsOf (typeIntervals a) == pcsOf (typeIntervals b)
      then Just (Tuple (suffixOf a) (suffixOf b))
      else Nothing

expectChord :: String -> Array Int -> Chord -> Effect Unit
expectChord label want (Chord actual) =
  assertEqual' label { actual, expected: want }
