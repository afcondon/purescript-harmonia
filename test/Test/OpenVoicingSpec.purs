-- | Tests for `Harmonia.OpenVoicing`.
-- |
-- | The module's claim is that it is a DIFFERENT strategy from `voiceLead`,
-- | not a refinement of it: bass pinned to the root, notes spread by octaves,
-- | and the TOP line smoothed rather than total motion minimised. So the suite
-- | asserts the two constraints that must never fail, and then MEASURES the
-- | claim against `voiceLead` on the same progression — because "smoother top
-- | line" is a comparison, and a test that did not compare would be asserting
-- | an adjective.
module Test.OpenVoicingSpec
  ( runOpenVoicingTests
  ) where

import Prelude

import Data.Array (all, cons, filter, index, length, nub, scanl, sort, zipWith)
import Data.Array as Array
import Data.Foldable (for_, sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (Rooted, defaults, openVoicing, playOpen, span, topNote)
import Harmonia.Palette (ChordType(..), palette, typeOn)
import Harmonia.Voicing (Voicing(..), voiceLead, voicingMidi)
import Test.Assert (assertEqual', assertTrue')

runOpenVoicingTests :: Effect Unit
runOpenVoicingTests = do
  log ""
  log "--- Harmonia.OpenVoicing — bass pinned, top line smoothed ---"

  -- ------------------------------------------------------------------
  -- The two invariants, over every chord type on every root: 480 chords.
  -- These are the constraints the source states, and either failing means
  -- the voicing is not the thing it claims to be.
  -- ------------------------------------------------------------------
  let everything = do
        t <- palette
        r <- Array.range 0 11
        pure { root: r, chord: typeOn r t, name: suffixOf t <> "/" <> show r }

  for_ everything \c -> do
    let v = openVoicing defaults { root: c.root, chord: c.chord }
    assertEqual' (c.name <> ": the root is the bass")
      { actual: map (\n -> mod n 12) (Array.head (voicingMidi v))
      , expected: Just (mod c.root 12)
      }
    assertEqual' (c.name <> ": the pitch classes are the chord's")
      { actual: sort (nub (map (\n -> mod n 12) (voicingMidi v)))
      , expected: pcsOf c.chord
      }

  -- Nothing comes back thinner than the padding floor. A triad voiced as
  -- three notes beside a five-note neighbour is the texture difference the
  -- source pads to remove.
  assertTrue' "no voicing is thinner than minTones"
    (all (\c -> length (voicingMidi (openVoicing defaults { root: c.root, chord: c.chord }))
                  >= defaults.minTones)
         everything)

  -- Open means open: spread over more than one octave, which is the whole
  -- difference from close position.
  assertTrue' "every voicing spans more than an octave"
    (all (\c -> span (openVoicing defaults { root: c.root, chord: c.chord }) > 12)
         everything)

  -- ------------------------------------------------------------------
  -- The claim, measured.
  --
  -- A progression of four-note sevenths, so `voiceLead` is working at its
  -- best: it falls back to close position when the voice count changes, and
  -- comparing against a fallback would be comparing against nothing.
  -- ------------------------------------------------------------------
  let ours = playOpen defaults progression
      theirs = minMotion (fromMaybe (Voicing []) (Array.head ours)) progression
      ourTop = topMotion ours
      theirTop = topMotion theirs
      ourAll = totalMotion ours
      theirAll = totalMotion theirs

  log ("  voiced open: " <> show (map voicingMidi ours))
  log ("  top-note motion   open " <> show ourTop <> " vs voiceLead " <> show theirTop)
  log ("  total motion      open " <> show ourAll <> " vs voiceLead " <> show theirAll)
  -- The raw total overstates the difference, and saying so is the honest
  -- reading: this strategy PINS the bass to each chord's root, so the bass
  -- must travel the root motion of the progression — 28 semitones here, before
  -- a single upper voice has moved. `voiceLead` is free to leave it where it
  -- is. Comparing what the two strategies actually choose means comparing the
  -- voices above the bass.
  log ("  bass motion       open " <> show (bassMotion ours)
        <> " (mandatory) vs voiceLead " <> show (bassMotion theirs))
  log ("  inner motion      open " <> show (ourAll - bassMotion ours)
        <> " vs voiceLead " <> show (theirAll - bassMotion theirs))

  assertTrue' "the open voicing moves the top note less than voiceLead does"
    (ourTop < theirTop)

  -- The other half of the claim, and the reason both belong in the library:
  -- ours is NOT better, it is different. It buys a smoother top line by
  -- spending motion in the inner voices, which is exactly what voiceLead is
  -- built to avoid.
  assertTrue' "and it pays for that in total motion"
    (ourAll >= theirAll)

  -- The bass says the harmony throughout, which voice-leading cannot promise.
  assertEqual' "every bass in the progression is its chord's root"
    { actual: zipWith (\v r -> map (\n -> mod n 12) (Array.head (voicingMidi v)) == Just (mod r.root 12))
                ours progression
    , expected: map (const true) progression
    }

  log ("  " <> show (length everything) <> " chords checked for bass and content")

-- ---------------------------------------------------------------------------
-- The comparison progression
-- ---------------------------------------------------------------------------

-- | `CM7 – Am7 – Dm7 – G7 – CM7`. Four notes throughout, ordinary enough that
-- | nobody can claim the result was engineered by picking exotic chords.
progression :: Array Rooted
progression =
  Array.mapMaybe identity
    [ rooted 0 "M7", rooted 9 "m7", rooted 2 "m7", rooted 7 "7", rooted 0 "M7" ]
  where
  -- Through the palette by the suffix the source prints, so the comparison
  -- runs on the actual vocabulary rather than on chords assembled here.
  rooted r sfx = do
    t <- Array.head (filter (\ct -> suffixOf ct == sfx) palette)
    pure { root: r, chord: typeOn r t }

-- | `voiceLead` chained from the same opening voicing — the fair comparison.
minMotion :: Voicing -> Array Rooted -> Array Voicing
minMotion v0 rs = case Array.uncons rs of
  Nothing -> []
  Just { tail: rest } -> cons v0 (scanl (\prev r -> voiceLead prev r.chord) v0 rest)

topMotion :: Array Voicing -> Int
topMotion vs = motionOver (Array.mapMaybe topNote vs)

-- | How far the lowest voice travels. Mandatory for the open strategy, which
-- | pins it to the root; free for `voiceLead`, which does not.
bassMotion :: Array Voicing -> Int
bassMotion vs = motionOver (Array.mapMaybe (Array.head <<< voicingMidi) vs)

totalMotion :: Array Voicing -> Int
totalMotion vs =
  sum (Array.mapMaybe pairMotion (Array.range 0 (length vs - 2)))
  where
  pairMotion i = do
    a <- index vs i
    b <- index vs (i + 1)
    let xs = voicingMidi a
        ys = voicingMidi b
    if length xs /= length ys then Nothing
      else Just (sum (zipWith (\p q -> abs (p - q)) xs ys))

motionOver :: Array Int -> Int
motionOver xs =
  sum (Array.mapMaybe step (Array.range 0 (length xs - 2)))
  where
  step i = do
    a <- index xs i
    b <- index xs (i + 1)
    pure (abs (a - b))

abs :: Int -> Int
abs n = if n < 0 then negate n else n

suffixOf :: ChordType -> String
suffixOf (ChordType t) = t.suffix

pcsOf :: Chord -> Array Int
pcsOf (Chord pcs) = sort (nub pcs)
