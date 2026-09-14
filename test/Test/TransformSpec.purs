-- | **Laws for the repositioning transforms** — `invert`, `transposeOctaves`
-- | and `refoot`.
-- |
-- | These three move a chord without changing it, and that sentence is the
-- | whole specification: **the note count and the pitch-class content are
-- | invariant.** Each has been written the obvious way at least once, and each
-- | obvious way lost a note — quietly, because a chord one note short does not
-- | look wrong, it just sounds thinner.
-- |
-- | They live here rather than beside their first caller because none of them
-- | knows anything but pitch. The bugs they had were musical bugs, not plumbing
-- | bugs, so the tests that catch them belong where every consumer inherits
-- | them.
-- |
-- | The corpus is the whole palette opened on every root — 480 voicings — plus
-- | the same chords in close position, so both a wide keyboard spacing and a
-- | tight one are covered. Spacing is exactly what separates the cases these
-- | transforms get wrong.
module Test.TransformSpec (runTransformTests) where

import Prelude

import Data.Array (all, filter, head, length, nub, sort)
import Data.Foldable (elem)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (defaults, openVoicing)
import Harmonia.Palette (palette, typeOn)
import Harmonia.Voicing
  ( Voicing(..), bassPitchClass, closeVoicing, invert, nextBassTone, refoot
  , slash, transposeOctaves, voicingMidi
  )
import Test.Assert (assertTrue')

-- | Every palette type on every root, voiced both ways.
corpus :: Array Voicing
corpus = do
  root <- Array.range 0 11
  ct <- palette
  let chord = typeOn root ct
  [ openVoicing defaults { root, chord }, closeVoicing { centre: 4 } chord ]

pcsOf :: Voicing -> Array Int
pcsOf = sort <<< nub <<< map (\m -> mod m 12) <<< voicingMidi

notes :: Voicing -> Array Int
notes = sort <<< voicingMidi

law :: String -> (Voicing -> Boolean) -> Effect Unit
law name holds = do
  let bad = filter (not <<< holds) corpus
  for_ (head bad) \v -> log ("      first failure: " <> show (notes v))
  assertTrue' (name <> " (" <> show (length bad) <> " of " <> show (length corpus) <> " failed)")
    (length bad == 0)

runTransformTests :: Effect Unit
runTransformTests = do
  log ("\n--- Harmonia.Voicing — repositioning laws over " <> show (length corpus) <> " voicings ---")

  -- The shared discipline, stated once per transform per direction.
  law "invert up keeps the note count" (\v -> length (notes (invert 1 v)) == length (notes v))
  law "invert down keeps the note count" (\v -> length (notes (invert (-1) v)) == length (notes v))
  law "invert up keeps the content" (\v -> pcsOf (invert 1 v) == pcsOf v)
  law "invert down keeps the content" (\v -> pcsOf (invert (-1) v) == pcsOf v)
  law "invert up then down keeps the content"
    (\v -> pcsOf (invert (-1) (invert 1 v)) == pcsOf v)
  -- Exactly one note moves: a positional comparison would be meaningless here,
  -- since sorting shifts every index when the bottom note goes to the top.
  law "invert moves exactly one note, no more"
    (\v -> length (voicingMidi v) < 2
           || length (Array.difference (notes v) (notes (invert 1 v))) == 1)

  law "an octave up moves every note by 12"
    (\v -> notes (transposeOctaves 1 v) == map (_ + 12) (notes v))
  law "an octave down moves every note by -12"
    (\v -> notes (transposeOctaves (-1) v) == map (_ - 12) (notes v))
  law "octaves round-trip" (\v -> notes (transposeOctaves (-1) (transposeOctaves 1 v)) == notes v)
  law "an octave keeps the content" (\v -> pcsOf (transposeOctaves 1 v) == pcsOf v)

  law "re-footing keeps the note count"
    (\v -> length (notes (refoot (nextBassTone 1 v) v)) == length (notes v))
  law "re-footing keeps the content"
    (\v -> pcsOf (refoot (nextBassTone 1 v) v) == pcsOf v)
  law "re-footing onto the current bass is the identity"
    (\v -> notes (refoot (bassPitchClass v) v) == notes v)
  law "re-footing puts the named tone lowest"
    (\v -> let pc = nextBassTone 1 v in bassPitchClass (refoot pc v) == pc)
  law "re-footing onto every sounding tone keeps the count"
    (\v -> all (\pc -> length (notes (refoot pc v)) == length (notes v)) (pcsOf v))
  law "re-footing onto every sounding tone keeps the content"
    (\v -> all (\pc -> pcsOf (refoot pc v) == pcsOf v) (pcsOf v))
  law "re-footing onto every sounding tone puts it lowest"
    (\v -> all (\pc -> bassPitchClass (refoot pc v) == pc) (pcsOf v))

  -- Walking the whole bass cycle must return the chord's own tones, in order,
  -- and get back to where it started.
  law "the bass cycle visits every sounding tone"
    (\v -> let n = length (pcsOf v)
               walk = Array.scanl (\pc _ -> nextBassTone 1 (refoot pc v)) (bassPitchClass v) (Array.range 1 n)
           in sort (nub walk) == pcsOf v)

  -- `slash` obeys the same invariants, and one more: it must not disturb the
  -- structure above the bass, which is the entire reason it exists beside
  -- `refoot`.
  law "slashing keeps the note count"
    (\v -> length (notes (slash (nextBassTone 1 v) v)) == length (notes v))
  law "slashing keeps the content"
    (\v -> pcsOf (slash (nextBassTone 1 v) v) == pcsOf v)
  law "slashing onto the current bass is the identity"
    (\v -> notes (slash (bassPitchClass v) v) == notes v)
  law "slashing puts the named tone lowest"
    (\v -> let pc = nextBassTone 1 v in bassPitchClass (slash pc v) == pc)
  law "slashing onto every sounding tone keeps count and content"
    (\v -> all (\pc -> length (notes (slash pc v)) == length (notes v)
                      && pcsOf (slash pc v) == pcsOf v) (pcsOf v))
  law "slashing onto every sounding tone puts it lowest"
    (\v -> all (\pc -> bassPitchClass (slash pc v) == pc) (pcsOf v))
  -- The distinguishing law: a chord with a real gap under it keeps that gap.
  law "slashing leaves the upper structure untouched when it can"
    (\v -> let pc = nextBassTone 1 v
               ups w = Array.drop 1 (notes w)
           in not (elem (bassPitchClass v) (map (\m -> mod m 12) (Array.drop 1 (notes v))))
              || ups (slash pc v) == ups v)

  log "  ✓ repositioning laws"
