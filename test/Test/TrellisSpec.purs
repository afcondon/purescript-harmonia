-- | `Harmonia.Trellis` — the path through a progression with alternatives.
-- |
-- | The laws are cheap; the MEASUREMENT is the point. A dial claiming to make
-- | the joins smoother is only worth having if the joins get smoother, so the
-- | suite prints the mean path motion at each setting over many seeds. If
-- | `Smooth` is not well below `Loose` the dial is decoration.
module Test.TrellisSpec (runTrellisTests) where

import Prelude

import Data.Array (all, catMaybes, index, length, mapWithIndex, nub, range, sort, zipWith)
import Data.Array as Array
import Data.Foldable (and, for_, sum)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertTrue')

import Harmonia.OpenVoicing (defaults) as OV
import Harmonia.Palette (palette, typeOn)
import Harmonia.Trellis (Pull(..), path, pathMotion, pulls, pullLabel, size)
import Harmonia.Vary (Density(..), Drift(..), variations)
import Harmonia.Voicing (Voicing, motionBetween, voicingMidi)
import Harmonia.Walk (seed)

notesOf :: Voicing -> Array Int
notesOf = sort <<< voicingMidi

-- | A four-slot progression whose slots hold real alternatives: each chord's
-- | own neighbourhood, taken from the Vary generator so the options are the
-- | ones the app would actually offer.
trellis :: Int -> Array (Array Voicing)
trellis roll = case Array.head palette of
  Nothing -> []
  Just ct -> map (slotFor ct) [ 0, 5, 9, 7 ]
  where
  slotFor ct root =
    let r = { root, chord: typeOn root ct }
    in map _.voicing (variations OV.defaults r Held Spaced (seed (roll * 31 + root + 1)) 6)

runTrellisTests :: Effect Unit
runTrellisTests = do
  log "\n--- Harmonia.Trellis — choosing a path when every chord has alternatives ---"

  let t = trellis 1
      widths = map length t

  -- ── motionBetween ──────────────────────────────────────────────────────
  let vs = Array.concat t
  assertTrue' "a voicing is at no distance from itself"
    (all (\v -> motionBetween v v == 0) vs)
  assertTrue' "motion is symmetric"
    (all (\a -> all (\b -> motionBetween a b == motionBetween b a) vs) vs)
  assertTrue' "only identical voicings are at distance zero"
    (all (\a -> all (\b -> motionBetween a b /= 0 || notesOf a == notesOf b) vs) vs)

  -- ── path ───────────────────────────────────────────────────────────────
  assertTrue' "a pass chooses once per slot"
    (all (\p -> all (\r -> length (path p (seed r) t) == length t) (range 1 40)) pulls)
  assertTrue' "every choice is a real option"
    (all (\p -> all (\r -> and (zipWith (\i w -> i >= 0 && i < w) (path p (seed r) t) widths))
                  (range 1 40))
       pulls)
  assertTrue' "the same seed gives the same pass"
    (all (\p -> all (\r -> path p (seed r) t == path p (seed r) t) (range 1 40)) pulls)

  -- A settled slot is a slot of one, and must be taken whatever the pull —
  -- this is the whole of what locking means, so it is the whole of the test.
  let locked = mapWithIndex (\i o -> if i == 1 then Array.take 1 o else o) t
  assertTrue' "a settled slot is always taken"
    (all (\p -> all (\r -> index (path p (seed r) locked) 1 == Just 0) (range 1 40)) pulls)

  assertTrue' "the lattice is as big as the product of its slots"
    (size t == Array.foldl (*) 1 widths)

  -- ── the measurement ────────────────────────────────────────────────────
  log ("\n  a " <> show (length t) <> "-slot progression, widths " <> show widths
        <> " — " <> show (size t) <> " paths")
  log "  mean motion at the joins, over 200 seeds:"
  for_ pulls \p -> do
    let ms = map (\r -> pathMotion (chosen (path p (seed r) t))) (range 1 200)
    log ("    " <> pullLabel p <> "   " <> show (sum ms / length ms))

  -- The dial has to be monotone or it is decoration. Asserted, not just
  -- printed: greedy `Smooth` measured WORSE than `Mid` (68 against 66) before
  -- it was replaced by the minimum-path walk, and nothing would have caught it.
  let mean p = let ms = map (\r -> pathMotion (chosen (path p (seed r) t))) (range 1 200)
               in sum ms / length ms
  assertTrue' ("the pull dial is monotone (loose " <> show (mean Loose)
                <> ", mid " <> show (mean Mid) <> ", smooth " <> show (mean Smooth) <> ")")
    (mean Smooth < mean Mid && mean Mid < mean Loose)

  -- And smooth must still VARY: were the first slot optimised too, every pass
  -- would be the same one and the setting would be worth pressing once.
  assertTrue' "a smooth pass still differs between seeds"
    (length (nub (map (\r -> path Smooth (seed r) t) (range 1 40))) > 1)

  log ("\n  " <> show (length pulls * 200) <> " passes walked")
  where
  chosen ixs = catMaybes (zipWith (\o i -> index o i) (trellis 1) ixs)
