-- | `Harmonia.Vary` — the neighbourhood grid, measured.
-- |
-- | Two things are worth holding to. The axes must mean what they say — `Held`
-- | may not change a note, and the drifting settings must actually drift — and
-- | the cells must fill without repeating themselves, since a grid of sixteen
-- | pads showing the same chord four times is worse than a grid of four.
module Test.VarySpec (runVaryTests) where

import Prelude

import Data.Array (all, filter, head, length, nub, sort)
import Data.Array as Array
import Data.Foldable (for_, maximum, minimum)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertTrue')

import Harmonia.Chord (Chord(..))
import Harmonia.OpenVoicing (Rooted, defaults)
import Harmonia.Palette (palette, typeOn)
import Harmonia.Vary (Density(..), Drift(..), Variation, densities, densityLabel, drifts, driftLabel, variations)
import Harmonia.Voicing (Voicing, voicingMidi)
import Harmonia.Walk (seed)

notesOf :: Voicing -> Array Int
notesOf = sort <<< voicingMidi

pcsOf :: Voicing -> Array Int
pcsOf = sort <<< nub <<< map (\m -> mod m 12) <<< voicingMidi

chordPcs :: Chord -> Array Int
chordPcs (Chord pcs) = sort (nub (map (\p -> mod p 12) pcs))

-- | A spread of real chords to vary: every palette type on a handful of roots.
corpus :: Array Rooted
corpus = do
  root <- [ 0, 3, 7, 10 ]
  ct <- palette
  [ { root, chord: typeOn root ct } ]

cell :: Rooted -> Drift -> Density -> Array Variation
cell r d dens = variations defaults r d dens (seed 4242) 16

law :: String -> (Rooted -> Boolean) -> Effect Unit
law name holds = do
  let bad = filter (not <<< holds) corpus
  assertTrue' (name <> " (" <> show (length bad) <> " of " <> show (length corpus) <> " failed)")
    (length bad == 0)

runVaryTests :: Effect Unit
runVaryTests = do
  log ("\n--- Harmonia.Vary — the neighbourhood of a chord over " <> show (length corpus) <> " chords ---")

  -- ── The axes mean what they say ────────────────────────────────────────
  law "held keeps the pitch-class content, in every density"
    (\r -> all (\dens -> all (\v -> chordPcs v.chord == chordPcs r.chord && pcsOf v.voicing == chordPcs r.chord)
                           (cell r Held dens))
             densities)
  law "held never reports that it moved"
    (\r -> all (\dens -> all (\v -> not v.moved) (cell r Held dens)) densities)
  law "thinning never drops the root"
    (\r -> all (\dens -> all (\v -> Array.elem (mod r.root 12) (chordPcs v.chord)) (cell r Thinned dens))
             densities)
  law "a swapped tone leaves a chord of the same size"
    (\r -> all (\dens -> all (\v -> not v.moved || length (chordPcs v.chord) == length (chordPcs r.chord))
                           (cell r Swapped dens))
             densities)

  -- ── The cells are usable ───────────────────────────────────────────────
  law "no cell repeats a voicing"
    (\r -> all (\d -> all (\dens ->
              let vs = map (notesOf <<< _.voicing) (cell r d dens)
              in length (nub vs) == length vs) densities) drifts)
  law "every candidate sounds what its chord says"
    (\r -> all (\d -> all (\dens -> all (\v -> pcsOf v.voicing == chordPcs v.chord) (cell r d dens))
                       densities) drifts)
  law "no candidate invents a unison"
    (\r -> all (\d -> all (\dens -> all (\v -> length (nub (notesOf v.voicing)) == length (notesOf v.voicing))
                                      (cell r d dens))
                       densities) drifts)
  law "every candidate is playable"
    (\r -> all (\d -> all (\dens -> all (\v ->
              fromMaybe false (map (_ >= 12) (minimum (notesOf v.voicing)))
                && fromMaybe false (map (_ <= 120) (maximum (notesOf v.voicing))))
              (cell r d dens)) densities) drifts)
  law "the same seed gives the same cell"
    (\r -> all (\d -> all (\dens -> map (notesOf <<< _.voicing) (cell r d dens)
                                      == map (notesOf <<< _.voicing) (cell r d dens))
                       densities) drifts)

  -- ── Catalogue at one end, lottery at the other ─────────────────────────
  -- Not a law but a measurement, printed: the close/held corner should come
  -- back short on small chords because it has been exhausted, and the far
  -- corner should always fill.
  log "\n  cell fill (of 16), averaged over the corpus:"
  for_ drifts \d -> do
    let row = map (\dens -> avg (map (\r -> length (cell r d dens)) corpus)) densities
    log ("    " <> pad (driftLabel d) <> " "
          <> Array.intercalate "  " (Array.zipWith (\dens n -> densityLabel dens <> " " <> show n) densities row))

  -- A sample to read by eye: C major 7 at the two corners.
  for_ (head palette) \ct -> do
   let cmaj7 = { root: 0, chord: typeOn 0 ct }
   log "\n  sample — the held/close corner:"
   for_ (Array.take 6 (cell cmaj7 Held Close)) \v -> log ("    " <> show (notesOf v.voicing))
   log "  sample — the swapped/wide corner:"
   for_ (Array.take 6 (cell cmaj7 Swapped Wide)) \v ->
     log ("    " <> show (notesOf v.voicing) <> "   pcs " <> show (chordPcs v.chord))

  log ("\n  " <> show (length corpus * 9) <> " cells generated")
  where
  avg xs = if length xs == 0 then 0 else Array.foldl (+) 0 xs / length xs
  pad s = s <> power " " (max 1 (9 - String.length s))
