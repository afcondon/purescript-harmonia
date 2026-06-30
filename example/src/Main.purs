-- | A runnable tour of `harmonia`. Realizes chord recipes against a key,
-- | voice-leads a progression, and prints the result as note-name / MIDI
-- | tables. Run from the package root with:
-- |
-- |     spago run -p harmonia-example
module Example.Main (main) where

import Prelude

import Data.Array (mapWithIndex, (!!))
import Data.Foldable (intercalate, sequence_)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String (length) as Str
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord
  ( Chord(..), DegreeChord, Numeral(..), Quality(..)
  , cMajorKey, deg, mcmullenYellow, mcmullenYellowNames, realize
  )
import Harmonia.Voicing
  ( Voicing, closeVoicing, drop2, play, voicingMidi )

-- ---------------------------------------------------------------------------
-- Pretty-printing helpers (note names, MIDI names, table padding)
-- ---------------------------------------------------------------------------

noteNames :: Array String
noteNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | Pitch class (0..11) → note name.
pcName :: Int -> String
pcName pc = fromMaybe "?" (noteNames !! (pc `mod` 12))

-- | MIDI note number → spelled note with octave (middle C = C4 = 60).
midiName :: Int -> String
midiName n = pcName n <> show ((n `div` 12) - 1)

-- | A Chord rendered as its pitch-class set in note names.
showChord :: Chord -> String
showChord (Chord pcs) = intercalate " " (map pcName pcs)

-- | A Voicing rendered as its MIDI notes in spelled-note form.
showVoicing :: Voicing -> String
showVoicing v = intercalate " " (map midiName (voicingMidi v))

-- | Right-pad a string to a fixed width (for table columns).
padTo :: Int -> String -> String
padTo n s = s <> power " " (max 0 (n - Str.length s))

rule :: String
rule = power "─" 64

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: Effect Unit
main = do
  log "harmonia — a runnable tour\n"

  -- 1. Recipes resolve against a key into pitch-class sets ------------------
  log rule
  log "1. A ii–V–I in C major: recipe → pitch classes → close voicing"
  log rule
  let
    cadence = [ deg II Min7 [], deg V Dom7 [], deg I Maj7 [] ]
    cadenceNames = [ "ii7", "V7", "Imaj7" ]
  log (padTo 8 "chord" <> padTo 16 "pitch classes" <> "close voicing")
  sequence_ $ mapWithIndex (cadenceRow cadenceNames) cadence

  -- 2. The same three chords, but voice-led -------------------------------
  log ""
  log rule
  log "2. Voice-led: play threads each chord into the next by least motion"
  log rule
  log (padTo 8 "chord" <> "voicing (common tones stay put)")
  sequence_ $ mapWithIndex (voicedRow cadenceNames) (play cMajorKey identity cadence)

  -- 3. A voicing strategy --------------------------------------------------
  log ""
  log rule
  log "3. Voicing strategies compose: closeVoicing then drop2 on Imaj7"
  log rule
  let close = closeVoicing { centre: 4 } (realize cMajorKey (deg I Maj7 []))
  log ("  close : " <> showVoicing close)
  log ("  drop2 : " <> showVoicing (drop2 close))

  -- 4. The McMullen "Yellow" walk -----------------------------------------
  log ""
  log rule
  log "4. McMullen \"Yellow\" — 18 recipes, realized in C and voice-led"
  log rule
  log (padTo 4 "#" <> padTo 12 "name" <> padTo 18 "pitch classes" <> "voicing")
  sequence_ $ mapWithIndex (yellowRow mcmullenYellowNames mcmullenYellow)
                           (play cMajorKey identity mcmullenYellow)

  log ""
  log "Done. Every value above is pure — no Effect beyond these logs."

-- One ii–V–I row: name, realized PCs, close voicing.
cadenceRow :: Array String -> Int -> DegreeChord -> Effect Unit
cadenceRow names i dc =
  let ch = realize cMajorKey dc
  in log (padTo 8 (nameAt names i)
            <> padTo 16 (showChord ch)
            <> showVoicing (closeVoicing { centre: 4 } ch))

-- One voice-led row: name, voicing.
voicedRow :: Array String -> Int -> Voicing -> Effect Unit
voicedRow names i v = log (padTo 8 (nameAt names i) <> showVoicing v)

-- One Yellow row: index, name, realized PCs, voicing.
yellowRow :: Array String -> Array DegreeChord -> Int -> Voicing -> Effect Unit
yellowRow names recipes i v =
  let ch = fromMaybe (Chord []) (realize cMajorKey <$> (recipes !! i))
  in log (padTo 4 (show (i + 1))
            <> padTo 12 (nameAt names i)
            <> padTo 18 (showChord ch)
            <> showVoicing v)

nameAt :: Array String -> Int -> String
nameAt names i = fromMaybe "?" (names !! i)
