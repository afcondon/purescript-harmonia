-- | A runnable tour of `Harmonia.Anchor` + `Harmonia.Graded` — the 0.2.0
-- | grade layer. It prints tables to stdout showing how much *harmonic
-- | leverage* a chord affords (its grade), which operations that unlocks, and
-- | — the point of the design — how an operation that runs out of reading
-- | hands back the input under **blame** rather than silently mangling it.
-- |
-- |     spago run -p harmonia-example --main Example.Anchor
-- |
-- | The story in one line: a chord that has left its scale is not *illegal*, it
-- | is *expressive*, so we keep it representable and let the operation report
-- | where its reading stopped.
module Example.Anchor (main) where

import Prelude

import Data.Array ((!!))
import Data.Foldable (for_, intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.String (length) as Str
import Effect (Effect)
import Effect.Console (log)

import Harmonia.Chord
  ( Chord(..), DegreeChord, Key, Mode(..), Numeral(..), Quality(..)
  , borrow, cMajorKey, deg, realize
  )
import Harmonia.Anchor (Anchor(..), permitted)
import Harmonia.Graded
  ( Blame, Graded, Phrase(..), blame, grade, modulate, reflavour, value )

-- ---------------------------------------------------------------------------
-- Pretty-printing
-- ---------------------------------------------------------------------------

noteNames :: Array String
noteNames = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

pcName :: Int -> String
pcName pc = fromMaybe "?" (noteNames !! (pc `mod` 12))

pad :: Int -> String -> String
pad w s = s <> power " " (max 0 (w - Str.length s))

rule :: String
rule = power "─" 68

-- | An anchor's realized pitch-class set as note names — or "· · ·" for `Free`,
-- | which has no key to read against.
anchorNotes :: Anchor -> String
anchorNotes = case _ of
  Free           -> "· · ·"
  Located key dc -> chordNotes key dc

chordNotes :: Key -> DegreeChord -> String
chordNotes key dc =
  let Chord pcs = realize key dc
  in intercalate " " (map pcName pcs)

-- | The operations a grade permits, as a flat string.
opsAt :: Anchor -> String
opsAt a = intercalate " " (map show (permitted (grade a)))

-- | A `Graded` outcome, rendered: the resulting notes, then a clean tick or the
-- | list of blames (the positions the operation could not move, and why).
showGraded :: Graded Anchor -> String
showGraded g = pad 14 (anchorNotes (value g)) <> blameTag (blame g)

blameTag :: Array Blame -> String
blameTag = case _ of
  [] -> "✓ clean"
  bs -> "⚠ blamed  " <> intercalate ", " (map showBlame bs)

showBlame :: Blame -> String
showBlame b = "@" <> show b.index <> " " <> show b.reason

-- ---------------------------------------------------------------------------
-- The cast — three chords in C major, one at each grade
-- ---------------------------------------------------------------------------

demos :: Array { label :: String, anchor :: Anchor }
demos =
  [ { label: "ii7  (D F A C — diatonic)"
    , anchor: Located cMajorKey (deg II Min7 [])
    }
  , { label: "bVI  (borrowed from Aeolian)"
    , anchor: Located cMajorKey (borrow Aeolian (deg VI Maj []))
    }
  , { label: "Free (a bag of pitches)"
    , anchor: Free
    }
  ]

-- ---------------------------------------------------------------------------
-- Two phrases — the fractal, one level up
-- ---------------------------------------------------------------------------

-- | A clean diatonic ii–V–I. Every member is at home, so the phrase is too.
diatonicPhrase :: Phrase
diatonicPhrase = Phrase
  [ Located cMajorKey (deg II Min7 [])
  , Located cMajorKey (deg V Dom7 [])
  , Located cMajorKey (deg I Maj7 [])
  ]

-- | The same shape with a borrowed chord in the middle. The phrase grade is the
-- | meet of its members, so that one foreign chord drops the whole phrase — and
-- | reflavouring it blames exactly that position, spelling out the boundary.
borrowedPhrase :: Phrase
borrowedPhrase = Phrase
  [ Located cMajorKey (deg II Min7 [])
  , Located cMajorKey (borrow Aeolian (deg VI Maj []))
  , Located cMajorKey (deg I Maj7 [])
  ]

phraseNotes :: Phrase -> String
phraseNotes (Phrase as) =
  intercalate "  |  " (map (\a -> "[" <> anchorNotes a <> "]") as)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: Effect Unit
main = do
  log ""
  log "harmonia · Anchor & Graded — grade-gated harmony, honest about partiality"
  log rule

  log "\nGrade & permitted operations"
  log (pad 32 "chord" <> pad 12 "grade" <> "can do")
  for_ demos \d -> do
    log (pad 32 d.label <> pad 12 (show (grade d.anchor)) <> opsAt d.anchor)

  log "\nApplying the two reading-dependent operations"
  log "(each returns the input under blame when its reading runs out)"
  log (pad 32 "chord" <> pad 40 "modulate +5  (→ F)" <> "reflavour Aeolian")
  for_ demos \d -> do
    log (pad 32 d.label
      <> pad 40 (showGraded (modulate 5 d.anchor))
      <> showGraded (reflavour Aeolian d.anchor))

  log "\nPhrases — grade is emergent (the meet of the members)"
  log rule
  log ("diatonic ii–V–I   grade " <> show (grade diatonicPhrase))
  log ("  before          " <> phraseNotes diatonicPhrase)
  log ("  reflavour Aeol. " <> phraseNotes (value (reflavour Aeolian diatonicPhrase))
    <> "   " <> blameTag (blame (reflavour Aeolian diatonicPhrase)))
  log ""
  log ("with a borrow     grade " <> show (grade borrowedPhrase))
  log ("  before          " <> phraseNotes borrowedPhrase)
  log ("  reflavour Aeol. " <> phraseNotes (value (reflavour Aeolian borrowedPhrase))
    <> "   " <> blameTag (blame (reflavour Aeolian borrowedPhrase)))
  log ("  → the blame names @1: the borrowed chord passed through, the spine moved.")
  log ""
