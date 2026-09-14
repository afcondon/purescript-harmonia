-- | **The four generator modules joined into one door.**
-- |
-- | `Palette` says which chord types are in play, `Freedom` says which roots
-- | they may sit on, `Walk` moves between them and `OpenVoicing` turns the
-- | result into notes. Each is independently useful and independently testable,
-- | which is why they are separate — but a consumer almost never wants one of
-- | them. It wants a progression, and that means composing all four in the same
-- | order every time.
-- |
-- | This module is that composition, and nothing else. It holds no rule of its
-- | own; every musical decision in here was made one layer down.
-- |
-- | ## The seam it exists to close
-- |
-- | The generator's currency is `Allowed = { root, chordType }` — an address in
-- | the vocabulary. The voicer's currency is `Rooted = { root, chord }` — pitch
-- | content with its root named. Those are one `typeOn` apart, and neither
-- | module can own the conversion: `Palette` knows nothing about voicing and
-- | `OpenVoicing` knows nothing about the palette. Left unjoined, every consumer
-- | writes the same line, which is how two consumers eventually disagree.
-- |
-- | ## A progression is a Spec, not an array
-- |
-- | `Spec` is the whole re-runnable object: settings, seed, length. Four numbers
-- | and two enums regenerate the chords exactly, so **the thing worth saving is
-- | the spec and not its output**.
-- |
-- | That is the property the sampling side depends on. A swept transect in
-- | Quadrat is stored as its spec and can be re-run at a different resolution;
-- | a generated progression now has the same shape, so it can be re-run at a
-- | different LENGTH, re-voiced in a different register, or resampled through a
-- | different instrument, and still be the same progression. The source app
-- | gets this from pads that hold still. We get it from saving a number.
-- |
-- | ## Voicing is done over the sequence, not per chord
-- |
-- | `voice` runs `playOpen` rather than mapping `openVoicing`, because the
-- | top-note contour is a property of the SEQUENCE — each chord is voiced to
-- | move least from the one before it. Voicing the chords independently and
-- | concatenating them is a different (and much worse) result, so the array is
-- | the unit here on purpose.
module Harmonia.Progression
  ( Spec
  , spec
  , Voiced
  , rooted
  , noteName
  , nameOf
  , voice
  , progression
  , notes
  ) where

import Prelude

import Data.Array (index, zipWith)
import Data.Maybe (fromMaybe)
import Harmonia.Chord (Chord)
import Harmonia.Freedom (Allowed, Freedom, Home)
import Harmonia.Palette (ChordType, Level, typeOn, typeSuffix)
import Harmonia.OpenVoicing (Open, Rooted, Spread, playOpen, spreadOf)
import Harmonia.Voicing (Voicing, voicingMidi)
import Harmonia.Walk (Seed, Setting, walk)

-- | Everything needed to regenerate a progression exactly: where home is, how
-- | far it may roam, how richly it may be coloured, which walk it took, and how
-- | many chords of it to take.
type Spec =
  { setting :: Setting
  , seed :: Seed
  , length :: Int
  }

-- | Build a spec from the parts a UI usually holds separately.
spec :: Home -> Freedom -> Level -> Boolean -> Seed -> Int -> Spec
spec h f lv duplicates s n =
  { setting: { home: h, freedom: f, complexity: lv, duplicates }
  , seed: s
  , length: n
  }

-- | One chord of a progression, carrying every reading of itself a consumer is
-- | likely to want: its address in the vocabulary (`root`, `chordType`), its
-- | pitch content (`chord`), its written name, its notes, and the `spread` those
-- | notes came from.
-- |
-- | The spread rides along because an editor needs it and cannot reliably
-- | recover it — `spreadOf` is a matching, not an inverse. A chord that was
-- | GENERATED here knows exactly which displacement produced it, so handing
-- | that on costs nothing and saves the editor from guessing.
type Voiced =
  { root :: Int
  , chordType :: ChordType
  , chord :: Chord
  , name :: String
  , voicing :: Voicing
  , spread :: Spread
  }

-- | The generator's address realised as pitch content with its root still named
-- | — the one line that joins the vocabulary to the voicer.
rooted :: Allowed -> Rooted
rooted a = { root: a.root, chord: typeOn a.root a.chordType }

-- | Pitch class as a note name, sharps throughout.
-- |
-- | A deliberately plain default: correct spelling is key-dependent (the same
-- | pitch class is A♭ in one context and G♯ in another) and a consumer that
-- | cares will have a better answer. Compose your own with `typeSuffix` rather
-- | than reaching past this.
noteName :: Int -> String
noteName pc = fromMaybe "?" (index names (mod (mod pc 12 + 12) 12))
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | A chord's written name: root plus the type's suffix.
nameOf :: Allowed -> String
nameOf a = noteName a.root <> typeSuffix a.chordType

-- | Voice a whole sequence, smoothing the top-note contour across it.
voice :: Open -> Array Allowed -> Array Voiced
voice o as = zipWith build as (playOpen o (map rooted as))
  where
  build a v =
    { root: a.root
    , chordType: a.chordType
    , chord: typeOn a.root a.chordType
    , name: nameOf a
    , voicing: v
    , spread: spreadOf o (rooted a) v
    }

-- | **The door: a spec in, voiced chords out.**
progression :: Open -> Spec -> Array Voiced
progression o s = voice o (walk s.setting s.seed s.length)

-- | The chord's notes, ascending MIDI, bass first. The bass IS the root — that
-- | is `OpenVoicing`'s whole constraint — so nothing needs re-grounding below
-- | it.
notes :: Voiced -> Array Int
notes = voicingMidi <<< _.voicing
