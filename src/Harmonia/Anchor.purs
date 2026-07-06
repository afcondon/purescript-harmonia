-- | `Harmonia.Anchor` — a chord's *scale reading* and the operations it unlocks.
-- |
-- | A chord in Harmonia is one of two things: a `DegreeChord` + `Key` (a chord
-- | that knows where it sits in a scale — its numeral, quality, and any
-- | borrowing) or just a bag of pitches with no reading at all. The `Anchor`
-- | type names that distinction directly:
-- |
-- |   * `Located key dc` — the chord carries its scale context.
-- |   * `Free`           — pitches only; no scale to interpret it against.
-- |
-- | The **grade** of an anchor is not stored — it is *derived* by reading the
-- | recipe against its key (`classify`). Grade is an ordinal ladder of how much
-- | harmonic leverage the chord affords:
-- |
-- |   `ShiftOnly < Keyed < Diatonic`
-- |
-- | and each rung permits a nested set of operations (`permitted`). Nothing here
-- | forbids a chord from being `Free` — an unlocated chord is a perfectly legal
-- | inhabitant. The type gives leverage (a total classification a caller can
-- | pattern-match exhaustively) without making the "illegal" state
-- | unrepresentable. Rendering — colour, text, greyed-out buttons — is the
-- | caller's job; this module only says *what is true*, never *how to show it*.
-- |
-- | Pure `Prelude`/`Data.*`; builds unchanged on JS and purerl.
module Harmonia.Anchor
  ( Anchor(..)
  , Grade(..)
  , Op(..)
  , classify
  , gradeAnchor
  , permitted
  , permits
  , scalePCs
  , chordInScale
  , diatonicQuality
  , isSeventh
  ) where

import Prelude

import Data.Array (elem, (!!))
import Data.Foldable (all)
import Data.Maybe (Maybe(..), fromMaybe)

import Harmonia.Chord
  ( Chord(..), DegreeChord(..), Key, Mode, Numeral, Quality(..)
  , modeIntervals, numeralIndex, realize
  )

-- ---------------------------------------------------------------------------
-- Grade — the ordinal capability ladder
-- ---------------------------------------------------------------------------

-- | How much harmonic leverage a chord affords, low to high. The order is the
-- | whole point: capabilities nest, so `Ord` is meaningful — a `Diatonic` chord
-- | can do everything a `Keyed` one can, and more.
data Grade
  = ShiftOnly   -- ^ no scale reading — only absolute semitone motion
  | Keyed       -- ^ located to a key, but borrowed / foreign to it: can modulate
  | Diatonic    -- ^ fully at home in its key: can also reflavour and substitute

derive instance eqGrade :: Eq Grade
derive instance ordGrade :: Ord Grade

instance showGrade :: Show Grade where
  show = case _ of
    ShiftOnly -> "ShiftOnly"
    Keyed     -> "Keyed"
    Diatonic  -> "Diatonic"

-- ---------------------------------------------------------------------------
-- Op — the closed set of grade-gated verbs
-- ---------------------------------------------------------------------------

-- | The operations a chord can be asked to undergo. `Shift` is the floor
-- | (always available); the rest need progressively richer readings. A closed
-- | set: these are program-logic alternatives, so extending them is a
-- | deliberate recompile, never open user data.
data Op
  = Shift        -- ^ absolute semitone transposition
  | Modulate     -- ^ move the tonic, preserve the degree
  | Reflavour    -- ^ keep tonic + degree, change the mode, re-derive
  | Substitute   -- ^ replace with another chord of the same function

derive instance eqOp :: Eq Op
derive instance ordOp :: Ord Op

instance showOp :: Show Op where
  show = case _ of
    Shift      -> "Shift"
    Modulate   -> "Modulate"
    Reflavour  -> "Reflavour"
    Substitute -> "Substitute"

-- | The verbs a grade permits — nested, so higher grades are supersets. This is
-- | where the musical knowledge "reflavour ⇒ modulate ⇒ shift" lives, so no
-- | caller has to re-derive it.
permitted :: Grade -> Array Op
permitted = case _ of
  ShiftOnly -> [ Shift ]
  Keyed     -> [ Shift, Modulate ]
  Diatonic  -> [ Shift, Modulate, Reflavour, Substitute ]

-- | Is this operation available at this grade?
permits :: Op -> Grade -> Boolean
permits op g = elem op (permitted g)

-- ---------------------------------------------------------------------------
-- Anchor — the reading (or its absence)
-- ---------------------------------------------------------------------------

-- | A chord's scale reading. `Located` carries the full recipe and its key;
-- | `Free` is a chord that has left home (hand-entered, dragged out of key, or
-- | caught with no context) and can only be shifted.
data Anchor
  = Located Key DegreeChord
  | Free

derive instance eqAnchor :: Eq Anchor

instance showAnchor :: Show Anchor where
  show = case _ of
    Located key dc -> "Located " <> show key.tonic <> " " <> show key.mode <> " " <> show dc
    Free           -> "Free"

-- | The grade of an anchor, derived. `Free` is always `ShiftOnly`; a `Located`
-- | chord is classified against its key.
gradeAnchor :: Anchor -> Grade
gradeAnchor = case _ of
  Free            -> ShiftOnly
  Located key dc  -> classify key dc

-- | The v1 judgment. A located chord is `Diatonic` when it wears no modal-
-- | interchange override *and* every pitch it realizes falls inside the key's
-- | scale. Otherwise it is `Keyed` — it still has a tonic to modulate around,
-- | but it is foreign enough that reflavouring it into another mode is
-- | ill-defined (there is no single "same degree" to re-read).
-- |
-- | This is deliberately conservative and needs no chord-analyser: an
-- | out-of-scale chord simply drops to `Keyed`, so downstream operations pass
-- | it through under blame rather than silently mangling it. The place to grow
-- | later is *upward* — recognising a chromatic chord as a secondary function
-- | (V/x, viio/x) of a phrase degree and promoting it back to `Diatonic` in
-- | context. That promotion belongs to the phrase, not the lone chord (see
-- | `Harmonia.Graded`).
classify :: Key -> DegreeChord -> Grade
classify key dc@(DegreeChord r) = case r.mode of
  Just m | m /= key.mode -> Keyed
  _ -> if chordInScale key dc then Diatonic else Keyed

-- ---------------------------------------------------------------------------
-- Scale membership
-- ---------------------------------------------------------------------------

-- | The pitch classes of a key's scale.
scalePCs :: Key -> Array Int
scalePCs key = map (\iv -> (key.tonic + iv) `mod` 12) (modeIntervals key.mode)

-- | Does every pitch class of the realized chord lie in the key's scale?
chordInScale :: Key -> DegreeChord -> Boolean
chordInScale key dc =
  let Chord pcs = realize key dc
  in all (\p -> elem p (scalePCs key)) pcs

-- ---------------------------------------------------------------------------
-- diatonicQuality — the substance of a reflavour
-- ---------------------------------------------------------------------------

-- | The diatonic chord quality on a given degree of a mode, built by stacking
-- | thirds *within the scale*. This is what makes reflavouring real: the same
-- | numeral read in a new mode becomes whatever that mode's own harmony makes
-- | it — I maj7 in Ionian becomes i m7 in Aeolian, not a transposed Imaj7.
-- |
-- | `seventh` chooses a four-note (true) or three-note (triad) reading, so a
-- | reflavour preserves the chord's *size* while re-deriving its *flavour*.
diatonicQuality :: Mode -> Numeral -> Boolean -> Quality
diatonicQuality m n seventh =
  let
    idx = numeralIndex n
    r   = scaleDegreeSemi m idx
    t   = (scaleDegreeSemi m (idx + 2) - r) `mod` 12
    f   = (scaleDegreeSemi m (idx + 4) - r) `mod` 12
    s   = (scaleDegreeSemi m (idx + 6) - r) `mod` 12
  in
    if seventh then seventhQuality t f s else triadQuality t f

-- | Semitone offset of the k-th scale degree above the tonic, wrapping past the
-- | octave so third-stacking above degree 7 keeps climbing.
scaleDegreeSemi :: Mode -> Int -> Int
scaleDegreeSemi m k =
  let base = fromMaybe 0 (modeIntervals m !! (k `mod` 7))
  in base + 12 * (k `div` 7)

seventhQuality :: Int -> Int -> Int -> Quality
seventhQuality third fifth sev = case [ third, fifth, sev ] of
  [ 4, 7, 11 ] -> Maj7
  [ 4, 7, 10 ] -> Dom7
  [ 3, 7, 10 ] -> Min7
  [ 3, 6, 10 ] -> HalfDim
  [ 3, 6, 9 ]  -> FullyDim
  [ 3, 7, 11 ] -> MinMaj7
  [ 4, 8, 11 ] -> AugMaj7
  _            -> triadQuality third fifth

triadQuality :: Int -> Int -> Quality
triadQuality third fifth = case [ third, fifth ] of
  [ 4, 7 ] -> Maj
  [ 3, 7 ] -> Min
  [ 3, 6 ] -> Dim
  [ 4, 8 ] -> Aug
  _        -> Maj

-- | Does this quality carry a seventh?
isSeventh :: Quality -> Boolean
isSeventh = case _ of
  Maj7     -> true
  Min7     -> true
  Dom7     -> true
  HalfDim  -> true
  FullyDim -> true
  MinMaj7  -> true
  AugMaj7  -> true
  _        -> false
