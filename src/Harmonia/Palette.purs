-- | **A chord vocabulary graded by complexity — the palette a generator draws
-- | from.**
-- |
-- | Harmonia's `Quality` and `Tension` say what a chord IS. This says which
-- | chords are *in play*, and it is the first half of a generator in the family
-- | of the Progressions app: pick a complexity, and you have narrowed forty
-- | chord types to the handful that belong at that level.
-- |
-- | The metaphor is the source's own. David Collett, whose *Circle of Colors*
-- | is the published companion to that app, describes the chord grid as
-- | *"analogous to a painter's palette of colors that can be blended to create
-- | a full spectrum – from the simplest hues to the full range of subtle and
-- | complex tones"*. The levels are exactly that: how many colours are on the
-- | palette, not how far from home you are allowed to carry them. **Distance is
-- | a separate axis** and does not live here.
-- |
-- | ## The levels are cumulative
-- |
-- | *"each successive level adds more complex chord groups to the mix"* — so
-- | `Extreme` contains plain triads as well as thirteenths, and a set generated
-- | at the top is expected to hold simple chords. This matters more than it
-- | looks: a generator that read the levels as disjoint bands would produce
-- | nothing but towers, which is not what the source does and not what it
-- | sounds like.
-- |
-- | `upTo` is therefore the function you want almost always; `atLevel` is for
-- | showing a reader what a level CONTRIBUTES.
-- |
-- | ## What Basic and Low contribute
-- |
-- | Levels are described in the source partly by ROLE — *"I, IV, V and their
-- | relative minors ii, iii, and vi triads"* — and a role is not a chord type.
-- | Stripped to types, `Basic` contributes exactly two: major and minor. Low
-- | adds the ambiguous triads, and note that **Low is not "denser"**: parallel
-- | majors and minors, suspensions and the no-fifth shells carry *less*
-- | information than a triad, not more. The ladder is diatonic → ambiguous →
-- | coloured → extended → far out, and only the top three are about density.
-- |
-- | ## Provenance, and where it is uncertain
-- |
-- | Assembled from three sources that disagree: the in-app chart (30 types),
-- | Collett's paper (states 40, lists more than the chart), and chord names
-- | observed in the app's own Replace menus. Where they conflict the app wins,
-- | since it is the thing actually generating.
-- |
-- | Two entries are inferred rather than published and are marked at their
-- | definition: `dim`, which no published list places at a level but which the
-- | menus show throughout Low, and `M9♭5`, which the menus show and neither
-- | document mentions. The total reaching forty is a check, not a proof — it
-- | agrees with Collett's stated count, but the membership of the last few is
-- | our reading.
-- |
-- | Full write-up, including the measured transition rule this palette will
-- | eventually be walked by: `docs/kb/reference/progressions-generator.md`.
module Harmonia.Palette
  ( Level(..)
  , levels
  , levelName
  , levelIndex
  , ChordType(..)
  , chordType
  , typeIntervals
  , typeOn
  , palette
  , atLevel
  , upTo
  ) where

import Prelude

import Data.Array (filter)
import Harmonia.Chord (Chord, DegreeChord, Mode(..), Numeral(..), Quality(..), Tension(..), deg, realize)

-- | How many colours are on the palette. Five levels, cumulative.
data Level
  = Basic
  | Low
  | Medium
  | High
  | Extreme

derive instance eqLevel :: Eq Level
derive instance ordLevel :: Ord Level

instance showLevel :: Show Level where
  show = levelName

levels :: Array Level
levels = [ Basic, Low, Medium, High, Extreme ]

levelName :: Level -> String
levelName = case _ of
  Basic -> "Basic"
  Low -> "Low"
  Medium -> "Medium"
  High -> "High"
  Extreme -> "Extreme"

-- | 1..5, for the levels the source numbers rather than names.
levelIndex :: Level -> Int
levelIndex = case _ of
  Basic -> 1
  Low -> 2
  Medium -> 3
  High -> 4
  Extreme -> 5

-- | One chord type: how Harmonia builds it, what it is called, and the level
-- | at which it first becomes available.
-- |
-- | `suffix` is the name the source prints after a root — `"m9b5"`, `"q3"` —
-- | rather than anything Harmonia invents, so a generated chord can be checked
-- | against the app's own label by eye.
newtype ChordType = ChordType
  { quality :: Quality
  , tensions :: Array Tension
  , suffix :: String
  , level :: Level
  }

derive instance eqChordType :: Eq ChordType

instance showChordType :: Show ChordType where
  show (ChordType t) = "ChordType " <> show t.suffix

chordType :: Level -> Quality -> Array Tension -> String -> ChordType
chordType lv q ts sfx =
  ChordType { quality: q, tensions: ts, suffix: sfx, level: lv }

-- | The type's pitch classes on root 0 — computed through `realize`, so the
-- | palette's idea of a chord is exactly the library's idea of it and the two
-- | cannot drift.
typeIntervals :: ChordType -> Chord
typeIntervals = typeOn 0

-- | The type realised on an absolute root pitch class.
-- |
-- | **This is the generator's entry point into Harmonia, and it is below
-- | `DegreeChord` on purpose.** A scale degree is relative to a key, and the
-- | distance axis this palette pairs with reaches chords that no numeral in the
-- | home key can name — at full freedom there is no key left for a numeral to
-- | be relative to. So a chord is addressed by its ROOT, and `deg I` over a
-- | tonic set to that root is the arithmetic that does it.
typeOn :: Int -> ChordType -> Chord
typeOn rootPC t = realize { tonic: mod rootPC 12, mode: Ionian } (asDegree t)

asDegree :: ChordType -> DegreeChord
asDegree (ChordType t) = deg I t.quality t.tensions

-- | Only the types a level introduces.
atLevel :: Level -> Array ChordType
atLevel lv = filter (\(ChordType t) -> t.level == lv) palette

-- | Every type available AT a level — cumulative, which is what the source
-- | means by its levels and what a generator wants.
upTo :: Level -> Array ChordType
upTo lv = filter (\(ChordType t) -> levelIndex t.level <= levelIndex lv) palette

-- | The forty.
palette :: Array ChordType
palette =
  -- ── Basic: the two triads everything else is a departure from ───────────
  [ chordType Basic Maj [] ""
  , chordType Basic Min [] "m"

  -- ── Low: ambiguity rather than density ──────────────────────────────────
  , chordType Low Maj [ Sus 2 ] "sus2"
  , chordType Low Maj [ Sus 4 ] "sus4"
  , chordType Low Aug [] "aug"
  -- INFERRED. No published list places the diminished triad at a level, yet
  -- the app's menus show it right through the Low band (A°, B♭°, C°, E♭°).
  -- Filed here because that is where it is observed, not because a document
  -- says so.
  , chordType Low Dim [] "dim"
  -- The shells. A seventh with its fifth removed is the ambiguity Low is
  -- about: fewer notes, less certainty, not more colour.
  , chordType Low Dom7 [ NoFifth ] "7(no5)"
  , chordType Low Min7 [ NoFifth ] "m7(no5)"

  -- ── Medium: sixths and sevenths ─────────────────────────────────────────
  , chordType Medium Maj [ Add 6 ] "M6"
  , chordType Medium Min [ Add 6 ] "m6"
  , chordType Medium Dom7 [] "7"
  , chordType Medium Min7 [] "m7"
  , chordType Medium FullyDim [] "°7"
  , chordType Medium HalfDim [] "ø7"
  , chordType Medium Dom7 [ Sus 4 ] "7sus4"
  , chordType Medium Maj7 [] "M7"
  -- An augmented triad carrying a minor seventh. Built from `Aug` rather than
  -- from `Dom7` with a raised fifth, because `Sharp` adds and does not move:
  -- the augmented fifth has to be in the quality or it sits beside a natural
  -- one.
  , chordType Medium Aug [ Flat 7 ] "7#5"
  , chordType Medium MajFlat5 [ Add 7 ] "M7b5"
  , chordType Medium MinMaj7 [] "mM7"

  -- ── High: ninths ────────────────────────────────────────────────────────
  , chordType High Dom7 [ Add 9 ] "9"
  , chordType High MajFlat5 [ Flat 7, Add 9 ] "9b5"
  , chordType High Min7 [ Add 9 ] "m9"
  , chordType High HalfDim [ Add 9 ] "m9b5"
  , chordType High Maj7 [ Add 9 ] "M7+9"
  , chordType High Maj [ Add 6, Add 9 ] "M6+9"
  , chordType High Min [ Add 6, Add 9 ] "m6+9"
  , chordType High MinMaj7 [ Add 9 ] "mM9"
  , chordType High Dom7 [ Sus 4, Add 9 ] "9sus4"
  , chordType High Dom7 [ Sus 4, Add 9, Add 13 ] "9sus4+13"
  -- INFERRED. Observed in the app (AM9♭5, CM9♭5, DM9♭5) and absent from both
  -- the chart and the paper.
  , chordType High MajFlat5 [ Add 7, Add 9 ] "M9b5"

  -- ── Extreme: elevenths, thirteenths, and what is not tertian at all ─────
  , chordType Extreme Dom7 [ Add 9, Sharp 11 ] "9#11"
  , chordType Extreme Min7 [ Add 9, Add 11 ] "m11"
  , chordType Extreme Maj7 [ Add 9, Add 11 ] "M11"
  , chordType Extreme Maj7 [ Add 9, Sharp 11 ] "M9#11"
  -- The source writes this one out as its own construction, °7+9+11, which is
  -- worth keeping in the name: it is a diminished seventh carrying tensions,
  -- not an eleventh chord that happens to be diminished.
  , chordType Extreme FullyDim [ Add 9, Add 11 ] "°11"
  , chordType Extreme Dom7 [ Add 9, Add 13 ] "13"
  , chordType Extreme Maj7 [ Add 9, Add 13 ] "M9+13"
  , chordType Extreme Quartal3 [] "q3"
  , chordType Extreme Quartal4 [] "q4"
  , chordType Extreme Mystic [] "Mystic"
  ]
