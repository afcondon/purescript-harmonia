-- | `Harmonia.Graded` — grade-gated harmonic operations, honest about partiality.
-- |
-- | Two type classes over anything harmonic:
-- |
-- |   * `Harmonic`     — `grade` (what leverage does this afford?) and
-- |     `transpose` (the always-available floor; never blames).
-- |   * `Reflavourable` — the operations that *consult the reading* and may fail
-- |     to apply: `modulate` (needs a key) and `reflavour` (needs a diatonic
-- |     reading). They are total functions — you can always *call* them — but
-- |     each returns a `Graded`, carrying both a result and any **blame**.
-- |
-- | This is the design inversion made concrete. In code we make illegal states
-- | unrepresentable; a chord that has left its scale is not illegal, it is
-- | *expressive*, so we make it representable and let the operation report where
-- | its reading ran out. Building it against the real types settled a design
-- | question left open in the sketch: every operation is **total-with-
-- | annotations** — the input always survives as a fallback (worst case, passed
-- | through untouched) — so the honest result shape is a value carrying blame,
-- | not a `These` with an uninhabited "nothing came back" case. The blame *is*
-- | the leverage: `Both`-style partial results force the caller to see the
-- | boundaries rather than silently getting a mangled chord.
-- |
-- | The classes instance for both a single `Anchor` and a `Phrase` (a sequence
-- | of readings) — the fractal the design turns on: a progression is the same
-- | kind of object as a chord, one level up, and its grade is *emergent* (the
-- | meet of its members in v1), not a property of any single chord.
-- |
-- | The "lock" from the design discussion turns out to be a **value**, not a
-- | class: every `Reflavourable` thing may attempt every operation; the *grade*
-- | decides which succeed and which come back blamed. Locked = high grade,
-- | unlocked = low grade — checked at the value level, exactly so the illegal
-- | move stays representable.
-- |
-- | Pure `Prelude`/`Data.*`; builds unchanged on JS and purerl.
module Harmonia.Graded
  ( BlameReason(..)
  , Blame
  , Graded(..)
  , value
  , blame
  , clean
  , blamed
  , class Harmonic
  , grade
  , transpose
  , class Reflavourable
  , modulate
  , reflavour
  , Phrase(..)
  ) where

import Prelude

import Data.Array (concatMap, mapWithIndex)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))

import Harmonia.Chord (DegreeChord(..), Mode)
import Harmonia.Anchor
  ( Anchor(..), Grade(..), diatonicQuality, gradeAnchor, isSeventh )

-- ---------------------------------------------------------------------------
-- Blame — where an operation's reading ran out
-- ---------------------------------------------------------------------------

-- | Why a chord could not follow an operation. Closed, and richer than a bare
-- | reference on purpose: the reason is exactly what a renderer needs to mark
-- | the boundary (a passed-through chord in a reflavoured phrase, say) — but the
-- | *choice* of how to mark it stays with the caller.
data BlameReason
  = NoReading   -- ^ `Free`: no scale context at all — only shifting is possible
  | Borrowed    -- ^ located but foreign to its key — can't be cleanly reflavoured

derive instance eqBlameReason :: Eq BlameReason

instance showBlameReason :: Show BlameReason where
  show = case _ of
    NoReading -> "NoReading"
    Borrowed  -> "Borrowed"

-- | A single blame: which position (0 for a lone chord; the phrase index for a
-- | member of a `Phrase`) and why.
type Blame = { index :: Int, reason :: BlameReason }

-- ---------------------------------------------------------------------------
-- Graded — a result carrying its blame
-- ---------------------------------------------------------------------------

-- | The outcome of a grade-gated operation: a value plus the blame accrued
-- | applying it. Empty blame = a clean, fully-applied result; non-empty blame =
-- | a partial result whose listed positions were passed through untouched.
newtype Graded a = Graded { value :: a, blame :: Array Blame }

derive newtype instance eqGraded :: Eq a => Eq (Graded a)

instance functorGraded :: Functor Graded where
  map f (Graded g) = Graded { value: f g.value, blame: g.blame }

-- | The result value (always present).
value :: forall a. Graded a -> a
value (Graded g) = g.value

-- | The blame accrued (empty when the operation applied cleanly).
blame :: forall a. Graded a -> Array Blame
blame (Graded g) = g.blame

-- | A clean result — the operation applied with nothing passed through.
clean :: forall a. a -> Graded a
clean a = Graded { value: a, blame: [] }

-- | A blamed result — the value (typically the input, unchanged) plus one
-- | reason it could not be transformed at the given position.
blamed :: forall a. Int -> BlameReason -> a -> Graded a
blamed i r a = Graded { value: a, blame: [ { index: i, reason: r } ] }

-- | Rewrite every blame's position — used to give a phrase member the index it
-- | sits at, since a lone-chord operation only ever knows position 0.
reindexTo :: forall a. Int -> Graded a -> Graded a
reindexTo i (Graded g) =
  Graded (g { blame = map (\b -> b { index = i }) g.blame })

-- ---------------------------------------------------------------------------
-- The classes
-- ---------------------------------------------------------------------------

-- | Everything harmonic reports its `grade` and can be `transpose`d. Transpose
-- | is the floor: pure semitone arithmetic, always total, never blames.
class Harmonic a where
  grade :: a -> Grade
  transpose :: Int -> a -> a

-- | Operations that consult the reading and may not apply. `modulate` moves the
-- | tonic (needs a key); `reflavour` re-reads the same degree in a new mode
-- | (needs a diatonic reading). Both return `Graded`, blaming what they cannot
-- | move.
class Harmonic a <= Reflavourable a where
  modulate :: Int -> a -> Graded a
  reflavour :: Mode -> a -> Graded a

-- ---------------------------------------------------------------------------
-- Anchor instances — a single chord's reading
-- ---------------------------------------------------------------------------

instance harmonicAnchor :: Harmonic Anchor where
  grade = gradeAnchor
  -- Shifting a located chord moves its key's tonic (the reading rides along,
  -- degree preserved); a Free chord has no reading to move — the caller shifts
  -- its raw pitches. Either way the audible result is +n semitones.
  transpose n = case _ of
    Located key dc -> Located (key { tonic = wrap (key.tonic + n) }) dc
    Free           -> Free

instance reflavourableAnchor :: Reflavourable Anchor where
  modulate newTonic = case _ of
    Free           -> blamed 0 NoReading Free
    -- A located chord is always at least `Keyed`, and `Keyed` permits Modulate,
    -- so this never blames.
    Located key dc -> clean (Located (key { tonic = wrap newTonic }) dc)

  reflavour newMode a = case a of
    Free -> blamed 0 NoReading Free
    Located key (DegreeChord r) -> case gradeAnchor a of
      Diatonic ->
        -- Re-read the same degree in the new mode: preserve numeral (and slash),
        -- re-derive the quality from the new mode's own diatonic harmony, and
        -- reset colour — tensions and any override were the *old* mode's, so a
        -- reflavour hands back the clean diatonic chord of that degree. (Whether
        -- tensions should carry across is a judgment call worth revisiting.)
        let
          q' = diatonicQuality newMode r.numeral (isSeventh r.quality)
          dc' = DegreeChord (r { quality = q', tensions = [], mode = Nothing })
        in
          clean (Located (key { mode = newMode }) dc')
      -- Keyed (borrowed/foreign): there is no single "same degree" to re-read,
      -- so pass it through and say why.
      _ -> blamed 0 Borrowed a

-- ---------------------------------------------------------------------------
-- Phrase — a sequence of readings; the fractal, one level up
-- ---------------------------------------------------------------------------

-- | An ordered sequence of chord readings. Its grade is *emergent*: in v1 the
-- | meet of its members, so a single foreign chord drops the whole phrase to
-- | what all of it can uniformly do. (The intended growth is to *absorb*
-- | subordinate chromaticism — a secondary dominant read against a phrase degree
-- | shouldn't lower the phrase — which is why this is the meet *for now*, with a
-- | clearly-marked seam rather than a claim that the meet is the last word.)
newtype Phrase = Phrase (Array Anchor)

derive instance eqPhrase :: Eq Phrase

instance showPhrase :: Show Phrase where
  show (Phrase as) = "Phrase " <> show as

instance harmonicPhrase :: Harmonic Phrase where
  grade (Phrase as) = foldl min Diatonic (map gradeAnchor as)
  transpose n (Phrase as) = Phrase (map (transpose n) as)

instance reflavourablePhrase :: Reflavourable Phrase where
  modulate newTonic (Phrase as) = phraseOp (modulate newTonic) as
  reflavour newMode (Phrase as) = phraseOp (reflavour newMode) as

-- | Apply a per-chord operation across a phrase, tagging each chord's blame with
-- | its position and concatenating. The multi-blame result is exactly the
-- | "reflavour the located spine, pass the foreign chords through" behaviour:
-- | the blame list names precisely the boundaries where the reading stopped.
phraseOp :: (Anchor -> Graded Anchor) -> Array Anchor -> Graded Phrase
phraseOp f as =
  let gs = mapWithIndex (\i a -> reindexTo i (f a)) as
  in Graded { value: Phrase (map value gs), blame: concatMap blame gs }

-- | Wrap a possibly-out-of-range pitch class back into 0..11.
wrap :: Int -> Int
wrap n = ((n `mod` 12) + 12) `mod` 12
