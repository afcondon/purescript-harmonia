-- | **Open voicing: root in the bass, notes spread by octaves, top line
-- | smoothed.**
-- |
-- | Harmonia already voice-leads, and this is a different thing. `voiceLead`
-- | minimises TOTAL motion across all voices — the choral answer, and the right
-- | one when every voice is a part somebody sings. This minimises motion of the
-- | TOP note while pinning the bass to the root, which is the answer a keyboard
-- | pad wants: the bass states the harmony, the top states the melody, and what
-- | happens between them is texture.
-- |
-- | The two produce different music from the same chords, and neither is a
-- | refinement of the other.
-- |
-- | ## Where the rule comes from
-- |
-- | Measured from the Progressions app, whose manual states it outright: at the
-- | upper complexity levels open voicing is FORCED and cannot be turned off,
-- | *"the root note stays fixed as the bottom note of any chord"*, the notes
-- | spread *"while trying to maintain top note alignment"*, and the app
-- | *"attempts to create the smoothest possible top-note contour"*. Its own
-- | model for a voicing is an array of per-tone semitone displacements with an
-- | `x` for a tone that is not played — which is exactly `Spread` below, minus
-- | the omission, which Harmonia already has in `Selector`.
-- |
-- | Full derivation: `docs/kb/reference/progressions-generator.md`.
-- |
-- | ## Why chords arrive with their root attached
-- |
-- | A `Chord` is a pitch-class SET and carries no record of which member is the
-- | root — `[0,4,7]` is as much an A minor 6th missing its A as it is a C major
-- | triad. Pinning the bass to the root therefore needs the root said out loud,
-- | so the unit here is `Rooted`. That also matches how a generator in this
-- | family works: it picks a root, then a chord type, and never goes through a
-- | scale degree at all.
module Harmonia.OpenVoicing
  ( Rooted
  , Spread
  , Open
  , defaults
  , tonesOf
  , stackFrom
  , displace
  , spreads
  , candidates
  , topNote
  , span
  , openVoicing
  , playOpen
  ) where

import Prelude

import Data.Array (cons, filter, length, nub, range, scanl, sort, sortBy)
import Data.Foldable (sum)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Harmonia.Chord (Chord(..))
import Harmonia.Voicing (Voicing(..), voicingMidi)

-- | A chord together with the pitch class that is its root.
type Rooted = { root :: Int, chord :: Chord }

-- | How far each tone above the bass is lifted, in OCTAVES. One entry per
-- | non-bass tone, in ascending order of the close-position stack. All zeroes
-- | is close position; the bass has no entry because it never moves.
type Spread = Array Int

-- | `octave` — where the bass sits, MIDI-style (3 puts middle C's octave above
-- | it). `reach` — the most octaves any one tone may be lifted. `aim` —
-- | semitones above the bass to place the FIRST chord's top note, which is the
-- | only chord with no previous top note to follow. `minTones` — pad shorter
-- | chords up to this many notes.
type Open =
  { octave :: Int
  , reach :: Int
  , aim :: Int
  , minTones :: Int
  }

-- | Reach 2 is the useful range: three positions per tone, so a six-note chord
-- | offers 243 candidates and the search stays free. `aim` of 24 starts the top
-- | note two octaves over the bass, which is the texture the source describes
-- | as spreading *"over several octaves rather than compacted into a single
-- | octave"*. `minTones` of 4 is the padding rule.
defaults :: Open
defaults = { octave: 3, reach: 2, aim: 24, minTones: 4 }

-- | **The chord's tones, root first, padded by DOUBLING THE ROOT.**
-- |
-- | The source pads three-note chords *"to ensure they carry as much weight as
-- | other 4- or 5-note chords"* but does not say with what. Doubling the root
-- | is the choice here, and it is the conservative one: every other candidate
-- | tone would add a colour the chord does not have, and a padded triad that
-- | has quietly become an added-sixth is a worse outcome than a thin one.
-- |
-- | The duplicate is a real entry rather than a special case, so it takes its
-- | own octave in the stack and its own place in the spread — which is how a
-- | doubled root ends up somewhere useful rather than on top of the bass.
tonesOf :: Int -> Rooted -> Array Int
tonesOf minTones r =
  let
    rootPc = mod r.root 12
    Chord pcs = r.chord
    others = sort (filter (_ /= rootPc) (nub pcs))
    tones = cons rootPc others
    short = minTones - length tones
  in
    if short <= 0 then tones
    else tones <> Array.replicate short rootPc

-- | Stack pitch classes upward from a bass note, each strictly above the last.
-- | A repeated pitch class therefore lands an octave higher rather than on top
-- | of its twin, which is what makes the doubled root above audible.
stackFrom :: Int -> Array Int -> Array Int
stackFrom bass tones = case Array.uncons tones of
  Nothing -> []
  Just { tail: rest } -> cons bass (scanl step bass rest)
  where
  step prev pc =
    let d = mod (pc - mod prev 12 + 12) 12
    in prev + (if d == 0 then 12 else d)

-- | Lift each non-bass note by its octaves. The bass is untouched, which is the
-- | whole constraint; the result is re-sorted because a lifted inner voice can
-- | overtake one above it, and that crossing is allowed — it is texture, not an
-- | error.
displace :: Spread -> Array Int -> Array Int
displace sp notes = case Array.uncons notes of
  Nothing -> []
  Just { head: bass, tail: rest } ->
    if length sp /= length rest then notes
    else cons bass (sort (Array.zipWith (\k n -> n + 12 * k) sp rest))

-- | Every displacement of `n` non-bass tones within `reach`.
spreads :: Int -> Int -> Array Spread
spreads reach n =
  if n <= 0 then [ [] ]
  else do
    k <- range 0 (max 0 reach)
    rest <- spreads reach (n - 1)
    pure (cons k rest)

-- | Every open voicing of a chord available under these settings.
candidates :: Open -> Rooted -> Array Voicing
candidates o r =
  let
    tones = tonesOf o.minTones r
    bass = mod r.root 12 + 12 * (o.octave + 1)
    base = stackFrom bass tones
  in
    map (\sp -> Voicing (displace sp base)) (spreads o.reach (length base - 1))

topNote :: Voicing -> Maybe Int
topNote = Array.last <<< voicingMidi

span :: Voicing -> Int
span v = case Array.head (voicingMidi v), Array.last (voicingMidi v) of
  Just lo, Just hi -> hi - lo
  _, _ -> 0

-- | **The open voicing whose top note lands nearest a target, then whose inner
-- | voices move least.**
-- |
-- | The second key matters more than it looks. With the bass pinned to the root
-- | and the top note chosen, the SPAN is already decided — so every remaining
-- | candidate differs only in where the inner voices sit, and a tie-break on
-- | width would be choosing between things of equal width. Leaving it unbroken
-- | measured at seven times `voiceLead`'s total motion, which is not "different
-- | strategy", it is inner voices leaping for no reason.
-- |
-- | Collett is explicit that the source smooths both: its algorithms *"do their
-- | best to produce smooth outer and inner voice leading"*. The outer line wins
-- | where they conflict, and that ordering is the whole strategy.
-- |
-- | Distance to the nearest note of the previous voicing, rather than a
-- | position-by-position pairing, because the two voicings need not have the
-- | same number of notes and an octave-displaced chord has no stable notion of
-- | "the third voice".
nearestTop :: Maybe Voicing -> Int -> Array Voicing -> Maybe Voicing
nearestTop prev aim vs =
  Array.head (sortBy (comparing key) vs)
  where
  -- A `Tuple` rather than a record: record comparison orders by LABEL
  -- alphabetically, so the primary and secondary keys would be decided by
  -- what they happen to be called.
  key v = Tuple (topCost v) (innerCost v)

  topCost v = case topNote v of
    Nothing -> unreachable
    Just t -> abs (t - aim)

  -- With no previous chord there is no inner line to keep smooth, so the
  -- tie falls to TEXTURE: of the voicings reaching the target top note,
  -- prefer the one whose notes are most evenly spaced. Left unbroken, the
  -- opening chord came out as close position with one note hoisted to the
  -- top — E3 G3 under a B4, a sixteen-semitone hole in the middle — because
  -- all-zeroes is simply generated first. And the opening chord sets the
  -- texture for everything after it, since each later chord is then chosen
  -- to move least from the one before.
  innerCost v = case prev of
    Nothing -> widestGap (voicingMidi v)
    Just p -> sum (map (nearestIn (voicingMidi p)) (voicingMidi v))

  widestGap ns = case Array.head (sortBy (flip compare) (gaps ns)) of
    Nothing -> 0
    Just g -> g

  gaps ns = Array.zipWith (\a b -> b - a) ns (fromMaybe [] (Array.tail ns))

  nearestIn ps n = case Array.head (sort (map (\q -> abs (n - q)) ps)) of
    Nothing -> 0
    Just d -> d

  unreachable = 1000000
  abs n = if n < 0 then negate n else n

-- | One chord, opened, aiming its top note `aim` semitones above the bass.
-- | A `VoicingStrategy` cannot be used here: a strategy takes a `Voicing` and
-- | has already lost which pitch class was the root.
openVoicing :: Open -> Rooted -> Voicing
openVoicing o r =
  let bass = mod r.root 12 + 12 * (o.octave + 1)
  in fromMaybe (Voicing []) (nearestTop Nothing (bass + o.aim) (candidates o r))

-- | **A progression, voiced for a smooth top line.**
-- |
-- | The first chord aims at `aim`; every chord after it aims at the previous
-- | chord's top note. That is the whole algorithm, and it is deliberately
-- | greedy rather than a search over the whole progression: the source's
-- | generator is itself a walk in which each chord is chosen from the one
-- | before, so a voicing that looked ahead would be answering a question the
-- | music has not asked yet.
playOpen :: Open -> Array Rooted -> Array Voicing
playOpen o chords = case Array.uncons chords of
  Nothing -> []
  Just { head: first, tail: rest } ->
    let v0 = openVoicing o first
    in cons v0 (scanl step v0 rest)
  where
  step prev r =
    let aim = fromMaybe (12 * (o.octave + 1) + o.aim) (topNote prev)
    in fromMaybe (Voicing []) (nearestTop (Just prev) aim (candidates o r))
