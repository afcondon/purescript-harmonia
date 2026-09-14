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
  , Place(..)
  , omit
  , at
  , octaves
  , sounds
  , Spread
  , inPlace
  , Open
  , defaults
  , tonesOf
  , stackFrom
  , baseStack
  , displace
  , spreads
  , candidates
  , topNote
  , span
  , openVoicing
  , playOpen
  -- editing a voicing as a spread
  , applySpread
  , spreadOf
  , setTone
  , moveTone
  , doubleTone
  , thinTone
  , dropAt
  , toggleTone
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
-- | **Where one chord tone sounds: the octaves it is heard at, above its
-- | position in the stack.**
-- |
-- | A tone is not a note. `Place [0]` is the tone sounding once where the stack
-- | puts it, `Place [1]` an octave higher, `Place [0, 1]` the same tone doubled
-- | an octave up, and `Place []` the tone NOT PLAYED — which is Progressions'
-- | own `x`, the greyed-out note you can switch back on.
-- |
-- | Omission and doubling being one axis rather than two is the whole reason
-- | for the type. The ladder's three gestures are then edits to one value:
-- | drag moves an octave, ⌥-drag adds one, dragging the last one away omits
-- | the tone. A flag for "played" beside a lift would make those three gestures
-- | three mechanisms, and make `Place [0, 1]` unrepresentable.
newtype Place = Place (Array Int)

derive instance eqPlace :: Eq Place
derive instance ordPlace :: Ord Place

instance showPlace :: Show Place where
  show (Place ks) = "Place " <> show ks

-- | The tone is not played.
omit :: Place
omit = Place []

-- | The tone sounds once, `k` octaves above its stack position.
at :: Int -> Place
at k = Place [ k ]

-- | The octaves this tone sounds at, as given.
octaves :: Place -> Array Int
octaves (Place ks) = ks

-- | Is the tone heard at all?
sounds :: Place -> Boolean
sounds (Place ks) = not (Array.null ks)

-- | **A voicing as one place per chord tone** — the non-bass tones, in stack
-- | order. The bass is not here because it is not free: pinning it to the root
-- | is the strategy's one constraint.
-- |
-- | This is the form worth STORING. A `Spread` is a handful of small integers
-- | that means the same thing on any chord, so a voicing you liked can be kept
-- | and applied elsewhere; an array of absolute MIDI notes can only ever come
-- | back on the notes it was taken from.
type Spread = Array Place

-- | Every tone sounding once, where the stack puts it — the plain closed form
-- | a spread is edited away from.
inPlace :: Int -> Spread
inPlace n = Array.replicate (max 0 n) (at 0)

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
    else cons bass (sort (Array.concat (Array.zipWith spoken sp rest)))
  where
  spoken (Place ks) n = map (\k -> n + 12 * k) ks

-- | Every displacement of `n` non-bass tones within `reach`.
-- |
-- | **Deliberately neither omits nor doubles.** Those are edits a player makes,
-- | not moves a generator should make on its own: the source omits rarely
-- | (measured at 2% of chords, and only ever the fifth) and enumerating the
-- | omissions here would multiply a candidate space that is already
-- | `(reach+1)^n` for no musical gain.
spreads :: Int -> Int -> Array Spread
spreads reach n =
  if n <= 0 then [ [] ]
  else do
    k <- range 0 (max 0 reach)
    rest <- spreads reach (n - 1)
    pure (cons (at k) rest)

-- | Every open voicing of a chord available under these settings.
candidates :: Open -> Rooted -> Array Voicing
candidates o r =
  let
    tones = tonesOf o.minTones r
    bass = mod r.root 12 + 12 * (o.octave + 1)
    base = stackFrom bass tones
  in
    map (\sp -> Voicing (displace sp base)) (spreads o.reach (length base - 1))

-- ---------------------------------------------------------------------------
-- A voicing as an editable spread
-- ---------------------------------------------------------------------------

-- | The chord's tones stacked upward from the pinned bass, one entry per tone —
-- | the frame a `Spread` displaces. Public because an editor has to draw it: the
-- | ladder's rows ARE these positions, and an omitted tone still needs a row to
-- | be switched back on at.
baseStack :: Open -> Rooted -> Array Int
baseStack o r = stackFrom (mod r.root 12 + 12 * (o.octave + 1)) (tonesOf o.minTones r)

-- | Render a spread as notes. The inverse direction from `spreadOf`, and the
-- | one an editor runs on every gesture.
applySpread :: Open -> Rooted -> Spread -> Voicing
applySpread o r sp = Voicing (displace sp (baseStack o r))

-- | **Read an existing voicing back as a spread.**
-- |
-- | Not an inverse, and the docs should not pretend otherwise: `displace`
-- | re-sorts, so the correspondence between a note and the tone it came from is
-- | not carried in the result. This recovers it by MATCHING — each note is
-- | assigned to the lowest stack tone of its pitch class that sits at or below
-- | it, and the octave distance becomes the lift.
-- |
-- | Two consequences worth knowing before trusting it:
-- |
-- |   * When the stack doubles a tone (`minTones` pads short chords with extra
-- |     roots) two positions share a pitch class, and the match is greedy —
-- |     lowest first. The notes are right, which position they are credited to
-- |     may not be.
-- |   * A note whose pitch class is not in the chord at all cannot be placed and
-- |     is dropped. That is the honest answer: it was never a displacement of
-- |     this chord, so no spread describes it.
-- |
-- | It exists so that ONE editor serves every lens. A chord from the Banks
-- | generator knows its spread already; a chord off the tonnetz or the lattice
-- | does not, and still has to open in the same widget.
spreadOf :: Open -> Rooted -> Voicing -> Spread
spreadOf o r v = map (\b -> Place (sort (map (\n -> (n - b) / 12) (claimed b)))) positions
  where
  positions = fromMaybe [] (Array.tail (baseStack o r))
  uppers = case Array.uncons (voicingMidi v) of
    Nothing -> []
    Just { tail: rest } -> sort rest
  -- a note belongs to the LOWEST stack position sharing its pitch class that it
  -- sits at or above; greedy, so a doubled tone credits the lower position.
  owner n = Array.head (filter (\b -> mod (n - b) 12 == 0 && n >= b) positions)
  claimed b = filter (\n -> owner n == Just b) uppers

-- ---------------------------------------------------------------------------
-- The ladder's gestures, as edits to a spread
-- ---------------------------------------------------------------------------

-- | Set tone `i` outright — the primitive the other edits are conveniences
-- | over, and the one an editor wants when the gesture names its destination
-- | rather than a direction (clicking the octave you want, rather than dragging
-- | towards it).
setTone :: Int -> Place -> Spread -> Spread
setTone i pl = mapPlace i (const pl)

-- | Move tone `i`'s LOWEST sounding octave by `d`, clamped into `0 .. reach`.
-- | A plain drag: the tone keeps sounding once, at a new height.
moveTone :: Open -> Int -> Int -> Spread -> Spread
moveTone o i d = mapPlace i \(Place ks) -> case Array.head (sort ks) of
  Nothing -> Place ks
  Just k -> Place (sort (nub (cons (clampReach o (k + d)) (fromMaybe [] (Array.tail (sort ks))))))

-- | Add an octave copy of tone `i` above its current top — ⌥-drag. A tone that
-- | was omitted comes back in place, which is what the gesture should mean on a
-- | greyed row.
doubleTone :: Open -> Int -> Spread -> Spread
doubleTone o i = mapPlace i \(Place ks) -> case Array.last (sort ks) of
  Nothing -> at 0
  Just k -> Place (sort (nub (cons (clampReach o (k + 1)) ks)))

-- | Drop ONE copy of tone `i` — the one sounding at octave `k`. Removing the
-- | last copy leaves the tone omitted.
-- |
-- | Distinct from `thinTone`, and the distinction is the whole point of a
-- | `Place` holding several octaves: a doubled tone is ONE tone heard twice, so
-- | an editor that lets you click a note must be able to say WHICH note. Acting
-- | on the tone instead silences both copies at once, which reads as a bug
-- | however defensible the model is.
dropAt :: Int -> Int -> Spread -> Spread
dropAt i k = mapPlace i \(Place ks) -> Place (filter (_ /= k) ks)

-- | Drop tone `i`'s topmost copy; the last one leaves the tone omitted. The
-- | undo of `doubleTone`, and the way a fifth gets dropped.
thinTone :: Int -> Spread -> Spread
thinTone i = mapPlace i \(Place ks) ->
  Place (fromMaybe [] (Array.init (sort ks)))

-- | Silence the tone, or bring it back in place — the greyed-note click.
toggleTone :: Int -> Spread -> Spread
toggleTone i = mapPlace i \pl -> if sounds pl then omit else at 0

clampReach :: Open -> Int -> Int
clampReach o k = max 0 (min (max 0 o.reach) k)

mapPlace :: Int -> (Place -> Place) -> Spread -> Spread
mapPlace i f sp = fromMaybe sp (Array.modifyAt i f sp)

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
