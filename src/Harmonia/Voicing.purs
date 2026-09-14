-- | `Harmonia.Voicing` — concrete pitch & voice leading.
-- |
-- | Voicings carry a chord's pitch classes at concrete octaves — ordered low
-- | to high, as MIDI note numbers. The lift
-- | `closeVoicing :: { centre } -> Chord -> Voicing` produces a default
-- | close-position voicing at the given octave; everything else is a
-- | `Voicing -> Voicing` transformation that composes through ordinary
-- | function composition.
-- |
-- | A `Selector` carves a sub-chord out of an existing chord or voicing — by
-- | position in the sorted pitch-class array (for Chord) or by position in the
-- | low-to-high voicing (for Voicing). The vocabulary is small: TakeLow /
-- | TakeHigh / TakeRange / TakeIndices / TakeEvery / DropS.
-- |
-- | `voiceLead` / `enumerateVoicings` move between chords by minimum total
-- | semitone motion; `play` realises a whole `Progression` with voice-leading
-- | carry-through. Pure `Prelude`/`Data.*`; depends only on `Harmonia.Chord`.
module Harmonia.Voicing
  ( Voicing(..)
  , voicingMidi
  , closeVoicing
  -- Primitives
  , openTriad
  , rootless
  , drop2
  , drop2and4
  , quartal
  , cluster
  , spread
  -- Composition alias
  , VoicingStrategy
  -- Selectors
  , Selector(..)
  , takeChord
  , takeVoicing
  -- Repositioning — same notes, different arrangement
  , invert
  , transposeOctaves
  , refoot
  , slash
  , nearestOctave
  , bassPitchClass
  , nextBassTone
  -- Voice leading (V-C)
  , Progression
  , voiceLead
  , enumerateVoicings
  , play
  , playFrom
  , nearestNote
  ) where

import Prelude

import Data.Array as Array
import Data.Array (cons, deleteAt, filter, nub, range, sort, (!!), zipWith)
import Data.Foldable (elem, foldl, sum)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)

import Harmonia.Chord (Chord(..), DegreeChord, Key, realize)

-- ---------------------------------------------------------------------------
-- Voicing — sorted-ascending array of MIDI note numbers
-- ---------------------------------------------------------------------------

-- | A voicing is an ordered array of MIDI note numbers, sorted
-- | ascending (low to high).  Each element is a concrete realisable
-- | note; the octave is implicit in the number (`n `div` 12`).
-- |
-- | MIDI convention: middle C (C4) is 60; octave 4 spans 60..71.
newtype Voicing = Voicing (Array Int)

derive instance eqVoicing :: Eq Voicing

instance showVoicing :: Show Voicing where
  show (Voicing xs) = "Voicing " <> show xs

-- | Extract the MIDI note numbers from a voicing.
voicingMidi :: Voicing -> Array Int
voicingMidi (Voicing xs) = xs

-- ---------------------------------------------------------------------------
-- closeVoicing — the one Chord → Voicing lift
-- ---------------------------------------------------------------------------

-- | Produce a default close-position voicing of a chord, placed so
-- | all notes sit in the named octave.  Chord pitch classes are
-- | played in ascending pitch-class order at the centre octave.
-- |
-- | `closeVoicing { centre: 4 } (Chord [0, 4, 7]) = Voicing [60, 64, 67]`
-- |
-- | Doesn't read slash info from the source DegreeChord — the slash
-- | was baked into the Chord's pitch-class set at realize time and
-- | the bass-below-other-notes arrangement is the caller's choice
-- | via `spread` or octave-shifts.
closeVoicing :: { centre :: Int } -> Chord -> Voicing
closeVoicing { centre } (Chord pcs) =
  Voicing (map (\pc -> pc + 12 * (centre + 1)) (sort pcs))

-- ---------------------------------------------------------------------------
-- Voicing transformations — composable through (<<<)
-- ---------------------------------------------------------------------------

-- | A voicing strategy is a transformation on a voicing.  Strategies
-- | compose through ordinary function composition.
type VoicingStrategy = Voicing -> Voicing

-- | Open the triad: for a 3-note voicing [root, 3rd, 5th] produces
-- | [root, 5th, 3rd-an-octave-up].  More generally: lifts the second-
-- | from-bottom note up an octave and re-sorts.  No-op on voicings
-- | shorter than 2 notes.
openTriad :: Voicing -> Voicing
openTriad (Voicing xs) =
  if Array.length xs < 2 then Voicing xs
  else case xs !! 1, deleteAt 1 xs of
    Just second, Just rest -> Voicing (sort (cons (second + 12) rest))
    _, _ -> Voicing xs

-- | Omit the bottom note (root).
rootless :: Voicing -> Voicing
rootless (Voicing xs) = case Array.uncons xs of
  Just { tail } -> Voicing tail
  Nothing -> Voicing xs

-- | Drop-2 voicing: lower the 2nd-highest note an octave.  Standard
-- | jazz transformation that "opens" a close-position voicing.
-- |
-- | `drop2 (Voicing [60, 64, 67, 71]) = Voicing [55, 60, 64, 71]`
-- | (Cmaj7 close → G2 C E B drop-2)
drop2 :: Voicing -> Voicing
drop2 (Voicing xs) =
  let n = Array.length xs
  in if n < 2 then Voicing xs
     else case xs !! (n - 2), deleteAt (n - 2) xs of
       Just second, Just rest -> Voicing (sort (cons (second - 12) rest))
       _, _ -> Voicing xs

-- | Drop the 2nd and 4th notes from the top, each down an octave.
-- | For 4-note voicings: lower the second-from-top and bottom note.
-- | Falls back to `drop2` on voicings shorter than 4 notes.
drop2and4 :: Voicing -> Voicing
drop2and4 v@(Voicing xs) =
  let n = Array.length xs
  in if n < 4 then drop2 v
     else
       case xs !! (n - 2), xs !! (n - 4) of
         Just two, Just four ->
           case deleteAt (n - 2) xs >>= deleteAt (n - 4) of
             Just rest -> Voicing (sort (cons (two - 12) (cons (four - 12) rest)))
             Nothing -> Voicing xs
         _, _ -> Voicing xs

-- | Restack the voicing's pitch classes in cycle-of-4ths order from
-- | the bottom note, placing each subsequent note at the lowest octave
-- | giving at least a perfect-4th interval (5 semitones) above the
-- | previous.  Quartal voicings sound "open" and modal; works well on
-- | chords that contain a 4th-stack (sus4, m11, jazz quartals), less
-- | well on pure triads where the natural intervals are 3rds.
-- |
-- | `quartal (Voicing [60, 65, 70, 67]) = Voicing [60, 65, 70, 79]`
-- | (sus chord C F Bb G → C F Bb G5; G placed above Bb)
quartal :: Voicing -> Voicing
quartal (Voicing xs) = case Array.uncons xs of
  Nothing -> Voicing []
  Just { head: bottom } ->
    let
      pcsPresent = nub (map (\n -> n `mod` 12) xs)
      bottomPc = bottom `mod` 12
      -- pitch classes in cycle-of-4ths order from the bottom PC,
      -- filtered to those actually in the chord
      cycleOrder = filter (\pc -> elem pc pcsPresent)
                          (map (\i -> (bottomPc + 5 * i) `mod` 12) (range 0 11))
      -- skip the first (it's the bottom's PC, already placed)
      restPcs = fromMaybe [] (Array.tail cycleOrder)
      stacked = foldl placeNext [bottom] restPcs
    in
      Voicing stacked
  where
    placeNext acc pc = case Array.last acc of
      Nothing -> acc
      Just prev ->
        let
          target = prev + 5
          base = (target `div` 12) * 12
          candidate = base + pc
          n = if candidate >= target then candidate else candidate + 12
        in acc <> [n]

-- | Compress the voicing into the smallest octave window — distinct
-- | pitch classes in ascending order at the bottom note's octave.
-- | Effectively `closeVoicing` applied to whatever PCs are present.
cluster :: Voicing -> Voicing
cluster (Voicing xs) = case Array.uncons xs of
  Nothing -> Voicing []
  Just { head: bottom } ->
    let
      bottomOct = bottom `div` 12
      pcs = sort (nub (map (\n -> n `mod` 12) xs))
    in
      Voicing (map (\pc -> pc + 12 * bottomOct) pcs)

-- | Distribute the voicing's notes across an octave range.  Lowest
-- | note shifted to (or near) the `low` octave, highest to (or near)
-- | the `high` octave, intermediate notes spaced evenly across the
-- | range.  Preserves pitch classes.
spread :: { low :: Int, high :: Int } -> Voicing -> Voicing
spread { low, high } (Voicing xs) =
  let n = Array.length xs
  in if n == 0 then Voicing []
     else if n == 1
       then Voicing (map (\note -> shiftToOctave low note) xs)
     else
       let
         lowMidi = 12 * (low + 1)
         highMidi = 12 * (high + 1) + 11
         span = highMidi - lowMidi
         step = if n <= 1 then 0 else span / (n - 1)
         placed = Array.mapWithIndex
           (\i note ->
              let
                pc = note `mod` 12
                targetMidi = lowMidi + step * i
                targetOctave = targetMidi `div` 12
                candidate = pc + 12 * targetOctave
              in
                nearest candidate targetMidi)
           xs
       in
         Voicing (sort placed)
  where
    shiftToOctave o n = (n `mod` 12) + 12 * (o + 1)
    nearest candidate target =
      let
        below = candidate - 12
        above = candidate + 12
        d0 = absVal (candidate - target)
        dBelow = absVal (below - target)
        dAbove = absVal (above - target)
      in
        if dBelow < d0 && dBelow <= dAbove then below
        else if dAbove < d0 then above
        else candidate
    absVal n = if n < 0 then -n else n

-- ---------------------------------------------------------------------------
-- Selectors — sub-chord plumbing
-- ---------------------------------------------------------------------------

-- | A Selector carves a sub-chord out of a chord or voicing.  Output
-- | is the same type as input, so selectors are composable and feed
-- | back into the Notation fabric like any other chord-shaped value.
data Selector
  = TakeLow Int             -- ^ The N lowest voices.
  | TakeHigh Int            -- ^ The N highest voices.
  | TakeRange Int Int       -- ^ Voices [i..j) — half-open.
  | TakeIndices (Array Int) -- ^ Explicit voice indices (0-based, low-to-high).
  | TakeEvery Int Int       -- ^ (offset, stride) — modulo selector.
  | DropS Selector          -- ^ Complement of a selector.

derive instance eqSelector :: Eq Selector

instance showSelector :: Show Selector where
  show = case _ of
    TakeLow n        -> "TakeLow " <> show n
    TakeHigh n       -> "TakeHigh " <> show n
    TakeRange i j    -> "TakeRange " <> show i <> " " <> show j
    TakeIndices xs   -> "TakeIndices " <> show xs
    TakeEvery o s    -> "TakeEvery " <> show o <> " " <> show s
    DropS s          -> "DropS (" <> show s <> ")"

-- | Apply a selector to a Chord's sorted pitch-class array.  Positions
-- | are interpreted against the sort order; the lowest PC numerically
-- | is position 0.  Output is itself a Chord, so selectors chain.
takeChord :: Selector -> Chord -> Chord
takeChord sel (Chord pcs) =
  Chord (selectFrom sel (sort (nub pcs)))

-- | Apply a selector to a Voicing.  Positions are interpreted against
-- | the low-to-high order; the bottom voice is position 0.
takeVoicing :: Selector -> Voicing -> Voicing
takeVoicing sel (Voicing notes) =
  Voicing (selectFrom sel notes)

-- | Selector arithmetic over an ordered array.  Resolves to selected
-- | indices first, then looks them up — avoids an Eq constraint
-- | on the element type.
selectFrom :: forall a. Selector -> Array a -> Array a
selectFrom sel xs =
  let ixs = selectIndices sel (Array.length xs)
  in  Array.mapMaybe (\i -> xs !! i) ixs

-- | Resolve a Selector to the indices it picks, given an array length.
selectIndices :: Selector -> Int -> Array Int
selectIndices sel len = case sel of
  TakeLow n
    | n <= 0 || len <= 0 -> []
    | otherwise          -> allIndices (min n len)
  TakeHigh n
    | n <= 0 || len <= 0 -> []
    | otherwise          ->
        let start = max 0 (len - n)
        in if start >= len then [] else rangeIncl start (len - 1)
  TakeRange i j ->
    let lo = max 0 i
        hi = min len (max 0 j) - 1
    in if hi < lo then [] else rangeIncl lo hi
  TakeIndices ixs ->
    filter (\i -> i >= 0 && i < len) ixs
  TakeEvery offset stride ->
    let validStride = if stride < 1 then 1 else stride
        countMax = if validStride == 0 then 0 else (len + validStride) / validStride
        candidates = map (\k -> offset + k * validStride) (allIndices countMax)
    in  filter (\i -> i >= 0 && i < len) candidates
  DropS inner ->
    let kept = selectIndices inner len
    in  filter (\i -> not (elem i kept)) (allIndices len)

allIndices :: Int -> Array Int
allIndices len
  | len <= 0  = []
  | otherwise = rangeIncl 0 (len - 1)

rangeIncl :: Int -> Int -> Array Int
rangeIncl lo hi
  | hi < lo   = []
  | otherwise = range lo hi

-- ---------------------------------------------------------------------------
-- Repositioning — the transforms that move a chord without changing it
-- ---------------------------------------------------------------------------
--
-- Three operations that share one discipline: **they preserve the note count
-- and the pitch-class content, always.** Inverting, transposing by octaves and
-- re-footing all rearrange a chord; none of them is allowed to add or lose a
-- note. Each has been written the obvious way at least once and each obvious
-- way was wrong, so the laws in `VoicingSpec` are the specification and these
-- are merely an implementation of it.
--
-- All three take a `Voicing` whose LOWEST note is its bass, and return one with
-- the same property. Applied to a direction they are `VoicingStrategy`s, so
-- they compose with the rest of this module.

-- | The pitch class in the bass — the lowest note's, not the chord's root.
bassPitchClass :: Voicing -> Int
bassPitchClass v = case Array.head (sort (voicingMidi v)) of
  Nothing -> 0
  Just b -> mod b 12

-- | **Invert: the lowest note up an octave, or the highest down.**
-- |
-- | The textbook operation, and the trap is what you apply it to. Inverting the
-- | UPPER voices of a voicing whose root sits in the bass — which is what
-- | `openVoicing` produces, and what a keyboard pad wants — deletes the root:
-- | rotating the uppers of C·G·B·E puts G at the bottom and the C is simply
-- | gone. The whole voicing, bass included, is the unit.
-- |
-- | ⟲ and ⟳ are NOT exact inverses on a widely-spaced chord, and cannot be:
-- | "lowest up" and "highest down" only undo each other when the note that
-- | moved up lands on top, which is true in close position and not otherwise.
-- | Both preserve the chord, which is the property worth having.
invert :: Int -> Voicing -> Voicing
invert dir v =
  let ns = sort (voicingMidi v)
  in if dir > 0 then case Array.uncons ns of
       Just { head: lo, tail: rest } -> Voicing (sort (Array.snoc rest (lo + 12)))
       Nothing -> v
     else case Array.unsnoc ns of
       Just { init: rest, last: hi } -> Voicing (sort (cons (hi - 12) rest))
       Nothing -> v

-- | Move a whole voicing by `d` octaves. Unbounded on purpose — what counts as
-- | a playable register belongs to whatever is going to sound it, not to the
-- | theory.
transposeOctaves :: Int -> Voicing -> Voicing
transposeOctaves d v = Voicing (map (_ + 12 * d) (voicingMidi v))

-- | The sounding pitch class `dir` steps around the cycle from the bass.
-- |
-- | **Sounding**, not merely a member of the chord: a tone that is not being
-- | played has no copy for `refoot` to trade with, so re-footing onto it could
-- | only preserve the chord by growing it.
nextBassTone :: Int -> Voicing -> Int
nextBassTone dir v =
  let tones = sort (nub (map (\m -> mod m 12) (voicingMidi v)))
      n = Array.length tones
  in case Array.findIndex (_ == bassPitchClass v) tones of
       Just i | n > 0 -> fromMaybe (bassPitchClass v) (tones !! mod (i + dir + n) n)
       _ -> bassPitchClass v

-- | **Re-foot the chord on another of its sounding tones: invert until that
-- | tone is in the bass.**
-- |
-- | Defining it as repeated `invert` rather than as its own surgery is the
-- | whole trick. Two attempts at the surgery failed on this corpus — putting
-- | the new tone at the octave nearest the old bass lands it ABOVE the other
-- | notes in a close voicing (473 of 960 voicings ended up with the wrong note
-- | lowest), and putting it strictly below drags the chord's register down a
-- | little further on every re-footing. Inverting has neither problem, cannot
-- | lose a note because `invert` cannot, and says something true: re-footing IS
-- | a run of inversions, and the two controls are the same control at different
-- | granularities.
-- |
-- | A pitch class the chord is not sounding leaves it untouched — there is no
-- | copy to bring down, and inventing one would change the chord.
refoot :: Int -> Voicing -> Voicing
refoot pc v = go (2 * Array.length (voicingMidi v) + 1) v
  where
  go n w
    | bassPitchClass w == mod pc 12 = w
    | n <= 0 = v
    | otherwise = go (n - 1) (invert 1 w)

-- | The octave of `pc` closest to the note `near`.
nearestOctave :: Int -> Int -> Int
nearestOctave pc near =
  let up = near + mod (mod pc 12 - mod near 12 + 12) 12
      down = up - 12
  in if near - down <= up - near then down else up

-- | **Slash: put a tone underneath and leave the upper structure alone.**
-- |
-- | The other half of `refoot`, and genuinely a different operation rather than
-- | a variation on it. `refoot` inverts until the wanted tone is lowest, which
-- | rotates the whole structure and closes any gap the chord had; `slash` keeps
-- | the structure exactly as voiced and changes only what is beneath it. That
-- | is what the name says — C/E is C·E·G with an E under it, not E·G·C — and it
-- | is what a chord with a deliberately low bass needs, since inverting such a
-- | chord collapses the very spacing that makes it a pad.
-- |
-- | Content and count still survive, by the same trade `refoot` avoids needing:
-- | if the outgoing bass tone no longer sounds anywhere, the incoming tone gives
-- | up its lowest upper copy and the outgoing one takes that place. And the new
-- | bass drops an octave if the nearest one would not have been lowest, which is
-- | what makes it work on a close voicing as well as a spread one.
slash :: Int -> Voicing -> Voicing
slash pc v =
  let ns = sort (voicingMidi v)
      oldPc = bassPitchClass v
  in case Array.uncons ns of
       Nothing -> v
       Just { head: bass, tail: ups }
         | mod pc 12 == oldPc -> v
         | otherwise ->
             let
               cand = nearestOctave pc bass
               newBass = case Array.head ups of
                 Just u | cand >= u -> cand - 12
                 _ -> cand
             in
               if elem oldPc (map (\m -> mod m 12) ups) then Voicing (sort (cons newBass ups))
               else case Array.find (\m -> mod m 12 == mod pc 12) ups of
                 Nothing -> Voicing (sort (cons newBass ups))
                 Just taken ->
                   Voicing (sort (cons newBass
                     (cons (nearestOctave oldPc taken) (filter (_ /= taken) ups))))

-- ---------------------------------------------------------------------------
-- Voice leading — V-C
-- ---------------------------------------------------------------------------

-- | A progression is an ordered list of chord recipes.
type Progression = Array DegreeChord

-- | Voice-lead from the current voicing into the next chord.  Returns
-- | a voicing of `nextChord` whose notes are as close as possible to
-- | `currentVoicing` — common pitch classes stay at the same MIDI
-- | number, non-common ones move to the nearest octave.
-- |
-- | Implementation: enumerate every permutation of the next chord's
-- | distinct pitch classes (factorial in chord size — fine for ≤7
-- | voices), pair each current voice with one PC via nearest-octave
-- | placement, score by total |motion|, return the minimum.
-- |
-- | If the current voicing and next chord have different sizes, falls
-- | back to `closeVoicing` centred on the current voicing's bottom
-- | octave.  Voice-counts-changing-mid-progression is V-D territory.
voiceLead :: Voicing -> Chord -> Voicing
voiceLead (Voicing []) chord =
  closeVoicing { centre: 4 } chord
voiceLead voicing@(Voicing current) (Chord pcs) =
  let
    nextPcs = nub pcs
    n = Array.length current
    m = Array.length nextPcs
  in
    if n /= m
      then closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)
      else case fst <$> bestPerm current nextPcs of
        Just v  -> v
        Nothing -> closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)

-- | All distinct (voicing, motion) candidates for voice-leading from
-- | a current voicing into a next chord, sorted by motion ascending.
-- | The smallest-motion result is `voiceLead`'s output; subsequent
-- | entries are progressively less smooth alternatives.
-- |
-- | Same size constraint as `voiceLead` — returns a single fallback
-- | entry on size mismatch.
enumerateVoicings :: Voicing -> Chord -> Array (Tuple Voicing Int)
enumerateVoicings (Voicing []) chord =
  [ Tuple (closeVoicing { centre: 4 } chord) 0 ]
enumerateVoicings voicing@(Voicing current) (Chord pcs) =
  let
    nextPcs = nub pcs
    n = Array.length current
    m = Array.length nextPcs
  in
    if n /= m
      then [ Tuple (closeVoicing { centre: bottomOctave voicing } (Chord nextPcs)) 0 ]
      else
        Array.sortBy (compare `on` snd)
          (Array.nubByEq (\a b -> fst a == fst b)
             (map (scorePerm current) (permutations nextPcs)))
  where
    snd (Tuple _ s) = s

-- | Glue: realise a progression in a key, applying the voicing
-- | strategy to the first chord and voice-leading every subsequent
-- | one from the previous voicing.  Returns one Voicing per
-- | DegreeChord in the progression.
-- |
-- | `centre` is the octave the first voicing centres on; subsequent
-- | voicings drift via voice-leading.  See `play` for a centre-4
-- | default.
playFrom :: Int -> Key -> VoicingStrategy -> Progression -> Array Voicing
playFrom centre key strategy chords =
  case Array.uncons chords of
    Nothing -> []
    Just { head: first, tail: rest } ->
      let
        firstV = strategy (closeVoicing { centre } (realize key first))
      in
        cons firstV
          (Array.scanl (\prev dc -> voiceLead prev (realize key dc)) firstV rest)

-- | `play` with the default centre octave 4 (middle C area).
play :: Key -> VoicingStrategy -> Progression -> Array Voicing
play = playFrom 4

-- ---------------------------------------------------------------------------
-- Voice-leading internals
-- ---------------------------------------------------------------------------

-- | The best (lowest-motion) permutation pairing of current voices to
-- | next-chord PCs.  Returns Nothing if no permutations exist (i.e.
-- | empty next-chord PC list).
bestPerm :: Array Int -> Array Int -> Maybe (Tuple Voicing Int)
bestPerm current nextPcs = case permutations nextPcs of
  [] -> Nothing
  perms ->
    let scored = map (scorePerm current) perms
        sorted = Array.sortBy (compare `on` (\(Tuple _ s) -> s)) scored
    in  Array.head sorted

-- | Score one permutation: pair each current voice with the nth PC
-- | from the perm, place that PC at the octave closest to the voice,
-- | sum the absolute motions.
scorePerm :: Array Int -> Array Int -> Tuple Voicing Int
scorePerm current perm =
  let
    placed = zipWith nearestNote current perm
    motion = sum (zipWith (\c p -> absInt (c - p)) current placed)
  in
    Tuple (Voicing (sort placed)) motion

-- | Place a pitch class at the octave whose MIDI number is closest to
-- | the target.  Checks the natural octave, one above, and one below.
nearestNote :: Int -> Int -> Int
nearestNote target pc =
  let
    -- truncating div toward zero; for positive (target - pc) this is floor
    base = (target - pc) `div` 12
    c0 = pc + 12 * base
    cAbove = c0 + 12
    cBelow = c0 - 12
    d0 = absInt (c0 - target)
    dA = absInt (cAbove - target)
    dB = absInt (cBelow - target)
  in
    if dA < d0 && dA <= dB then cAbove
    else if dB < d0 && dB < dA then cBelow
    else c0

-- | The bottom note's octave in MIDI convention (octave 4 = middle-C
-- | octave).  Used as the centre when voicing-leading falls back to
-- | closeVoicing on size mismatch.
bottomOctave :: Voicing -> Int
bottomOctave (Voicing xs) = case Array.head xs of
  Just n  -> (n `div` 12) - 1
  Nothing -> 4

-- | All permutations of an array (recursive; factorial in size).
permutations :: forall a. Array a -> Array (Array a)
permutations xs = case Array.length xs of
  0 -> [[]]
  _ ->
    Array.concatMap
      (\i -> case xs !! i, deleteAt i xs of
         Just x, Just rest -> map (cons x) (permutations rest)
         _, _              -> [])
      (allIndices (Array.length xs))

absInt :: Int -> Int
absInt n = if n < 0 then -n else n
