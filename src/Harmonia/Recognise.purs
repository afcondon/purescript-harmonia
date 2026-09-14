-- | `Harmonia.Recognise` — the inverse of `Harmonia.Chord.realize`.
-- |
-- | `realize` goes recipe → pitch classes. This module goes the other way: a
-- | bag of pitch classes in, RANKED chord readings out. The caller is a sampler
-- | workflow — somebody strikes a chord, an onset detector cuts it out, a pitch
-- | estimator says "these pitch classes are present, and this one is in the
-- | bass" — and the UI wants a *suggested* name a human can accept or overtype.
-- |
-- | Two facts about that job shape everything here.
-- |
-- |   * **The input is noisy.** A chroma estimate over real audio reports pitch
-- |     classes that are not in the chord (harmonics of the real notes, room,
-- |     the tail of the last chord) and drops ones that are (a quiet fifth). So
-- |     recognition is a SCORING problem, never an exact-match lookup.
-- |
-- |   * **A pitch-class set is ambiguous even when perfect.** `{9,0,4,7}` is
-- |     both Am7 and C6; a diminished seventh has four equally good roots; a
-- |     `sus2` is somebody else's `sus4` a fifth up. Returning several ranked
-- |     readings is CORRECT behaviour, not a failure. A known bass note is the
-- |     strongest disambiguator we have, which is what `bass` is for.
-- |
-- | The vocabulary is a fixed table of `Template`s, and each template's pitch
-- | classes are produced by calling `realize` itself (at `I` of an Ionian key on
-- | the candidate root). That is deliberate: the recogniser cannot drift away
-- | from the thing it inverts, because it shares its arithmetic.
-- |
-- | ## The scoring model
-- |
-- | Every (root, template) pair — 12 × `length templates` of them — is scored:
-- |
-- | ```
-- |   score = (matchedWeight - missFactor * missingWeight) / templateWeight
-- |         - Σ extraCost(interval)      -- observed notes the template can't explain
-- |         + prior                      -- how common this chord type is
-- |         + bass adjustment            -- root in the bass, an inversion, or neither
-- | ```
-- |
-- | A perfect match scores 1.0 plus its prior. Tones carry weights by ROLE
-- | (`intervalWeight`): the root and third are what identify a chord, the
-- | seventh colours it, and the perfect fifth is nearly free — which is exactly
-- | the note real players and real pitch estimators drop. Extra notes are
-- | penalised in proportion to how FOREIGN they are (`extraCost`): a 9th or 13th
-- | over a triad is almost free, a contradicting third is expensive.
-- | Normalising the fit by template weight but NOT the extras is what keeps the
-- | recogniser parsimonious — a big template must earn its extra tones, and a
-- | two-note power chord cannot win by leaving three notes unexplained.
-- |
-- | The prior and the bass adjustment are the two pieces of evidence that are
-- | not about the notes, and they are deliberately of comparable size (see
-- | `Template` and `rootBassBonus`): commonness can outvote a bass note, so
-- | "E G B C over an E bass" comes back as Cmaj7/E rather than E minor ♭13, but
-- | neither can outvote a note that is actually there.
-- |
-- | ## Where it is known to be weak
-- |
-- |   * **Overtones read as contradictions.** The loudest partials of a note are
-- |     its fifth and its major third, so a chroma estimator will sometimes
-- |     report a third that nobody played — and `extraCost` charges 0.45 for a
-- |     third. We accept that: making thirds cheap would make every major triad
-- |     an equally good minor seventh, which is worse.
-- |
-- |   * **Spelling.** Harmonia has no spelled-pitch type — D♯ and E♭ are one
-- |     pitch class — so absolute names use SHARPS throughout ("D#m7", never
-- |     "Ebm7"). Spelling is a notation concern; see the README on PSoM.
-- |
-- |   * **Enharmonic recipes are not recovered, only their sound.** `Add 4` and
-- |     `Add 11` are the same interval, so the table picks one spelling
-- |     (`Add 11`); `borrow` that does not move the root is invisible in the
-- |     pitch classes and cannot be recovered. Recognition round-trips the
-- |     SOUND of a `DegreeChord`, not its text.
-- |
-- |   * **The key only disambiguates through the root.** `recogniseInKey` gives
-- |     a bonus to a root that is diatonic to the key, but does not ask whether
-- |     the chord's colour tones belong to the key. A chromatic passing chord on
-- |     a diatonic root is therefore rated as highly as a diatonic one.
-- |
-- | Pure `Prelude`/`Data.*` — no `Effect`, no FFI, no new dependency — so it
-- | compiles unchanged under the JS backend and purerl (Erlang).
module Harmonia.Recognise
  ( Observation
  , observe
  , observeWithBass
  , Score(..)
  , scoreNumber
  , Fit
  , Template(..)
  , tpl
  , templates
  , templateChord
  , templateSuffix
  , Candidate(..)
  , candidateRoot
  , candidateScore
  , candidateChord
  , candidateName
  , candidateFit
  , recognise
  , recogniseWith
  , recogniseTop
  , best
  , KeyedCandidate(..)
  , keyedChord
  , keyedScore
  , recogniseInKey
  , recogniseInKeyTop
  , bestInKey
  , degreeFor
  , pitchClassName
  , scoreFloor
  , intervalWeight
  , extraCost
  ) where

import Prelude

import Data.Array (catMaybes, concatMap, cons, filter, head, nub, null, range, sort, sortBy, take, (!!))
import Data.Foldable (elem, find, findMap, sum)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)

import Harmonia.Chord
  ( Chord(..)
  , DegreeChord
  , Key
  , Mode(..)
  , Numeral(..)
  , Quality(..)
  , Tension(..)
  , borrow
  , deg
  , modeIntervals
  , numeralIndex
  , realize
  , slashed
  )

-- ---------------------------------------------------------------------------
-- Observation — what somebody else measured
-- ---------------------------------------------------------------------------

-- | The input: pitch classes somebody else computed, plus the bass if it is
-- | known. Order and duplication in `pitches` are irrelevant — `observe`
-- | normalises to a sorted, deduplicated set in 0..11. `bass` is a pitch class
-- | too, not a MIDI note: this layer never sees register.
type Observation =
  { pitches :: Array Int
  , bass :: Maybe Int
  }

-- | An observation with no known bass. Every reading is then equally entitled
-- | to claim the root, and inversions cannot be told from their relative-minor
-- | twins.
observe :: Array Int -> Observation
observe pcs = { pitches: normalisePCs pcs, bass: Nothing }

-- | An observation whose lowest sounding pitch class is known. The bass is
-- | folded into `pitches` as well — a bass note is a note.
observeWithBass :: Int -> Array Int -> Observation
observeWithBass b pcs =
  { pitches: normalisePCs (cons b pcs), bass: Just (pmod b 12) }

normalisePCs :: Array Int -> Array Int
normalisePCs = nub <<< sort <<< map (\pc -> pmod pc 12)

-- ---------------------------------------------------------------------------
-- Score
-- ---------------------------------------------------------------------------

-- | How well a reading explains an observation. 1.0 + prior is a perfect,
-- | note-for-note match; anything above `scoreFloor` is worth showing.
newtype Score = Score Number

derive instance eqScore :: Eq Score
derive instance ordScore :: Ord Score

instance showScore :: Show Score where
  show (Score n) = "Score " <> show n

scoreNumber :: Score -> Number
scoreNumber (Score n) = n

-- | Readings below this are not returned. Set where a chord missing its third
-- | AND carrying a foreign note still survives, but a coincidence does not.
scoreFloor :: Number
scoreFloor = 0.35

-- ---------------------------------------------------------------------------
-- Templates — the vocabulary
-- ---------------------------------------------------------------------------

-- | A chord type, root-relative: the `Quality` and `Tension`s that
-- | `Harmonia.Chord` would use to build it, the suffix its absolute name takes,
-- | and a `prior` standing for how common the chord type is. Priors run 0.0 to
-- | 0.15, and the two ends of that range are chosen, not arbitrary: a prior gap
-- | is big enough to settle "was that fifth really absent?" (a missed fifth
-- | costs about 0.08) and to stop an exotic reading taking the top slot on a
-- | bass bonus alone, but never bigger than the CHEAPEST unexplained note
-- | (0.15) — so commonness can never outweigh a note that is actually there.
newtype Template = Template
  { quality :: Quality
  , tensions :: Array Tension
  , suffix :: String
  , prior :: Number
  }

derive instance eqTemplate :: Eq Template

instance showTemplate :: Show Template where
  show (Template t) = "Template " <> show t.suffix

tpl :: Quality -> Array Tension -> String -> Number -> Template
tpl q ts sfx p = Template { quality: q, tensions: ts, suffix: sfx, prior: p }

templateSuffix :: Template -> String
templateSuffix (Template t) = t.suffix

-- | A template's pitch classes on a given root — computed by `realize` itself,
-- | so the recogniser's vocabulary is exactly the vocabulary it inverts.
templateChord :: Int -> Template -> Chord
templateChord rootPC (Template t) =
  realize { tonic: pmod rootPC 12, mode: Ionian } (deg I t.quality t.tensions)

-- | The recognition vocabulary. Ordinary before exotic, and no two entries
-- | produce the same interval set (a duplicate would only ever split a score
-- | between two names for one sound). Chords that merely OMIT a tone —
-- | "maj7 no 5" — are deliberately absent: the missing-tone penalty already
-- | handles those, and adding them would double-count.
templates :: Array Template
templates =
  -- triads and plain sevenths: the load-bearing part of the table
  [ tpl Maj [] "" 0.150
  , tpl Min [] "m" 0.145
  , tpl Dom7 [] "7" 0.140
  , tpl Maj7 [] "maj7" 0.138
  , tpl Min7 [] "m7" 0.138
  , tpl Dim [] "dim" 0.090
  , tpl HalfDim [] "m7b5" 0.085
  , tpl FullyDim [] "dim7" 0.070
  , tpl Aug [] "aug" 0.044
  , tpl MinMaj7 [] "mMaj7" 0.034
  , tpl Aug [ Flat 7 ] "7#5" 0.018
  , tpl AugMaj7 [] "maj7#5" 0.010
  -- suspensions
  , tpl Maj [ Sus 4 ] "sus4" 0.095
  , tpl Dom7 [ Sus 4 ] "7sus4" 0.090
  , tpl Maj [ Sus 2 ] "sus2" 0.082
  , tpl Dom7 [ Sus 4, Add 9 ] "9sus4" 0.042
  , tpl Maj7 [ Sus 4 ] "maj7sus4" 0.030
  , tpl HalfDim [ Sus 4 ] "m7b5sus4" 0.006
  -- sixths and adds
  , tpl Maj [ Add 6 ] "6" 0.095
  , tpl Maj [ Add 9 ] "add9" 0.082
  , tpl Min [ Add 6 ] "m6" 0.078
  , tpl Maj [ Add 6, Add 9 ] "6/9" 0.058
  , tpl Min [ Add 9 ] "m(add9)" 0.052
  , tpl Min [ Add 6, Add 9 ] "m6/9" 0.038
  , tpl Min [ Add 11 ] "m(add11)" 0.032
  , tpl Maj [ Add 11 ] "add11" 0.024
  , tpl Maj [ Sharp 11 ] "add#11" 0.018
  -- ninths and beyond
  , tpl Maj7 [ Add 9 ] "maj9" 0.110
  , tpl Min7 [ Add 9 ] "m9" 0.110
  , tpl Dom7 [ Add 9 ] "9" 0.105
  , tpl Min7 [ Add 9, Add 11 ] "m11" 0.058
  , tpl Dom7 [ Add 9, Add 13 ] "13" 0.052
  , tpl Maj7 [ Add 9, Sharp 11 ] "maj9#11" 0.024
  , tpl HalfDim [ Add 9 ] "m9b5" 0.018
  -- altered dominants
  , tpl Dom7 [ Flat 9 ] "7b9" 0.044
  , tpl Dom7 [ Sharp 9 ] "7#9" 0.038
  , tpl Dom7 [ Sharp 11 ] "7#11" 0.030
  , tpl Dom7 [ Flat 13 ] "7b13" 0.030
  -- minor colours (rare, but the McMullen table wants them)
  , tpl Min [ Flat 9 ] "m(b9)" 0.008
  , tpl Min [ Flat 13 ] "m(b13)" 0.008
  -- the dyad, last and cheapest: it explains almost nothing, so it wins only
  -- when there is almost nothing to explain
  , tpl Maj [ NoThird ] "5" 0.016
  ]

-- ---------------------------------------------------------------------------
-- Weights — what a tone is worth, and what an intruder costs
-- ---------------------------------------------------------------------------

-- | What a template tone is worth, by its interval from the root and the
-- | quality it sits in. The ordering is the claim: root and third IDENTIFY a
-- | chord, the seventh COLOURS it, and the perfect fifth is nearly free —
-- | which is precisely the note a player drops and a pitch estimator misses.
-- | A diminished fifth or an augmented fifth is not free, because there the
-- | altered fifth *is* the chord's identity.
intervalWeight :: Quality -> Int -> Number
intervalWeight q = case _ of
  0 -> 3.0 -- root
  3 -> 2.5 -- minor third
  4 -> 2.5 -- major third
  6 -> if diminishedQuality q then 2.0 else 1.2 -- ♭5 vs ♯11
  7 -> 1.0 -- perfect fifth: the first thing anyone drops
  8 -> if augmentedQuality q then 2.0 else 1.2 -- ♯5 vs ♭13
  9 -> if q == FullyDim then 2.0 else 1.5 -- °7 vs 6th/13th
  10 -> 1.8 -- minor seventh
  11 -> 1.8 -- major seventh
  _ -> 1.2 -- ♭9, 9, 11 — colour

diminishedQuality :: Quality -> Boolean
diminishedQuality q = q `elem` [ Dim, HalfDim, FullyDim ]

augmentedQuality :: Quality -> Boolean
augmentedQuality q = q `elem` [ Aug, AugMaj7 ]

-- | What an unexplained observed note costs, by its interval from the candidate
-- | root. Absolute, not normalised by template size, so one intruder costs every
-- | reading the same — that is what stops a small template winning by ignoring
-- | notes. The ordering is the claim: a 9th, 11th or 13th over a chord is nearly
-- | free (players add them, and harmonics land there), a ♭9 or ♭13 is a real
-- | colour choice, and a contradicting THIRD or SEVENTH is expensive because it
-- | names a different chord.
extraCost :: Int -> Number
extraCost = case _ of
  0 -> 0.00 -- unreachable: every template contains its root
  1 -> 0.40 -- ♭9
  2 -> 0.15 -- 9
  3 -> 0.45 -- a second third
  4 -> 0.45 -- a second third
  5 -> 0.18 -- 11
  6 -> 0.35 -- ♯11 / ♭5
  7 -> 0.20 -- fifth
  8 -> 0.35 -- ♭13 / ♯5
  9 -> 0.15 -- 6th / 13
  10 -> 0.32 -- an unexpected ♭7
  11 -> 0.35 -- an unexpected maj7
  _ -> 0.30

-- | How much of a missing tone's weight is charged as a penalty. Below 1.0
-- | because absence is weaker evidence than presence: an estimator that failed
-- | to hear the fifth is far commoner than a player who left it out on purpose.
missFactor :: Number
missFactor = 0.7

-- | The bass is the single strongest disambiguator, so root-position pays and a
-- | bass note the chord does not even contain hurts badly.
rootBassBonus :: Number
rootBassBonus = 0.09

inversionCost :: Number
inversionCost = -0.02

foreignBassCost :: Number
foreignBassCost = -0.30

-- | Paid by a reading whose root is diatonic to the supplied key, in
-- | `recogniseInKey` only. Large enough to settle a tie between a diatonic and a
-- | borrowed reading of the same notes, small enough that it cannot rescue a bad
-- | fit.
diatonicRootBonus :: Number
diatonicRootBonus = 0.06

-- ---------------------------------------------------------------------------
-- Candidate — one ranked reading
-- ---------------------------------------------------------------------------

-- | Why a candidate scored what it did — the three sets a UI wants to show:
-- | tones heard, tones the reading expected but did not hear, and notes it
-- | cannot account for. All are pitch classes.
type Fit =
  { matched :: Array Int
  , missing :: Array Int
  , extra :: Array Int
  }

-- | One reading of the observation: a root, a chord type, an optional slash
-- | bass, what it scored and why.
newtype Candidate = Candidate
  { root :: Int
  , template :: Template
  , bass :: Maybe Int
  , score :: Score
  , fit :: Fit
  }

derive instance eqCandidate :: Eq Candidate

instance showCandidate :: Show Candidate where
  show c = candidateName c <> " " <> show (candidateScore c)

candidateRoot :: Candidate -> Int
candidateRoot (Candidate c) = c.root

candidateScore :: Candidate -> Score
candidateScore (Candidate c) = c.score

candidateFit :: Candidate -> Fit
candidateFit (Candidate c) = c.fit

-- | The pitch classes this reading claims — the template's tones, plus the bass
-- | when the reading is a slash over a note the chord does not contain.
candidateChord :: Candidate -> Chord
candidateChord (Candidate c) =
  let
    Chord tones = templateChord c.root c.template
  in
    case c.bass of
      Nothing -> Chord tones
      Just b -> Chord (if b `elem` tones then tones else normalisePCs (cons b tones))

-- | The ABSOLUTE name, needing no key: root, suffix, and a slash bass when the
-- | bass is not the root — "Fmaj7", "Am9", "G7/B". Roots are spelled with
-- | sharps (see the module header on spelling).
candidateName :: Candidate -> String
candidateName (Candidate c) =
  pitchClassName c.root
    <> templateSuffix c.template
    <> maybe "" (\b -> "/" <> pitchClassName b) c.bass

-- ---------------------------------------------------------------------------
-- Scoring
-- ---------------------------------------------------------------------------

scoreAgainst :: Observation -> Int -> Template -> Candidate
scoreAgainst obs rootPC template@(Template t) =
  let
    Chord tones = templateChord rootPC template

    matched = filter (\pc -> pc `elem` obs.pitches) tones
    missing = filter (\pc -> not (pc `elem` obs.pitches)) tones
    extra = filter (\pc -> not (pc `elem` tones)) obs.pitches

    weightAt pc = intervalWeight t.quality (pmod (pc - rootPC) 12)

    totalWeight = sum (map weightAt tones)
    matchedWeight = sum (map weightAt matched)
    missingWeight = sum (map weightAt missing)
    extraWeight = sum (map (\pc -> extraCost (pmod (pc - rootPC) 12)) extra)

    fitPart =
      if totalWeight <= 0.0 then 0.0
      else (matchedWeight - missFactor * missingWeight) / totalWeight

    bassAdjustment = case obs.bass of
      Nothing -> 0.0
      Just b
        | b == rootPC -> rootBassBonus
        | b `elem` tones -> inversionCost
        | otherwise -> foreignBassCost

    slashBass = case obs.bass of
      Nothing -> Nothing
      Just b
        | b == rootPC -> Nothing
        | otherwise -> Just b
  in
    Candidate
      { root: rootPC
      , template
      , bass: slashBass
      , score: Score (fitPart - extraWeight + t.prior + bassAdjustment)
      , fit: { matched, missing, extra }
      }

candidatesFor :: Array Template -> Observation -> Array Candidate
candidatesFor vocab obs =
  concatMap (\rootPC -> map (scoreAgainst obs rootPC) vocab) (range 0 11)

byScoreDesc :: Candidate -> Candidate -> Ordering
byScoreDesc a b = compare (candidateScore b) (candidateScore a)

-- | Every reading worth showing, best first. Ties are broken by the lower root
-- | and then by table order, so the ranking is deterministic — a UI showing
-- | "the top three" shows the same three every time.
recognise :: Observation -> Array Candidate
recognise = recogniseWith templates

-- | `recognise` against a caller-supplied vocabulary, for a workflow that knows
-- | its material is (say) triads only, or that wants a house chord type the
-- | table does not carry.
recogniseWith :: Array Template -> Observation -> Array Candidate
recogniseWith vocab obs
  | null obs.pitches = []
  | otherwise =
      filter (\c -> scoreNumber (candidateScore c) >= scoreFloor)
        (sortBy byScoreDesc (candidatesFor vocab obs))

recogniseTop :: Int -> Observation -> Array Candidate
recogniseTop n = take n <<< recognise

best :: Observation -> Maybe Candidate
best = head <<< recognise

-- ---------------------------------------------------------------------------
-- Reading against a key — candidates as DegreeChords
-- ---------------------------------------------------------------------------

-- | A reading expressed in the library's own vocabulary: an actual
-- | `DegreeChord`, so the result composes with `realize`, `Harmonia.Voicing`
-- | and `Harmonia.Anchor` instead of dead-ending in a string.
newtype KeyedCandidate = KeyedCandidate
  { chord :: DegreeChord
  , score :: Score
  , candidate :: Candidate
  }

instance showKeyedCandidate :: Show KeyedCandidate where
  show (KeyedCandidate k) = show k.candidate <> " = " <> show k.chord

keyedChord :: KeyedCandidate -> DegreeChord
keyedChord (KeyedCandidate k) = k.chord

keyedScore :: KeyedCandidate -> Score
keyedScore (KeyedCandidate k) = k.score

-- | Express a pitch class as a scale degree of a key — directly if the key's own
-- | mode contains it, otherwise by borrowing from a parallel mode. The borrow
-- | order is a preference, not a search: Aeolian first because ♭III, ♭VI and ♭VII
-- | are how borrowed roots usually arrive. Every pitch class is reachable, so
-- | this is total in practice; the `Maybe` covers a `Custom` mode with holes.
degreeFor :: Key -> Int -> Maybe { numeral :: Numeral, borrowed :: Maybe Mode }
degreeFor key pc = case numeralIn key.mode key.tonic pc of
  Just n -> Just { numeral: n, borrowed: Nothing }
  Nothing ->
    findMap
      (\m -> map (\n -> { numeral: n, borrowed: Just m }) (numeralIn m key.tonic pc))
      borrowOrder

borrowOrder :: Array Mode
borrowOrder =
  [ Aeolian, Ionian, Dorian, Mixolydian, Lydian, Phrygian
  , HarmonicMinor, MelodicMinor, Locrian, Altered
  ]

numeralIn :: Mode -> Int -> Int -> Maybe Numeral
numeralIn m tonic pc =
  find (\n -> pmod (tonic + degreeOffset m n) 12 == pc) allNumerals

allNumerals :: Array Numeral
allNumerals = [ I, II, III, IV, V, VI, VII ]

degreeOffset :: Mode -> Numeral -> Int
degreeOffset m n = fromMaybe 0 (modeIntervals m !! numeralIndex n)

-- | Lift one absolute reading into the key. Fails only when the root has no
-- | numeral at all in any borrowable mode.
keyedFrom :: Key -> Candidate -> Maybe KeyedCandidate
keyedFrom key candidate@(Candidate c) = do
  rootDegree <- degreeFor key c.root
  let
    Template t = c.template
    effectiveMode = fromMaybe key.mode rootDegree.borrowed

    plain = deg rootDegree.numeral t.quality t.tensions
    borrowed = maybe plain (\m -> borrow m plain) rootDegree.borrowed

    -- A slash is attachable only when the bass is itself a degree of the mode
    -- the chord is read in — `realize` looks the slash numeral up in the
    -- EFFECTIVE mode, so anything else would not round-trip. When it is not,
    -- the reading keeps its absolute name (which still shows the slash) and
    -- loses the slash from the recipe.
    withSlash = case c.bass of
      Nothing -> borrowed
      Just b -> maybe borrowed (slashed borrowed) (numeralIn effectiveMode key.tonic b)

    bonus = if isJust rootDegree.borrowed then 0.0 else diatonicRootBonus
  pure (KeyedCandidate { chord: withSlash, score: Score (scoreNumber c.score + bonus), candidate })

-- | Every reading worth showing, as `DegreeChord`s in the given key, best first.
-- | The key re-ranks: a root that is diatonic to it is paid `diatonicRootBonus`,
-- | which is usually enough to prefer "V7 in C" over its tritone twin.
recogniseInKey :: Key -> Observation -> Array KeyedCandidate
recogniseInKey key obs
  | null obs.pitches = []
  | otherwise =
      filter (\k -> scoreNumber (keyedScore k) >= scoreFloor)
        (sortBy byKeyedScoreDesc (catMaybes (map (keyedFrom key) (candidatesFor templates obs))))

byKeyedScoreDesc :: KeyedCandidate -> KeyedCandidate -> Ordering
byKeyedScoreDesc a b = compare (keyedScore b) (keyedScore a)

recogniseInKeyTop :: Int -> Key -> Observation -> Array KeyedCandidate
recogniseInKeyTop n key = take n <<< recogniseInKey key

bestInKey :: Key -> Observation -> Maybe KeyedCandidate
bestInKey key = head <<< recogniseInKey key

-- ---------------------------------------------------------------------------
-- Spelling
-- ---------------------------------------------------------------------------

-- | Pitch class to letter. Sharps throughout — harmonia has no spelled-pitch
-- | type, so there is no principled way to choose ♭ here, only a convention.
pitchClassName :: Int -> String
pitchClassName pc = fromMaybe "?" (pitchClassNames !! pmod pc 12)

pitchClassNames :: Array String
pitchClassNames =
  [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | Positive modulo, backend-stable (the same reason `Harmonia.PitchSet` has
-- | its own).
pmod :: Int -> Int -> Int
pmod a m = ((a `mod` m) + m) `mod` m
