-- | Tests for `Harmonia.Recognise` — pitch classes back to chord names.
-- |
-- | The central property, and the reason recognition belongs in Harmonia at
-- | all, is the ROUND TRIP against `realize`: take every `DegreeChord` in a
-- | decent vocabulary, realize it, hand the pitch classes back, and look for the
-- | original among the readings. "The original" can only mean *the same root
-- | realizing to the same pitch classes* — a pitch-class set carries no record
-- | of whether its 11th was spelled `Add 4` or `Add 11`, or of a `borrow` that
-- | did not move the root, so demanding `DegreeChord` equality would be
-- | demanding something recognition cannot in principle deliver.
-- |
-- | What is REQUIRED and what is merely RECORDED:
-- |
-- |   * REQUIRED — with the bass known, every chord in the corpus ranks FIRST.
-- |     That is the real use (an onset detector knows the lowest note), and it
-- |     is the assertion that breaks if the scoring drifts.
-- |   * REQUIRED — with no bass at all, every chord is still PRESENT. Nothing
-- |     ever falls out of the ranking.
-- |   * RECORDED — which chords do not rank first without a bass, and what beat
-- |     them. Every one is a genuine ambiguity (a 6th chord *is* its relative
-- |     minor 7th; a °7 has four equal roots), and the golden list below names
-- |     them all. It is not a bug list: contorting the scoring to empty it would
-- |     be lying about the notes.
-- |
-- | The noise suite is the other half, because noise is the actual input:
-- | dropping the fifth, adding one foreign pitch class, adding two. Those counts
-- | are goldens too — a model change has to re-record them deliberately.
module Test.RecogniseSpec
  ( runRecogniseTests
  ) where

import Prelude

import Data.Array (concatMap, cons, elem, filter, findIndex, index, length, null, range, take, zipWith)
import Data.Foldable (for_, maximum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual')

import Harmonia.Chord
  ( Chord(..)
  , DegreeChord
  , Key
  , Mode(..)
  , Numeral(..)
  , Quality(..)
  , Tension(..)
  , aMinorKey
  , cMajorKey
  , chordBass
  , chordRoot
  , deg
  , mcmullenYellow
  , mcmullenYellowNames
  , realize
  , slashed
  )
import Harmonia.Recognise
  ( Observation
  , best
  , bestInKey
  , candidateName
  , keyedChord
  , observe
  , observeWithBass
  , recognise
  , recogniseInKey
  )

runRecogniseTests :: Effect Unit
runRecogniseTests = do
  log ""
  log "--- Harmonia.Recognise — absolute naming ---"

  expectName "C E G           → C" "C" (observe [ 0, 4, 7 ])
  expectName "C E G B         → Cmaj7" "Cmaj7" (observe [ 0, 4, 7, 11 ])
  expectName "G B D F         → G7" "G7" (observe [ 7, 11, 2, 5 ])
  expectName "D F A C         → Dm7" "Dm7" (observe [ 2, 5, 9, 0 ])
  expectName "B D F           → Bdim" "Bdim" (observe [ 11, 2, 5 ])
  expectName "G C D F         → G7sus4" "G7sus4" (observe [ 7, 0, 2, 5 ])
  expectName "C E G B D       → Cmaj9" "Cmaj9" (observe [ 0, 4, 7, 11, 2 ])

  log ""
  log "--- Harmonia.Recognise — ambiguity, and what the bass settles ---"

  -- {9,0,4,7} is Am7 and C6, exactly and equally. With no bass only the prior
  -- separates them (a m7 is commoner than a 6th chord) and BOTH stay in the
  -- ranking; the bass decides it properly.
  expectName "A C E G, no bass → Am7 (on the prior alone)" "Am7" (observe [ 9, 0, 4, 7 ])
  expectName "A C E G, bass A  → Am7" "Am7" (observeWithBass 9 [ 9, 0, 4, 7 ])
  expectName "A C E G, bass C  → C6" "C6" (observeWithBass 0 [ 9, 0, 4, 7 ])
  assertEqual' "  the losing reading survives either way"
    { actual:
        { c6WithNoBass: elem "C6" (names (observe [ 9, 0, 4, 7 ]))
        , am7OverC: elem "Am7/C" (names (observeWithBass 0 [ 9, 0, 4, 7 ]))
        }
    , expected: { c6WithNoBass: true, am7OverC: true }
    }

  -- The same twinning one rung down: a sus2 is a sus4 a fifth up, and a m6 is
  -- its half-diminished seventh a minor third below.
  expectName "C D G,    bass C → Csus2" "Csus2" (observeWithBass 0 [ 0, 2, 7 ])
  expectName "C D G,    bass G → Gsus4" "Gsus4" (observeWithBass 7 [ 0, 2, 7 ])
  expectName "F G# C D, bass F → Fm6" "Fm6" (observeWithBass 5 [ 5, 8, 0, 2 ])
  expectName "F G# C D, bass D → Dm7b5" "Dm7b5" (observeWithBass 2 [ 5, 8, 0, 2 ])

  -- An inversion is named as a slash, and the bass is exactly what tells it
  -- apart from the reading that would claim that bass note as ITS root.
  expectName "C E G B, bass E  → Cmaj7/E" "Cmaj7/E" (observeWithBass 4 [ 0, 4, 7, 11 ])
  expectName "C E G,   bass G  → C/G" "C/G" (observeWithBass 7 [ 0, 4, 7 ])

  log ""
  log "--- Harmonia.Recognise — reading against a key ---"

  -- In a key the answer is a DegreeChord, not a string, so it composes with the
  -- rest of the library instead of dead-ending.
  assertEqual' "  G B D F in C major → V7"
    { actual: map keyedChord (bestInKey cMajorKey (observeWithBass 7 [ 7, 11, 2, 5 ]))
    , expected: Just (deg V Dom7 [])
    }
  assertEqual' "  D F A C in C major → ii7"
    { actual: map keyedChord (bestInKey cMajorKey (observeWithBass 2 [ 2, 5, 9, 0 ]))
    , expected: Just (deg II Min7 [])
    }

  -- The key genuinely changes the answer. {0,3,7,8} read with no key is
  -- G#maj7 — a major seventh outranks an exotic minor-♭13 — but in C major the
  -- root C is diatonic and G# is not, and it flips to McMullen's i♭13.
  expectName "C D# G G#, no key → G#maj7" "G#maj7" (observe [ 0, 3, 7, 8 ])
  assertEqual' "  C D# G G# in C major → root C, not G#"
    { actual: map (chordRoot cMajorKey <<< keyedChord)
        (bestInKey cMajorKey (observeWithBass 0 [ 0, 3, 7, 8 ]))
    , expected: Just 0
    }

  log ""
  log "--- Harmonia.Recognise — round trip against realize ---"
  log ("  corpus: " <> show (length corpus) <> " DegreeChords — McMullen Yellow,")
  log "  plus common triads/sevenths/extensions across five keys and modes,"
  log "  plus inversions."

  let withBass = summarise true
      noBass = summarise false

  log ("  WITH bass:    first " <> show withBass.first <> ", present-not-first "
    <> show withBass.notFirst <> ", absent " <> show withBass.absent)
  log ("  WITHOUT bass: first " <> show noBass.first <> ", present-not-first "
    <> show noBass.notFirst <> ", absent " <> show noBass.absent)

  -- REQUIRED: the bass is what the real workflow supplies, and with it the
  -- recogniser is exact over the whole corpus.
  assertEqual' "  with a known bass, every corpus chord ranks FIRST"
    { actual: { first: withBass.first, absent: withBass.absent }
    , expected: { first: length corpus, absent: 0 }
    }

  -- REQUIRED: without a bass, nothing is ever LOST — only reordered.
  assertEqual' "  without a bass, every corpus chord is still present"
    { actual: noBass.absent, expected: 0 }

  -- RECORDED: which ones lose the top slot, and to what.
  log "  without a bass these are outranked, each by an equally good reading:"
  for_ (notFirstLabels false) \l -> log ("    " <> l)
  assertEqual' "  the not-first list is exactly the known ambiguities"
    { actual: notFirstLabels false
    , expected:
        [ "McMullen ♭VII6 → Gm7"
        , "McMullen i♭13 → G#maj7"
        , "McMullen iv6 → Dm7b5"
        , "C-ion I6 → Am7"
        , "C-ion vidim7 → Cdim7"
        , "A-aeo vidim7 → Ddim7"
        , "F-lyd I6 → Dm7"
        , "D#-ion I6 → Cm7"
        , "D-dor I6 → Bm7"
        , "D-dor vidim7 → Ddim7"
        ]
    }

  log ""
  log "--- Harmonia.Recognise — noise tolerance ---"

  -- A DROPPED FIFTH is the commonest miss, from players and estimators alike,
  -- and it is the cheapest tone to lose (see `intervalWeight`).
  expectName "  Cmaj7 less its fifth → Cmaj7" "Cmaj7" (observe [ 0, 4, 11 ])
  expectName "  Dm7 less its fifth   → Dm7" "Dm7" (observe [ 2, 5, 0 ])
  expectName "  G7 less its fifth    → G7" "G7" (observe [ 7, 11, 5 ])
  expectName "  C less its fifth     → C" "C" (observe [ 0, 4 ])

  -- ONE FOREIGN PITCH CLASS, every way round, on four base chords.
  let one = extraSummary 1
  log ("  one foreign pc (" <> show one.probes <> " probes): still present "
    <> show one.present <> ", still first " <> show one.first
    <> ", worst rank " <> show one.worst)
  assertEqual' "  one foreign pc never loses the true chord"
    { actual: { present: one.present, probes: one.probes }
    , expected: { present: 33, probes: 33 }
    }
  assertEqual' "  ...and the true chord still ranks first in 7 of the 33"
    { actual: { first: one.first, worst: one.worst }
    , expected: { first: 7, worst: 7 }
    }
  log "  it is demoted only when the intruder COMPLETES a chord the table knows"
  log "  — Am7 + F is Fmaj9, G7 + G# is G7♭9 — which is the right answer, not a miss:"
  expectName "  Cmaj7 + A# → Cmaj7 (an intruder nothing explains)"
    "Cmaj7" (observe [ 0, 4, 7, 11, 10 ])
  expectName "  Cmaj7 + D  → Cmaj9 (an intruder that IS the chord)"
    "Cmaj9" (observe [ 0, 4, 7, 11, 2 ])
  expectName "  Am7 + F    → Fmaj9" "Fmaj9" (observe [ 9, 0, 4, 7, 5 ])

  -- TWO FOREIGN PITCH CLASSES. Six of twelve pitch classes is half the
  -- chromatic scale, and by then the observation has stopped being one chord.
  let two = extraSummary 2
  log ("  two foreign pcs (" <> show two.probes <> " probes): still present "
    <> show two.present <> ", still first " <> show two.first
    <> ", worst rank " <> show two.worst)
  assertEqual' "  two foreign pcs: the true chord survives most, but not all"
    { actual: { present: two.present, probes: two.probes, first: two.first }
    , expected: { present: 49, probes: 56, first: 2 }
    }
  log "  the seven losses are all cases where the six notes read cleanly as some"
  log "  OTHER chord: the recogniser is not wrong, the input is no longer a chord."
  log ""

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

names :: Observation -> Array String
names = map candidateName <<< recognise

expectName :: String -> String -> Observation -> Effect Unit
expectName label expected obs =
  assertEqual' label
    { actual: maybe "(nothing)" candidateName (best obs), expected }

-- ---------------------------------------------------------------------------
-- Round-trip corpus
-- ---------------------------------------------------------------------------

type Case = { label :: String, key :: Key, chord :: DegreeChord }

corpus :: Array Case
corpus = mcmullenCases <> generalCases <> slashCases

mcmullenCases :: Array Case
mcmullenCases =
  zipWith
    (\name dc -> { label: "McMullen " <> name, key: cMajorKey, chord: dc })
    mcmullenYellowNames
    mcmullenYellow

generalKeys :: Array { name :: String, key :: Key }
generalKeys =
  [ { name: "C-ion", key: cMajorKey }
  , { name: "A-aeo", key: aMinorKey }
  , { name: "F-lyd", key: { tonic: 5, mode: Lydian } }
  , { name: "D#-ion", key: { tonic: 3, mode: Ionian } }
  , { name: "D-dor", key: { tonic: 2, mode: Dorian } }
  ]

generalChords :: Array { name :: String, chord :: DegreeChord }
generalChords =
  [ { name: "I", chord: deg I Maj [] }
  , { name: "ii7", chord: deg II Min7 [] }
  , { name: "iii", chord: deg III Min [] }
  , { name: "IVmaj7", chord: deg IV Maj7 [] }
  , { name: "V7", chord: deg V Dom7 [] }
  , { name: "vi7", chord: deg VI Min7 [] }
  , { name: "viiø", chord: deg VII HalfDim [] }
  , { name: "Imaj7", chord: deg I Maj7 [] }
  , { name: "V7sus4", chord: deg V Dom7 [ Sus 4 ] }
  , { name: "ii9", chord: deg II Min7 [ Add 9 ] }
  , { name: "Imaj9", chord: deg I Maj7 [ Add 9 ] }
  , { name: "V13", chord: deg V Dom7 [ Add 9, Add 13 ] }
  , { name: "Iadd9", chord: deg I Maj [ Add 9 ] }
  , { name: "I6", chord: deg I Maj [ Add 6 ] }
  , { name: "vidim7", chord: deg VI FullyDim [] }
  , { name: "V7b9", chord: deg V Dom7 [ Flat 9 ] }
  -- `I/v` is the only slash safe in every mode above (all five have a perfect
  -- fifth); mode-specific inversions are in `slashCases`.
  , { name: "I/v", chord: slashed (deg I Maj []) V }
  ]

generalCases :: Array Case
generalCases =
  concatMap
    ( \k -> map
        (\c -> { label: k.name <> " " <> c.name, key: k.key, chord: c.chord })
        generalChords
    )
    generalKeys

-- | Inversions, pinned to modes where the slash degree really is a chord tone.
-- | (`slashed (deg I Maj []) III` in AEOLIAN is not an inversion at all — a
-- | major I over a minor III is an A7♯9 with the seventh missing — so it is not
-- | here. That is a fact about the recipe, not about recognition.)
slashCases :: Array Case
slashCases =
  [ { label: "C-ion I/iii", key: cMajorKey, chord: slashed (deg I Maj []) III }
  , { label: "C-ion V7/vii", key: cMajorKey, chord: slashed (deg V Dom7 []) VII }
  , { label: "C-ion IVmaj7/vi", key: cMajorKey, chord: slashed (deg IV Maj7 []) VI }
  , { label: "A-aeo i/III", key: aMinorKey, chord: slashed (deg I Min []) III }
  , { label: "A-aeo iv/VI", key: aMinorKey, chord: slashed (deg IV Min []) VI }
  ]

-- | A candidate RECOVERS the original when it has the same root and realizes to
-- | the same pitch classes. Anything stronger would ask recognition to recover
-- | text the pitch classes never carried (see the module header).
recovers :: Key -> DegreeChord -> DegreeChord -> Boolean
recovers key original found =
  chordRoot key found == chordRoot key original
    && realize key found == realize key original

observationOf :: Boolean -> Case -> Observation
observationOf withBass c =
  let
    Chord pcs = realize c.key c.chord
  in
    if withBass then observeWithBass (chordBass c.key c.chord) pcs else observe pcs

type Outcome = { label :: String, present :: Boolean, first :: Boolean, winner :: String }

outcomes :: Boolean -> Array Outcome
outcomes withBass = map judge corpus
  where
  judge c =
    let
      found = map keyedChord (recogniseInKey c.key (observationOf withBass c))
    in
      { label: c.label
      , present: not (null (filter (recovers c.key c.chord) found))
      , first: maybe false (recovers c.key c.chord) (index found 0)
      , winner: maybe "(nothing)" candidateName (best (observationOf withBass c))
      }

summarise :: Boolean -> { first :: Int, notFirst :: Int, absent :: Int }
summarise withBass =
  let
    os = outcomes withBass
  in
    { first: length (filter _.first os)
    , notFirst: length (filter (\o -> o.present && not o.first) os)
    , absent: length (filter (not <<< _.present) os)
    }

-- | "the corpus chord → the reading that outranked it" — the ambiguity, named.
notFirstLabels :: Boolean -> Array String
notFirstLabels withBass =
  map (\o -> o.label <> " → " <> o.winner)
    (filter (\o -> o.present && not o.first) (outcomes withBass))

-- ---------------------------------------------------------------------------
-- Noise
-- ---------------------------------------------------------------------------

noiseBases :: Array { name :: String, pcs :: Array Int }
noiseBases =
  [ { name: "Cmaj7", pcs: [ 0, 4, 7, 11 ] }
  , { name: "G7", pcs: [ 7, 11, 2, 5 ] }
  , { name: "C", pcs: [ 0, 4, 7 ] }
  , { name: "Am7", pcs: [ 9, 0, 4, 7 ] }
  ]

-- | Rank (1-based) of a named reading; 0 when it is not returned at all.
rankOf :: String -> Observation -> Int
rankOf want obs =
  fromMaybe 0 (map (_ + 1) (findIndex (\c -> candidateName c == want) (recognise obs)))

-- | Add `n` foreign pitch classes to each base chord, every way round, and
-- | record where the true chord lands. `n == 2` uses `Cmaj7` and `G7` only —
-- | six of twelve pitch classes is enough to make the point.
extraSummary :: Int -> { probes :: Int, present :: Int, first :: Int, worst :: Int }
extraSummary n =
  let
    bases = if n == 1 then noiseBases else take 2 noiseBases
    ranks = concatMap (\b -> map (rankOf b.name <<< observe) (noisy n b.pcs)) bases
  in
    { probes: length ranks
    , present: length (filter (_ > 0) ranks)
    , first: length (filter (_ == 1) ranks)
    , worst: fromMaybe 0 (maximum ranks)
    }

noisy :: Int -> Array Int -> Array (Array Int)
noisy n pcs =
  let
    outside = filter (\x -> not (elem x pcs)) (range 0 11)
  in
    if n == 1 then map (\x -> cons x pcs) outside
    else concatMap (\a -> map (\b -> [ a, b ] <> pcs) (filter (_ > a) outside)) outside
