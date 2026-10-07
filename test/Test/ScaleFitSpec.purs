-- | Goldens for `Harmonia.ScaleFit`: the scales that fit a group of chords.
module Test.ScaleFitSpec
  ( runScaleFitTests
  ) where

import Prelude

import Data.Array (filter, head, length, take)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Chord (Mode(..))
import Harmonia.ScaleFit (Fit, Name, fitsFor, reading)
import Test.Assert (assertEqual)

-- | A fit's first name, as "root mode".
firstName :: Fit -> Maybe Name
firstName f = head f.names

runScaleFitTests :: Effect Unit
runScaleFitTests = do
  log ""
  log "--- Harmonia — scales that fit a group of chords (golden) ---"

  -- Dm7 G7 Cmaj7: one complete fit leads, the diatonic set, named on the
  -- first chord's root (D dorian), and nothing it leaves out
  let ii_V_I = [ 50, 53, 57, 60, 55, 59, 62, 65, 48, 52, 55, 59 ]
      best = head (fitsFor 0 [ 2, 7, 0 ] ii_V_I)
  assertEqual { actual: best <#> _.family, expected: Just "major" }
  assertEqual { actual: best >>= firstName, expected: Just { root: 2, mode: "dorian", key: Just Dorian } }
  assertEqual { actual: best <#> _.outside, expected: Just [] }

  -- C major triad and F major triad: no pentatonic holds C E F G A, two
  -- diatonic sets do, and both are named from C, the first chord's root:
  -- C major, then F major's notes as C mixolydian
  let cf = [ 60, 64, 67, 65, 69, 72 ]
      cfFits = fitsFor 0 [ 0, 5 ] cf
  assertEqual { actual: map (\f -> head f.names) (take 2 cfFits)
              , expected: [ Just { root: 0, mode: "major", key: Just Ionian }, Just { root: 0, mode: "mixolydian", key: Just Mixolydian } ] }

  -- a pentatonic leads when it holds everything: C, Am (C E G A) sit in C
  -- major pentatonic, ahead of the seven-note sets
  let pent = head (fitsFor 0 [ 0, 9 ] [ 60, 64, 67, 57, 60, 64 ])
  assertEqual { actual: pent >>= firstName, expected: Just { root: 0, mode: "major pentatonic", key: Nothing } }

  -- C and B♭ (a borrowed ♭VII in C): no C major; named first on C, the set
  -- is C mixolydian
  let cBb = [ 60, 64, 67, 58, 62, 65 ]
      mixo = head (fitsFor 0 [ 0, 10 ] cBb)
  assertEqual { actual: mixo >>= firstName, expected: Just { root: 0, mode: "mixolydian", key: Just Mixolydian } }

  -- E7 → Am: G♯ and G are both needed, so nothing diatonic fits; the two
  -- minors that raise the seventh both hold every note, named from E
  let e7am = [ 52, 56, 59, 62, 57, 60, 64 ]
      complete = filter (\f -> f.outside == []) (fitsFor 1 [ 4, 9 ] e7am)
  assertEqual { actual: map _.family (take 2 complete), expected: [ "melodic minor", "harmonic minor" ] }
  assertEqual { actual: map (\f -> head f.names) (take 2 complete)
              , expected: [ Just { root: 4, mode: "mixolydian ♭6", key: Just MixolydianFlat6 }, Just { root: 4, mode: "phrygian dominant", key: Just PhrygianDominant } ] }

  -- symmetric scales are listed once a distinct set: whole tone has two
  -- and diminished three
  let every = fitsFor 12 [] [ 60 ]
  assertEqual { actual: length (filter (\f -> f.family == "whole tone") every), expected: 2 }
  assertEqual { actual: length (filter (\f -> f.family == "diminished") every), expected: 3 }
  -- the reading: C and B♭ read in C mixolydian, nothing left out; a
  -- pentatonic is never a key, so C and Am read in C major
  assertEqual { actual: reading [ 0, 10 ] cBb <#> \r -> { root: r.root, key: r.key, outside: r.outside }
              , expected: Just { root: 0, key: Mixolydian, outside: [] } }
  assertEqual { actual: reading [ 0, 9 ] [ 60, 64, 67, 57 ] <#> _.key, expected: Just Ionian }
  -- C♯° E°7 D♭ B♭m: F harmonic minor (vii°, vii°7, VI, iv), not the
  -- D♭ lydian ♯2 its first root would name
  assertEqual { actual: reading [ 1, 4, 1, 10 ] [ 49, 52, 55, 52, 55, 58, 61, 49, 53, 56, 58, 61, 65 ] <#> \r -> { root: r.root, key: r.key, outside: r.outside }
              , expected: Just { root: 5, key: HarmonicMinor, outside: [] } }
  log "scale fit goldens: ok"
