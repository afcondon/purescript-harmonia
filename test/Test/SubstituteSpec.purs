-- | Goldens for `Harmonia.Substitute`: chords that could stand in for a chord.
module Test.SubstituteSpec
  ( runSubstituteTests
  ) where

import Prelude

import Data.Array (any, filter, head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Substitute (Reason(..), substitutes)
import Test.Assert (assert', assertEqual)

cMajor :: Array Int
cMajor = [ 0, 2, 4, 5, 7, 9, 11 ]

runSubstituteTests :: Effect Unit
runSubstituteTests = do
  log ""
  log "--- Harmonia — substitutes (golden) ---"

  -- G7: its tritone substitute is D♭7, first
  let g7 = substitutes 8 cMajor 7 [ 7, 11, 2, 5 ]
  assertEqual { actual: head g7 <#> \s -> { root: s.root, suffix: s.suffix }, expected: Just { root: 1, suffix: "7" } }
  assert' "G7: Bø7 shares three notes" (any (\s -> s.root == 11 && s.suffix == "ø7" && s.reason == Shares 3) g7)
  assert' "G7: G7sus4 on the same root" (any (\s -> s.root == 7 && s.suffix == "7sus4" && s.reason == SameRoot) g7)

  -- C: no tritone (not a dominant); Am and Em share two notes and stay in
  -- the key, so they lead that section; Cm is the same root
  let c = substitutes 8 cMajor 0 [ 0, 4, 7 ]
  assert' "C: no tritone section" (not (any (\s -> s.reason == Tritone) c))
  let twos = filter (\s -> s.reason == Shares 2) c
  assert' "C: Am among the two-in-common, in key" (any (\s -> s.root == 9 && s.suffix == "m" && s.outside == 0) twos)
  assert' "C: Em among the two-in-common" (any (\s -> s.root == 4 && s.suffix == "m") twos)
  assert' "C: Cm on the same root" (any (\s -> s.root == 0 && s.suffix == "m" && s.reason == SameRoot) c)

  -- Cmaj7: Am7 and Em7 each keep three of its notes, in the key
  let cM7 = filter (\s -> s.reason == Shares 3) (substitutes 8 cMajor 0 [ 0, 4, 7, 11 ])
  assert' "Cmaj7: Am7 keeps three" (any (\s -> s.root == 9 && s.suffix == "m7" && s.outside == 0) cM7)
  assert' "Cmaj7: Em7 keeps three" (any (\s -> s.root == 4 && s.suffix == "m7" && s.outside == 0) cM7)
  log "substitute goldens: ok"
