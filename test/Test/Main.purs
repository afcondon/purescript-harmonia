-- | Test entry point. Runs the Chord (recipe) and Voicing (voice-leading)
-- | golden suites. Each helper throws via `Test.Assert` on mismatch, so a
-- | failure exits non-zero — `spago test` fails CI.
module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.ChordSpec (runChordTests)
import Test.VoicingSpec (runVoicingTests)

main :: Effect Unit
main = do
  runChordTests
  runVoicingTests
  log "\nAll Harmonia golden tests passed."
