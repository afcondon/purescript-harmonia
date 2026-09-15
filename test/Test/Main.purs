-- | Test entry point. Runs the Chord (recipe) and Voicing (voice-leading)
-- | golden suites. Each helper throws via `Test.Assert` on mismatch, so a
-- | failure exits non-zero — `spago test` fails CI.
module Test.Main (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.AnchorSpec (runAnchorTests)
import Test.ChordSpec (runChordTests)
import Test.FreedomSpec (runFreedomTests)
import Test.GaloisSpec (runGaloisTests)
import Test.GradedSpec (runGradedTests)
import Test.OpenVoicingSpec (runOpenVoicingTests, runSpreadTests)
import Test.PaletteSpec (runPaletteTests)
import Test.ProgressionSpec (runProgressionTests)
import Test.PropSpec (runPropTests)
import Test.QuantiseSpec (runQuantiseTests)
import Test.RecogniseSpec (runRecogniseTests)
import Test.VoiceSpec (runVoiceTests)
import Test.WalkSpec (runWalkTests)
import Test.TransformSpec (runTransformTests)
import Test.TrellisSpec (runTrellisTests)
import Test.VarySpec (runVaryTests)
import Test.VoicingSpec (runVoicingTests)

main :: Effect Unit
main = do
  runChordTests
  runVoicingTests
  runAnchorTests
  runGradedTests
  runPropTests
  runQuantiseTests
  runGaloisTests
  runVoiceTests
  runRecogniseTests
  runPaletteTests
  runOpenVoicingTests
  runSpreadTests
  runFreedomTests
  runWalkTests
  runProgressionTests
  runTransformTests
  runVaryTests
  runTrellisTests
  log "\nAll Harmonia golden tests passed."
