-- | Tests for `Harmonia.Progression` — the module that joins the other four.
-- |
-- | It holds no musical rule of its own, so these are seam tests rather than
-- | golden ones: that the spec really does determine the output, that the
-- | chords it yields are the ones `Walk` yields, that voicing happened over the
-- | sequence rather than per chord, and that `OpenVoicing`'s bass-is-root
-- | constraint survives the join.
module Test.ProgressionSpec
  ( runProgressionTests
  ) where

import Prelude

import Data.Array (all, drop, head, last, length, zipWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Harmonia.Freedom (freedom)
import Harmonia.OpenVoicing (defaults) as OV
import Harmonia.Palette (Level(..))
import Harmonia.Progression (nameOf, noteName, notes, progression, rooted, spec)
import Harmonia.Voicing (voicingMidi)
import Harmonia.Walk (seed, walk)
import Test.Assert (assertEqual', assertTrue')

cMajor :: { tonic :: Int, minor :: Boolean }
cMajor = { tonic: 0, minor: false }

runProgressionTests :: Effect Unit
runProgressionTests = do
  log "\n— Progression —"

  -- The spec determines the output: same spec, same chords, every time.
  let s = spec cMajor (freedom 2) Medium false (seed 4242) 12
      a = progression OV.defaults s
      b = progression OV.defaults s
  assertEqual' "a spec regenerates its progression exactly"
    { actual: map _.name a, expected: map _.name b }
  assertEqual' "the spec's length is honoured"
    { actual: length a, expected: 12 }

  -- A different seed is a different progression (same settings).
  let c = progression OV.defaults (s { seed = seed 99 })
  assertTrue' "a different seed gives a different walk"
    (map _.name a /= map _.name c)

  -- The chords are Walk's chords — the join adds voicing, not choices.
  let w = walk s.setting s.seed s.length
  assertEqual' "the chords are exactly the walk's"
    { actual: map _.name a, expected: map nameOf w }
  assertEqual' "each chord's content is its address realised"
    { actual: map _.chord a, expected: map (_.chord <<< rooted) w }

  -- OpenVoicing's one constraint has to survive the join: the lowest note of
  -- every chord is its root.
  assertTrue' "the bass of every chord is its root"
    (all (\v -> map (_ `mod` 12) (head (notes v)) == Just (mod v.root 12)) a)

  -- Voicing was done over the SEQUENCE: consecutive top notes move a little,
  -- which is the whole point of `playOpen` over `map openVoicing`. Sixteen
  -- semitones would be a leap; the contour rule keeps it far below that.
  let tops = map (\v -> fromMaybe 0 (last (voicingMidi v.voicing))) a
      hops = zipWith (\x y -> abs (y - x)) tops (drop 1 tops)
  assertTrue' "the top line moves smoothly across the progression"
    (all (_ <= 7) hops)

  -- Naming: the root spelled plus the type's suffix, and nothing invented.
  assertEqual' "note names are plain sharps"
    { actual: map noteName [ 0, 1, 9, 11, 12, -1 ]
    , expected: [ "C", "C#", "A", "B", "C", "B" ]
    }
  assertTrue' "every chord is named"
    (all (\v -> v.name /= "") a)

  -- A walk at Basic in a strict window stays inside the vocabulary it was
  -- given — triads only, so no name carries a seventh or a ninth.
  let basic = progression OV.defaults (spec cMajor (freedom 0) Basic false (seed 7) 8)
  assertTrue' "a Basic walk yields only major and minor triads"
    (all (\v -> v.name == noteName v.root || v.name == noteName v.root <> "m") basic)

  log "  ✓ Progression seams"
  where
  abs n = if n < 0 then negate n else n
