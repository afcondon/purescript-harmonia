-- | **Chords that could stand in for a chord.**
-- |
-- | A substitute does the job of the chord it replaces: it keeps enough of
-- | its notes that the harmony still moves the same way, and changes the
-- | rest. The classic reasons, each a section of the answer:
-- |
-- | - **tritone**: a dominant seventh and the dominant seventh a tritone
-- |   away share their third and seventh (swapped), so either resolves the
-- |   same way (G7 → D♭7 before C);
-- | - **same root**: another quality on the same root (C → Cm, Cmaj7 → C7),
-- |   the root and usually the fifth kept;
-- | - **three notes in common**: the chord most like it (Am7 for Cmaj7, Em7
-- |   for Cmaj7, Bø7 for G7);
-- | - **two in common**: the relatives of a triad (Am, Em for C), and the
-- |   further reaches of a seventh.
-- |
-- | Candidates come from the palette (`Harmonia.Palette`, up to `Medium`:
-- | triads, sus, sixths, sevenths, diminished) on every root. Within a
-- | section, the ones with fewest notes outside the scale come first, then
-- | the simpler. Pitch content only: voicing a substitute (near the chord
-- | it replaces) is the consumer's business.
module Harmonia.Substitute
  ( Reason(..)
  , Substitute
  , substitutes
  ) where

import Prelude

import Data.Array (concatMap, filter, length, nub, range, sort, sortBy, take)
import Harmonia.Chord (Chord(..))
import Harmonia.Palette (Level(..), levelIndex, typeLevel, typeOn, typeSuffix, upTo)

data Reason = Tritone | SameRoot | Shares Int

derive instance eqReason :: Eq Reason

-- | A chord that could stand in: its root, its suffix as the palette names
-- | it ("m7", "7sus4"), its pitch classes, why, how many notes it keeps,
-- | and how many fall outside the scale.
type Substitute =
  { root :: Int
  , suffix :: String
  , pcs :: Array Int
  , reason :: Reason
  , shared :: Int
  , outside :: Int
  }

-- | **The substitutes for a chord** (`root`, `pcs`), against a scale (its
-- | pitch classes, for ranking), at most `n` a section: tritone, same root,
-- | three in common, two in common, in that order.
substitutes :: Int -> Array Int -> Int -> Array Int -> Array Substitute
substitutes n scale root0 pcs0 =
  take n tritone <> take n sameRoot <> take n (shares 3) <> take n (shares 2)
  where
  root = pc root0
  want = nub (sort (map pc pcs0))
  types = filter plain (upTo Medium)
  -- the no-fifth shells say nothing a seventh does not
  plain t = typeSuffix t /= "7(no5)" && typeSuffix t /= "m7(no5)"
  cands = concatMap (\r -> map (cand r) types) (range 0 11)
  cand r t =
    let ps = chordPcs (typeOn r t)
        shared = length (filter (\p -> elemOf p want) ps)
    in { root: r, suffix: typeSuffix t, pcs: ps, reason: Shares shared, shared
       , outside: length (filter (\p -> not (elemOf p scale)) ps), level: levelIndex (typeLevel t) }
  different c = c.pcs /= want
  isDominant = elemOf (pc (root + 4)) want && elemOf (pc (root + 10)) want
  tritone =
    if not isDominant then []
    else map (as Tritone) (rank (filter (\c -> c.root == pc (root + 6) && c.suffix == "7") cands))
  sameRoot = map (as SameRoot) (rank (filter (\c -> c.root == root && different c && c.shared >= 2) cands))
  -- by notes in common, excluding what the earlier sections hold
  shares k =
    map (as (Shares k)) (rank (filter (\c -> c.shared == k && c.root /= root && different c
                                          && not (isDominant && c.root == pc (root + 6) && c.suffix == "7")) cands))
  rank = sortBy (\a b -> compare a.outside b.outside <> compare a.level b.level <> compare a.root b.root)
  as reason c = { root: c.root, suffix: c.suffix, pcs: c.pcs, reason, shared: c.shared, outside: c.outside }
  chordPcs (Chord ps) = nub (sort (map pc ps))
  elemOf x xs = length (filter (_ == x) xs) > 0
  pc x = ((x `mod` 12) + 12) `mod` 12
