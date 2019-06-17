{-# LANGUAGE TemplateHaskell #-}
module Data.LineWrap.BruteForce (bruteForce) where

import Control.Lens (snoc, (.=), use, makeClassy, (+=))

import Data.Function
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (subsequences, nub, sort, drop, take,sortBy)

import Control.Monad.State
import Data.LineWrap.Types

-- All credit goes to 'Xxyxyz' and their thorough blog post:
-- https://xxyxyz.org/line-breaking/
--
-- Brute force
--
-- An immediate approach is to try to search trough all the possible
-- break configurations and return the best one. Since any two
-- consecutive words might get split up by a break, there are order of
-- O(2 ^ n) alternatives. Consequently, it is not feasible to form a
-- paragraph of more than 30 - 40 words.
--
-- from itertools import combinations, chain
--
-- def powerset(iterable):
--     s = list(iterable)
--     return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))
--
-- def naive(text, width):
--     words = text.split()
--     count = len(words)
--
--     minimum = 10 ** 20
--     breaks = ()
--     for b in powerset(range(1, count)):
--         m = 0
--         i = 0
--         for j in chain(b, (count,)):
--             w = len(' '.join(words[i:j]))
--             if w > width:
--                 break
--             m += (width - w) ** 2
--             i = j
--         else:
--             if m < minimum:
--                 minimum = m
--                 breaks = b
--
--     lines = []
--     i = 0
--     for j in chain(breaks, (count,)):
--         lines.append(' '.join(words[i:j]))
--         i = j
--     return lines

powerset :: (Ord a, Eq a) => [a] -> [[a]]
powerset = sortBy (compare `on` length) . sort . nub . subsequences

data Loop = Loop
  { _loopM :: Int
  , _loopI :: Int
  , _loopMinimum :: Int
  , _loopBreaks :: [Int]
  }
  deriving Show
makeClassy ''Loop

-- Brute force
--
-- An immediate approach is to try to search trough all the possible
-- break configurations and return the best one. Since any two
-- consecutive words might get split up by a break, there are order of
-- O(2 ^ n) alternatives. Consequently, it is not feasible to form a
-- paragraph of more than 30 - 40 words.
bruteForce :: Text -> Width -> [Text]
bruteForce t (Width width) =
  let
    -- words = text.split()
    ts = T.words t
    -- count = len(words)
    count = length ts

    l = flip execState (Loop 0 0 (10 ^ (20::Int)) mempty) $
      forM_ (powerset [0..count - 1]) $ \b -> do
        loopM .= 0
        loopI .= 0

        r <- froldM (snoc b count) (Just ()) $ \x j -> case x of
          Nothing -> pure x
          Just _ -> do
            i <- use loopI
            let w = T.length . T.unwords . take (j - i) $ drop i ts
            if w > width then pure Nothing --- errr, break? How do people code like this??
              else do
                loopM += (width - w) ^ (2::Int)
                loopI .= j
                pure $ Just ()

        m0 <- use loopM
        minimum0 <- use loopMinimum

        when (isJust r && m0 < minimum0) $ do
          loopMinimum .= m0
          loopBreaks .= b
  in
    snd . frold (snoc (_loopBreaks l) count) (0, mempty) $ \(i,ls) j ->
      let line = T.unwords . take (j - i) $ drop i ts
      in (j, ls `snoc` line)
      
