{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.LineWrap.DynamicProg (dynamicProgramming) where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import Data.Text (Text)
import qualified Data.Text as T

import Data.LineWrap.Types

-- Dynamic programming
--
-- The deficiency of first idea lies in that it repeatedly solves the
-- same subproblems. Yet suppose there was an optimal configuration of
-- lines. Plucking off its last line would still keep the layout
-- optimal because otherwise it would be possible to improve it and,
-- together with the removed line, would result in even better
-- configuration, contradicting its optimality. To solve each
-- subproblem just once, it is then necessary to find out and later
-- re-use which of the lines ending with some word contributes least
-- to the overall cost. As each of the "n" words could terminate at
-- most "n" potential lines, the algorithm runs in O(n ^ 2).
--
-- def dynamic(text, width):
--     words = text.split()
--     count = len(words)
--     slack = [[0] * count for i in range(count)]
--     for i in range(count):
--         slack[i][i] = width - len(words[i])
--         for j in range(i + 1, count):
--             slack[i][j] = slack[i][j - 1] - len(words[j]) - 1
--
--     minima = [0] + [10 ** 20] * count
--     breaks = [0] * count
--     for j in range(count):
--         i = j
--         while i >= 0:
--             if slack[i][j] < 0:
--                 cost = 10 ** 10
--             else:
--                 cost = minima[i] + slack[i][j] ** 2
--             if minima[j + 1] > cost:
--                 minima[j + 1] = cost
--                 breaks[j] = i
--             i -= 1
--
--     lines = []
--     j = count
--     while j > 0:
--         i = breaks[j - 1]
--         lines.append(' '.join(words[i:j]))
--         j = i
--     lines.reverse()
--     return lines

mkSlack :: (a -> Int) -> Width -> [a] -> Int -> ST s [[STRef s Int]]
mkSlack _ _ [] _ = pure []
mkSlack _ _ _  0 = pure []
mkSlack len (Width width) words0 count = do
  -- slack = [[0] * count for i in range(count)]
  x <- replicateM count (replicateM count (newSTRef (0::Int)))
  -- for i in range(count):
  forM_ [0 .. (count - 1)] $ \i -> do
    -- slack[i][i] = width - len(words[i])
    modifySTRef (x !! i !! i) (const $ width - len (words0 !! i))
    -- for j in range(i + 1, count):
    forM_ [(i+1) .. (count - 1)] $ \j -> do
      slj1 <- readSTRef $ x !! i !! (j - 1)
      -- slack[i][j] = slack[i][j - 1] - len(words[j]) - 1
      modifySTRef (x !! i !! j) (const $ slj1 - len (words0 !! j) - 1)

  return x

buildBreaks :: Int -> [[STRef s Int]] -> ST s [STRef s Int]
buildBreaks count slack = do
  -- minima = [0] + [10 ** 20] * count
  minimaHead <- newSTRef 0
  minimaTail <- replicateM count (newSTRef $ 10 ^ (20::Int))
  let minima = minimaHead : minimaTail
  -- breaks = [0] * count
  breaks <- replicateM count (newSTRef (0::Int))

  iref <- newSTRef 0
  costref <- newSTRef 0

  let
    baseCost = 10 ^ (10 :: Int)

    loop0 j = do
      i <- readSTRef iref
      if i >= 0
        then do -- while i >= 0:
          slackij <- readSTRef (slack !! i !! j)

          -- if slack[i][j] < 0:
          cost <- if slackij < 0
            then pure baseCost -- cost = 10 ** 10
            else do            -- cost = minima[i] + slack[i][j] ** 2
              minimaI <- readSTRef (minima !! i)
              pure $ minimaI + slackij ^ (2 :: Int)

          writeSTRef costref cost

          minimai1 <- readSTRef (minima !! (i + 1))
          -- if minima[j + 1] > cost:
          when (minimai1 > cost) $ do
            writeSTRef (minima !! (i + 1)) cost -- minima[j + 1] = cost
            writeSTRef (breaks !! j) i          -- breaks[j] = i
          modifySTRef iref (subtract 1)         -- i -= 1
          loop0 j
        else 
          pure ()

  -- for j in range(count)
  forM_ [0 .. count - 1] $ \j -> do
    writeSTRef iref j -- i = j
    loop0 j

  pure breaks

dynamicProgramming 
  :: ([a] -> a)
  -> (a -> [a])
  -> (a -> Int)
  -> a
  -> Width
  -> [a]
dynamicProgramming unsplit split len input width@(Width w) = if len input <= w then [input] else runST $ do
  let
    words0 = split input
    count  = length words0

  slack <- mkSlack len width words0 count
  breaks <- buildBreaks count slack

  -- j = count
  jref <- newSTRef count
  -- lines = []
  linesref <- newSTRef mempty

  let
    loop0 = do
      j <- readSTRef jref
      -- while j > 0:
      if j > 0
        then do
          -- i = breaks[j - 1]
          i <- readSTRef $ breaks !! (j - 1)
          let newline = unsplit . take (j - i) $ drop i words0
          -- lines.append(' '.join(words[i:j]))
          modifySTRef linesref (newline:)
          -- j = i
          writeSTRef jref i
          loop0
        else
          pure ()

  loop0

  readSTRef linesref
  
