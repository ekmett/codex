module Data.LineWrap.ShortestPath (shortestPath) where

import Data.STRef
import Control.Monad.ST
import Control.Monad
import Data.LineWrap.Types

-- Shortest path
--
-- The previous way can be sped up even further: the length offsets
-- used to calculate any line length in constant time can easily be
-- pre-processed in O(n), rather than O(n ^ 2), and there is no point
-- in putting ever more words on a line once it reaches the allowed
-- width. The performance then improves down to O(n * width).
--
-- This is exactly the same result as if the text was thought of as a
-- (topologically sorted) directed acyclic graph, with the nodes and
-- arcs representing words and breaks, respectively. By substituting
-- the penalties for the weights, the problem becomes the one of
-- finding the shortest path which is known to be solvable in linear
-- time. Note that the number of edges remains O(n * width).
--
-- Flat uses the latter method.
--
-- def shortest(text, width):
--     words = text.split()
--     count = len(words)
--     offsets = [0]
--     for w in words:
--         offsets.append(offsets[-1] + len(w))
--
--     minima = [0] + [10 ** 20] * count
--     breaks = [0] * (count + 1)
--     for i in range(count):
--         j = i + 1
--         while j <= count:
--             w = offsets[j] - offsets[i] + j - i - 1
--             if w > width:
--                 break
--             cost = minima[i] + (width - w) ** 2
--             if cost < minima[j]:
--                 minima[j] = cost
--                 breaks[j] = i
--             j += 1
--
--     lines = []
--     j = count
--     while j > 0:
--         i = breaks[j]
--         lines.append(' '.join(words[i:j]))
--         j = i
--     lines.reverse()
--     return lines

shortestPath :: ([a] -> a) -> (a -> [a]) -> (a -> Int) -> a -> Width -> [a]
shortestPath unsplit split len input w@(Width width) = if len input <= width then [input]
  else shortestPath0 unsplit split len input w

shortestPath0 :: ([a] -> a) -> (a -> [a]) -> (a -> Int) -> a -> Width -> [a]
shortestPath0 unsplit split len input (Width width) = runST $ do
  let
    words0 = split input
    count = length words0
    -- offsets = [0]
    -- for w in words:
    --   offsets.append(offsets[-1] + len(w))
    offsets = scanl (\offset -> (offset+) . len) 0 words0

    inf = 10 ^ (20::Int)

  -- minima = [0] + [10 ** 20] * count
  minima <- (:) <$> newSTRef 0 <*> replicateM count (newSTRef inf)
  -- breaks = [0] * (count + 1)
  breaks <- replicateM (count + 1) (newSTRef (0::Int))
  
  jref <- newSTRef 0

  let
    loop0 i = do
      j <- readSTRef jref
      -- while j <= count:
      when (j <= count) $ do
        -- w = offsets[j] - offsets[i] + j - i - 1
        let w = (offsets !! j) - (offsets !! i) + j - i - 1
        -- if w > width: break
        unless (w > width) $ do
          -- cost = minima[i] + (width - w) ** 2
          cost <- (\mi -> mi + (width - w) ^ (2::Int)) <$> readSTRef (minima !! i)
          mj <- readSTRef (minima !! j)
          -- if cost < minima[j]:
          when (cost < mj) $ do
            -- minima[j] = cost
            writeSTRef (minima !! j) cost
            -- breaks[j] = i
            writeSTRef (breaks !! j) i
          -- j += 1
          modifySTRef jref (+1)
          loop0 i

  -- for i in range(count):
  forM_ [0 .. count - 1] $ \i -> do
    -- j = i + 1
    writeSTRef jref (i+1)
    loop0 i

  -- lines = []
  linesref <- newSTRef mempty

  let
    loop1 = do
      j <- readSTRef jref
      -- while j > 0:
      when (j > 0) $ do
        -- i = breaks[j]
        i <- readSTRef $ breaks !! j
        let newline = unsplit . take (j - i) $ drop i words0
        modifySTRef linesref (newline:)
        -- j = i
        writeSTRef jref i
        loop1

  -- j = count
  writeSTRef jref count
  loop1

  readSTRef linesref
