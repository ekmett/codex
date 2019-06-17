module Data.LineWrap.Linear (linear) where

import Debug.Trace (traceShowM,traceShowId)
import Data.STRef
import Control.Monad.ST
import Control.Monad

import Data.Foldable (foldlM)
import Data.LineWrap.Types

-- import Data.Smawk

import Data.Vector (Vector)
import qualified Data.Vector as V

-- Total monotonicity
--
-- Each iteration of the dynamic programming scheme can also be seen
-- as filling in a matrix, where a cell adds up the least overall cost
-- to a subproblem (a column minimum) and a penalty. A concave weight
-- function implies that the matrix is totally monotone and in 1987
-- Shor, Moran, Aggarwal, Wilber and Klawe devised an algorithm which
-- finds the row maxima of such matrix in linear time. Even though
-- SMAWK can be modified to find column minima instead, it is not
-- possible to apply it directly to this "on-line" matrix as it might
-- try to evaluate a not "available" cell, i.e. a cell dependent on
-- some yet unknown column minimum. However, Wilber came up with an
-- algorithm in 1988 which "pretends" to know the minima and still
-- runs in O(n) time. An "ordered" algorithm which obeys the
-- availability of the matrix as presented by Aggarwal and Tokuyama in
-- 1998 follows.
--
-- def linear(text, width):
--     words = text.split()
--     count = len(words)
--     offsets = [0]
--     for w in words:
--         offsets.append(offsets[-1] + len(w))
--
--     minima = [0] + [10 ** 20] * count
--     breaks = [0] * (count + 1)
--
--     def cost(i, j):
--         w = offsets[j] - offsets[i] + j - i - 1
--         if w > width:
--             return 10 ** 10 * (w - width)
--         return minima[i] + (width - w) ** 2
--
--     def smawk(rows, columns):
--         stack = []
--         i = 0
--         while i < len(rows):
--             if stack:
--                 c = columns[len(stack) - 1]
--                 if cost(stack[-1], c) < cost(rows[i], c):
--                     if len(stack) < len(columns):
--                         stack.append(rows[i])
--                     i += 1
--                 else:
--                     stack.pop()
--             else:
--                 stack.append(rows[i])
--                 i += 1
--         rows = stack
--
--         if len(columns) > 1:
--             smawk(rows, columns[1::2])
--
--         i = j = 0
--         while j < len(columns):
--             if j + 1 < len(columns):
--                 end = breaks[columns[j + 1]]
--             else:
--                 end = rows[-1]
--             c = cost(rows[i], columns[j])
--             if c < minima[columns[j]]:
--                 minima[columns[j]] = c
--                 breaks[columns[j]] = rows[i]
--             if rows[i] < end:
--                 i += 1
--             else:
--                 j += 2
--
--     n = count + 1
--     i = 0
--     offset = 0
--     while True:
--         r = min(n, 2 ** (i + 1))
--         edge = 2 ** i + offset
--         smawk(range(0 + offset, edge), range(edge, r + offset))
--         x = minima[r - 1 + offset]
--         for j in range(2 ** i, r - 1):
--             y = cost(j + offset, r - 1 + offset)
--             if y <= x:
--                 n -= j
--                 i = 0
--                 offset += j
--                 break
--         else:
--             if r == n:
--                 break
--             i = i + 1
--
--     lines = []
--     j = count
--     while j > 0:
--         i = breaks[j]
--         lines.append(' '.join(words[i:j]))
--         j = i
--     lines.reverse()
--     return lines

--     def cost(i, j):
cost
  :: Int
  -> Vector Int
  -> Vector (STRef s Int)
  -> Int
  -> Int
  -> ST s Int
cost width offsets minima i j  = do
  -- w = offsets[j] - offsets[i] + j - i - 1
  let w = (offsets V.! j) - (offsets V.! i) + j - i - 1
  if w > width then
    -- return 10 ** 10 * (w - width)
    pure $ 10 ^ (10::Int) * (w - width)
  else
    -- return minima[i] + (width - w) ** 2
    (\mi -> mi + (width - 1) ^ (2::Int)) <$> readSTRef (minima V.! i)

--     def smawk(rows, columns):
smawkish
  :: Int
  -> Vector Int
  -> Vector (STRef s Int)
  -> Vector (STRef s Int)
  -> Vector (STRef s Int)
  -> Vector (STRef s Int)
  -> ST s ()
smawkish width offsets minima breaks rows columns = do
  -- i = 0
  iRef <- newSTRef 0
  -- stack = []
  stackRef <- newSTRef V.empty

  let
    costFn = cost width offsets minima

    -- while i < len(rows):
    rowL00p = do
      i <- readSTRef iRef
      when (i < V.length rows) $ do
        stackNotEmpty <- not . V.null <$> readSTRef stackRef
        -- if stack:
        if stackNotEmpty then do
          -- c = columns[len(stack) - 1]

          -- traceShowM "stack"
          -- traceShowM =<< readSTRef stackRef
          -- traceShowM "stack length"
          -- traceShowM . V.length =<< readSTRef stackRef
          -- traceShowM "columns"
          -- traceShowM =<< traverse readSTRef columns

          stackLength <- V.length <$> readSTRef stackRef 
          c <- readSTRef $ columns V.! (stackLength - 1)

          -- traceShowM "c"
          -- traceShowM c

          -- cost(stack[-1], c)
          cost0 <- readSTRef stackRef >>= \stack -> costFn (V.last stack) c

          -- cost(rows[i], c)
          rowI <- readSTRef $ rows V.! i
          cost1 <- costFn rowI c

          -- if cost(stack[-1], c) < cost(rows[i], c):
          if cost0 < cost1 then do
            -- if len(stack) < len(columns):
            when (stackLength < V.length columns) $
              -- stack.append(rows[i])
              modifySTRef stackRef (`V.snoc` rowI)
            -- i += 1
            modifySTRef iRef (+1) 
            rowL00p
          else do 
            -- stack.pop()
            modifySTRef stackRef V.init 
            rowL00p
        else do
          rowI <- readSTRef $ rows V.! i
          -- stack.append(rows[i])
          modifySTRef stackRef (`V.snoc` rowI)
          -- i += 1
          modifySTRef iRef (+1)
          rowL00p

  rowL00p

  -- rows = stack
  newRows <- traverse newSTRef =<< readSTRef stackRef

  -- if len(columns) > 1:
  when (V.length columns > 1) $ do
    -- columns[1::2]
    let secondIshColumns = V.ifilter (\i _ -> i `mod` 2 /= 0) columns
    -- smawk(rows, columns[1::2])
    smawkish width offsets minima breaks newRows secondIshColumns

  rows0 <- traverse readSTRef newRows

  -- i = j = 0
  writeSTRef iRef 0
  jRef <- newSTRef 0
  endRef <- newSTRef 0

  let
    columnL00p = do
      i <- readSTRef iRef
      j <- readSTRef jRef
      -- while j < len(columns):
      when (j < V.length columns) $ do
        -- if j + 1 < len(columns):
        if j + 1 < V.length columns then do
          -- end = breaks[columns[j + 1]]
          colJ1 <- readSTRef $ columns V.! (j + 1)
          readSTRef (breaks V.! colJ1) >>= writeSTRef endRef 
        else
          -- end = rows[-1]
          writeSTRef endRef (V.last rows0)
        colJ <- readSTRef $ columns V.! j
        -- c = cost(rows[i], columns[j])
        c <- costFn (rows0 V.! i) colJ
        mcj <- readSTRef (minima V.! colJ)
        -- if c < minima[columns[j]]:
        when (c < mcj) $ do
          -- minima[columns[j]] = c
          writeSTRef (minima V.! colJ) c
          -- breaks[columns[j]] = rows[i]
          writeSTRef (breaks V.! colJ) (rows0 V.! i)
        -- if rows[i] < end:
        end <- readSTRef endRef
        if (rows0 V.! i) < end then
          -- i += 1
          modifySTRef iRef (+1) >> columnL00p
        else
          -- j += 2
          modifySTRef jRef (+2) >> columnL00p

  columnL00p

innerLoop
  :: Int
  -> Vector Int
  -> Vector (STRef s Int)
  -> Vector (STRef s Int)
  -> STRef s Int
  -> STRef s Int
  -> STRef s Int
  -> ST s ()
innerLoop width offsets minima breaks nRef iRef offsetRef = do
  i <- readSTRef iRef
  n <- readSTRef nRef
  offset <- readSTRef offsetRef

  let
    -- r = min(n, 2 ** (i + 1))
    r = min n (2 ^ (i + 1))
    -- edge = 2 ** i + offset
    edge = 2 ^ (i + offset)

  traceShowM ("r", r, "edge", edge, "offset", offset)

  -- range(0 + offset, edge)
  traceShowM "rows"
  rowRefs <- traverse newSTRef $ traceShowId $ V.fromList [0 + offset .. edge - 1]
  -- range(edge, r + offset)
  traceShowM "cols"
  colRefs <- traverse newSTRef $ traceShowId $ V.fromList [edge .. r + offset - 1]

  -- smawk(range(0 + offset, edge), range(edge, r + offset))
  smawkish width offsets minima breaks rowRefs colRefs

  x <- readSTRef $ minima V.! (r - 1 + offset)

  traceShowM ("r", r - 2)
  -- [2 ^ i .. min 0 (r - 1)]
  broken <- (\f -> foldlM f False (V.enumFromN (2 ^ i) (r - 2))) $ \done j -> if done then pure done else do
    -- y = cost(j + offset, r - 1 + offset)
    y <- cost width offsets minima (j + offset) (r - 1 + offset)
    -- if y <= x:
    if y <= x then do
      -- n -= j
      modifySTRef nRef (subtract j)
      -- i = 0
      writeSTRef iRef 0
      -- offset += j
      modifySTRef offsetRef (+j)
      pure True
    else
      pure done

  n0 <- readSTRef nRef

  if not broken then
    if r == n0 then
      pure ()
    else do
      writeSTRef iRef (i + 1)
      innerLoop width offsets minima breaks nRef iRef offsetRef
  else
    innerLoop width offsets minima breaks nRef iRef offsetRef

-- def linear(text, width):
linear
  :: ([a] -> a)
  -> (a -> [a])
  -> (a -> Int)
  -> a
  -> Width
  -> [a]
linear unsplit split len input (Width width) = if len input <= width then [input] else runST $ do
  let
    words0 = V.fromList $ split input
    count = V.length words0
    -- offsets = [0]
    -- for w in words:
    --   offsets.append(offsets[-1] + len(w))
    offsets = V.scanl (\offset -> (offset +) . len) 0 words0
    initialMinima = 10 ^ (20::Int)

  -- minima = [0] + [10 ** 20] * count
  minima <- V.cons <$> newSTRef 0 <*> V.replicateM count (newSTRef initialMinima)
  
  -- breaks = [0] * (count + 1)
  breaks <- V.replicateM (count + 1) $ newSTRef 0

  -- n = count + 1
  nRef <- newSTRef (count + 1)
  -- i = 0
  iRef <- newSTRef 0
  -- offset = 0
  offsetRef <- newSTRef 0

  innerLoop width offsets minima breaks nRef iRef offsetRef

  -- lines = []
  linesref <- newSTRef mempty
  -- j = count
  jref <- newSTRef count

  let
    loop1 = do
      j <- readSTRef jref
      -- while j > 0:
      when (j > 0) $ do
        -- i = breaks[j]
        i <- readSTRef $ breaks V.! j
        let newline = unsplit . V.toList . V.take (j - i) $ V.drop i words0
        modifySTRef linesref (newline:)
        -- j = i
        writeSTRef jref i
        loop1

  loop1

  readSTRef linesref
