module Data.LineWrap.BinarySearch (binarySearch) where

import Control.Applicative

import Data.STRef
import Control.Monad.ST

import Control.Monad

import Data.Functor ((<&>))
import qualified Data.Vector as V
import Data.Sequence (ViewL (..), ViewR (..))
import qualified Data.Sequence as S
import Data.LineWrap.Types

-- Binary search
--
-- The method using dynamic programming can be written as two nested
-- loops: the outer one iterates over every word and the inner one
-- searches for the most suitable break. Hirschberg and Larmore showed
-- in 1987 an algorithm which is able to use binary search instead of
-- the inner loop in the case the weight function is "concave". It
-- turns out that the penalty discussed thus far can be made to
-- satisfy this property by handling the "infinities" slightly
-- differently, giving an overall time of O(n * log n). A similar
-- algorithm due to Galil and Giancarlo from 1989 is given below.
--
-- The concave property says: w(i, j) + w(i', j') <= w(i', j) + w(i, j'), where i < i' < j < j'.
--
-- from collections import deque
--
-- def binary(text, width):
--     words = text.split()
--     count = len(words)
--     offsets = [0]
--     for w in words:
--         offsets.append(offsets[-1] + len(w))
--
--     minima = [0] * (count + 1)
--     breaks = [0] * (count + 1)
--
--     def c(i, j):
--         w = offsets[j] - offsets[i] + j - i - 1
--         if w > width:
--             return 10 ** 10 * (w - width)
--         return minima[i] + (width - w) ** 2
--
--     def h(l, k):
--         low, high = l + 1, count
--         while low < high:
--             mid = (low + high) // 2
--             if c(l, mid) <= c(k, mid):
--                 high = mid
--             else:
--                 low = mid + 1
--         if c(l, high) <= c(k, high):
--             return high
--         return l + 2
--
--     q = deque([(0, 1)])
--     for j in range(1, count + 1):
--         l = q[0][0]
--         if c(j - 1, j) <= c(l, j):
--             minima[j] = c(j - 1, j)
--             breaks[j] = j - 1
--             q.clear()
--             q.append((j - 1, j + 1))
--         else:
--             minima[j] = c(l, j)
--             breaks[j] = l
--             while c(j - 1, q[-1][1]) <= c(q[-1][0], q[-1][1]):
--                 q.pop()
--             q.append((j - 1, h(j - 1, q[-1][0])))
--             if j + 1 == q[1][1]:
--                 q.popleft()
--             else:
--                 q[0] = q[0][0], (q[0][1] + 1)
--
--     lines = []
--     j = count
--     while j > 0:
--         i = breaks[j]
--         lines.append(' '.join(words[i:j]))
--         j = i
--     lines.reverse()
--     return lines

unsafeRight :: STRef s (S.Seq a) -> ST s (S.Seq a, a)
unsafeRight sref = readSTRef sref <&> \s -> case S.viewr s of
  EmptyR -> error "empty binary search queue!!"
  xs :> x -> (xs,x)

unsafeLeft :: STRef s (S.Seq a) -> ST s (a, S.Seq a)
unsafeLeft sref = readSTRef sref <&> \s -> case S.viewl s of
  EmptyL -> error "empty binary search queue!!"
  x :< xs -> (x,xs)

unsafeRightVal = fmap snd . unsafeRight
unsafeLeftVal = fmap fst . unsafeLeft

binarySearch
  :: ([a] -> a)
  -> (a -> [a])
  -> (a -> Int)
  -> a
  -> Width
  -> [a]
binarySearch unsplit split len input (Width width) = if len input <= width then [input] else runST $ do
  let
    -- words = text.split()
    words0 = split input
    -- count = len(words)
    count = length words0
    -- offsets = [0]
    -- for w in words:
    --   offsets.append(offsets[-1] + len(w))
    offsets = scanl (\offset -> (offset +) . len) 0 words0

  -- minima = [0] * (count + 1)
  minima <- replicateM (count + 1) $ newSTRef 0
  -- breaks = [0] * (count + 1)
  breaks <- replicateM (count + 1) $ newSTRef 0

  let
    -- def c(i, j):
    cFn i j =
      let
        -- w = offsets[j] - offsets[i] + j - i - 1
        w = (offsets !! j) - (offsets !! i) + j - i - 1
      in 
        -- if w > width:
        if w > width then
          -- return 10 ** 10 * (w - width)
          pure $ 10 ^ 10 * (w - width)
        else 
          -- return minima[i] + (width - w) ** 2
          (\mi -> mi + (width - w) ^ 2) <$> readSTRef (minima !! i)
                
  -- Needed for 'h' function
  lowRef <- newSTRef 0
  highRef <- newSTRef 0
  midRef <- newSTRef 0

--         else:

  let
    hFn_l00p l k = do
      low0 <- readSTRef lowRef
      high0 <- readSTRef highRef
      -- while low < high:
      when (low0 < high0) $ do
        -- mid = (low + high) // 2
        writeSTRef midRef $ (low0 + high0) `div` 2
        mid <- readSTRef midRef
        clmid <- cFn l mid -- c(l, mid) 
        ckmid <- cFn k mid -- c(k, mid)
        -- if c(l, mid) <= c(k, mid):
        if clmid <= ckmid
          -- high = mid
          then writeSTRef highRef mid
          -- low = mid + 1
          else writeSTRef lowRef (mid + 1)

    -- def h(l, k):
    hFn l k = do
      -- low, high = l + 1, count
      writeSTRef lowRef (l + 1)
      writeSTRef highRef count

      hFn_l00p l k
      high0 <- readSTRef highRef
      clhigh <- cFn l high0 -- c(l, high)
      ckhigh <- cFn k high0 -- c(k, high)
      -- if c(l, high) <= c(k, high):
      if clhigh <= ckhigh
        -- return high
        then pure high0         
        -- return l + 2
        else pure $ l + 2       

  -- q = deque([(0, 1)])
  qRef <- newSTRef =<< (S.fromList . pure <$> liftA2 (,) (newSTRef 0) (newSTRef 1))

  let
  
    q_l00p j = do
      qR1 <- unsafeRightVal qRef >>= readSTRef . snd
      qR0 <- unsafeRightVal qRef >>= readSTRef . fst

      cj1qR1 <- cFn (j - 1) qR1
      cqR0qR1 <- cFn qR0 qR1

      -- while c(j - 1, q[-1][1]) <= c(q[-1][0], q[-1][1]):
      when (cj1qR1 <= cqR0qR1) $ do
        q <- unsafeRight qRef
        --   q.pop()
        writeSTRef qRef (fst q)
        q_l00p j

  lRef <- newSTRef 0
  -- for j in range(1, count + 1):
  forM_ [1 .. count] $ \j -> do
    -- l = q[0][0]
    l <- unsafeLeftVal qRef >>= readSTRef . fst >>= writeSTRef lRef >> readSTRef lRef
    cj1j <- cFn (j - 1) j
    clj <- cFn l j

    if cj1j <= clj then do
      -- minima[j] = c(j - 1, j)
      writeSTRef (minima !! j) cj1j
      -- breaks[j] = j - 1
      writeSTRef (breaks !! j) (j - 1)
      -- q.clear()
      writeSTRef qRef S.empty
      -- q.append((j - 1, j + 1))
      newQVal <- (,) <$> newSTRef (j - 1) <*> newSTRef (j + 1)
      modifySTRef qRef (S.|> newQVal)
    else do
      -- minima[j] = c(l, j)
      writeSTRef (minima !! j) clj
      -- breaks[j] = l
      writeSTRef (breaks !! j) l

      -- while c(j - 1, q[-1][1]) <= c(q[-1][0], q[-1][1]):
      --   q.pop()
      q_l00p j

      qR0 <- unsafeRightVal qRef >>= readSTRef . fst
      hj1qR0 <- hFn (j - 1) qR0
      newQVal <- (,) <$> newSTRef (j - 1) <*> newSTRef hj1qR0
      -- q.append((j - 1, h(j - 1, q[-1][0])))
      modifySTRef qRef (S.|> newQVal)

      q11 <- readSTRef . snd . (`S.index` 1) =<< readSTRef qRef
      -- if j + 1 == q[1][1]:
      if (j + 1) == q11 then
        -- q.popleft() -- Remove the leftmost element
        unsafeLeft qRef >>= writeSTRef qRef . snd 
      else do
        q <- readSTRef qRef
        -- q[0] = q[0][0], (q[0][1] + 1)
        modifySTRef (snd $ S.index q 0) (+1)

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
        i <- readSTRef $ breaks !! j
        let newline = unsplit . take (j - i) $ drop i words0
        -- lines.append(' '.join(words[i:j]))
        modifySTRef linesref (newline:)
        -- j = i
        writeSTRef jref i
        loop1

  -- j = count
  writeSTRef jref count
  loop1

  -- return lines
  readSTRef linesref


