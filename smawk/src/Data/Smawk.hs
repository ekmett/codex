{-# options_ghc -Wno-incomplete-uni-patterns #-}

-- | This module implements a generalized version of the SMAWK algorithm
-- for computing row minima in totally ordered matrices.
--
-- I do not require rows or column numbers to be actual numbers, or even ordered,
-- instead comparing columns using occurrence order.
--
-- Unlike @Map@-based implementations, the runtime of this is actually linear.
module Data.Smawk
  ( smawk
  , smawk1
  ) where

import Control.Monad.Trans.State.Strict
import qualified Data.Foldable as Foldable
import Data.Maybe (fromJust)
import Data.Semigroup (Min(..),Arg(..))
import Data.Semigroup.Foldable (Foldable1)
import Data.List.NonEmpty (nonEmpty)
import Data.Primitive.Array (indexArray)
import GHC.Exts as Exts

-- |
-- >>> collate "abcde"
-- ("ace","bd")
collate :: [a] -> ([a],[a])
collate = Prelude.foldr (\a ~(x,y) -> (a:y,x)) mempty

-- |
-- @'uncurry' 'interleave' . 'collate' = id@
--
-- >>> interleave "ace" "bd"
-- "abcde"
interleave :: [a] -> [a] -> [a]
interleave (a:as) bs = a:interleave bs as
interleave [] bs = bs

-- |
-- /O(|rows| + |cols|)/.
--
-- Computes __row__ minima in totally monotone matrices using the SMAWK algorithm.
--
-- Returns 'Nothing' if we have no columns.
smawk
  :: (Traversable f, Foldable g, Ord a)
  => f r -- ^ rows (in any desired ascending order)
  -> g c -- ^ columns (in any desired ascending order)
  -> (r -> c -> a) -- ^ a monotone matrix
  -> Maybe (f c) -- ^ each of the row minima
smawk rs cs0 m = (\cs -> smawk1 rs cs m) <$> nonEmpty (Foldable.toList cs0)
{-# inline smawk #-}

-- |
-- /O(|rows| + |cols|)/.
--
-- Computes __row__ minima in totally monotone matrices using the SMAWK algorithm.
smawk1
  :: (Traversable f, Foldable1 g, Ord a)
  => f r -- ^ rows (in any desired ascending order)
  -> g c -- ^ columns (in any desired ascending order)
  -> (r -> c -> a) -- ^ a monotone matrix
  -> f c -- ^ each of the row minima
smawk1 rs0 cs0 m = evalState (traverse refill rs0) $ go (Foldable.toList rs0) [0..length raws-1] where
  raws = Exts.fromList $ Foldable.toList cs0
  refill _ = state $ \ ~(x:xs) -> (indexArray raws x,xs)
  go [] _ = []
  go rs cs = interleave broken minima where
    m' r c = m r (indexArray raws c)
    broken = zipWith skim es $ path cs' (minima <> repeat l)
    -- skim a bs = snd $ minimum $ fmap (\i -> (m i a, i)) bs
    skim a bs = case getMin $ fromJust $ foldMap (\i -> Just $ Min $ Arg (m' a i) i) bs of Arg _ i -> i
    path xs ~(y:ys) = case span (<=y) xs of
      (as, bs) -> as:path (y:bs) ys
    rs' = Exts.fromList rs -- a zipper yields same complexity, worst constants
    n = length rs'
    minima = go os cs'
    (es,os) = collate rs
    rcs' = reduce cs [] 0
    cs' = reverse rcs'
    l = head rcs'
    reduce [] ys _ = ys
    reduce (x:xs) [] _ = reduce xs [x] 1
    reduce xxs@(x:xs) yys@(y:ys) t
      | ri <- indexArray rs' (t-1), m' ri y > m' ri y = reduce xxs ys (t-1)
      | t /= n = reduce xs (x:yys) (t+1)
      | otherwise = reduce xs yys t
{-# inlinable smawk1 #-}
