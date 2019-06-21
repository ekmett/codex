{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language BlockArguments #-}

-- |
-- Copyright :  (c) Edward Kmett 2018-2019
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Fast unique symbols. These form a 'ByteArray#'
-- and use its original memory address to form
-- the hash code, but use its ongoing location
-- to check for equality, so nothing is held in
-- place.
module Data.Primitive.Unique
  ( Unique, UniqueM
  , newUnique
  ) where

import Control.Monad.Primitive
import Data.Hashable
import GHC.Prim
import GHC.Ptr
import GHC.Types

data Unique s = Unique Addr# ByteArray#
type UniqueM m = Unique (PrimState m)

instance Eq (Unique s) where
  Unique _ p == Unique _ q = isTrue# (unsafeCoerce# sameMutableByteArray# p q)

-- | Non-deterministic, do not rely on order when reasoning about @'ST' s@!
instance Hashable (Unique s) where
  hash (Unique p _) = hash (Ptr p)
  hashWithSalt d (Unique p _)  = hashWithSalt d (Ptr p)

newUnique :: PrimMonad m => m (UniqueM m)
newUnique = primitive \s -> case newByteArray# 0# s of
  (# s', mba #) -> case unsafeFreezeByteArray# mba s' of
    (# s'', ba #) -> (# s'', Unique (byteArrayContents# ba) ba #)
