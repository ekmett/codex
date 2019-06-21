{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language RoleAnnotations #-}

-- |
-- Copyright :  (c) Edward Kmett 2018-2019
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This construction is based on
-- <https://people.seas.harvard.edu/~pbuiras/publications/KeyMonadHaskell2016.pdf The Key Monad: Type-Safe Unconstrained Dynamic Typing>
-- by Atze van der Ploeg, Koen Claessen, and Pablo Buiras

module Data.Primitive.Key
  ( Key, newKey
  , Box(Lock), unlock
  ) where

import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Proxy
import Data.Type.Coercion
import Data.Type.Equality
import Unsafe.Coerce

-- move to Equality.Key?
-- why do we need a region?
newtype Key a = Key (MutVar RealWorld (Proxy a))

type role Key nominal

instance TestEquality Key where
  testEquality (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  {-# inline testEquality #-}

instance TestCoercion Key where
  testCoercion (Key s :: Key a) (Key t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

newKey :: PrimMonad m => m (Key a)
newKey = unsafeIOToPrim $ Key <$> newMutVar Proxy
{-# inline newKey #-}

data Box where
  Lock :: {-# unpack #-} !(Key a) -> a -> Box

unlock :: Key a -> Box -> Maybe a
unlock k (Lock l x) = case testEquality k l of
  Just Refl -> Just x
  Nothing -> Nothing
{-# inline unlock #-}
