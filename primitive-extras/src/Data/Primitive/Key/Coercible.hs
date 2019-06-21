{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language GADTs #-}
{-# language RoleAnnotations #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

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
--
-- but it is left Coercible, this should be legal directly using things in base,
-- but we're currently missing a TestCoercion instance for STRefs

module Data.Primitive.Key.Coercible
  ( Key, newKey
  , Box(Lock), unlock
  , AsCoercibleKey(..)
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Hashable
import Data.Primitive.Unique
import Data.Type.Coercion
import Unsafe.Coerce

newtype Key a = Key Unique
  deriving newtype (Eq, Hashable, AsUnique)

type role Key representational

instance TestCoercion Key where
  testCoercion (Key s :: Key a) (Key t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise           = Nothing
  {-# inline testCoercion #-}

newKey :: PrimMonad m => m (Key a)
newKey = unsafeIOToPrim $ Key <$> newUnique
{-# inline newKey #-}

data Box where
  Lock :: {-# unpack #-} !(Key a) -> a -> Box

unlock :: AsCoercibleKey a t => t -> Box -> Maybe a
unlock k (Lock l x) = case testCoercion (coercibleKey k) l of
  Just Coercion -> Just $ coerce x
  Nothing -> Nothing
{-# inline unlock #-}

class AsCoercibleKey a t | t -> a where
  coercibleKey :: t -> Key a

instance AsCoercibleKey a (Key a) where
  coercibleKey = id
