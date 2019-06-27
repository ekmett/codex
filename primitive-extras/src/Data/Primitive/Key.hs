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

module Data.Primitive.Key
  ( Key, newKey
  , Box(Lock), unlock
  , AsKey(..)
  ) where

import Control.Monad.Primitive
import Data.Hashable
import qualified Data.Primitive.Key.Coercible as Coercible
import Data.Primitive.Unique
import Data.Type.Coercion
import Data.Type.Equality
import Unsafe.Coerce

-- move to Equality.Key?
-- why do we need a region?
newtype Key a = Key (Coercible.Key a)
  deriving newtype (Eq, Hashable, Coercible.AsCoercibleKey a, AsUnique)

type role Key nominal

instance TestEquality Key where
  testEquality (Key s) (Key t)
    | s == unsafeCoerce t = Just (unsafeCoerce Refl)
    | otherwise           = Nothing
  {-# inline testEquality #-}

instance TestCoercion Key where
  testCoercion (Key s) (Key t) = testCoercion s t
  {-# inline testCoercion #-}

newKey :: PrimMonad m => m (Key a)
newKey = unsafeIOToPrim $ Key <$> Coercible.newKey
{-# inline newKey #-}

data Box f where
  Lock :: {-# unpack #-} !(Key a) -> f a -> Box f

instance AsUnique (Box f) where
  unique (Lock k _) = unique k

unlock :: AsKey a t => t -> Box f -> Maybe (f a)
unlock k (Lock l x) = case testEquality (key k) l of
  Just Refl -> Just x
  Nothing -> Nothing
{-# inline unlock #-}

class AsKey a t | t -> a where
  key :: t -> Key a

instance AsKey a (Key a) where
  key = id
