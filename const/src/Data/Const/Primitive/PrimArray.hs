{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}

-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Data.Const.Primitive.PrimArray
  ( ConstPrimArray
  , constPrimArray
  , APrimArray

  , readAPrimArray
  , samePrimArray
  , copyAPrimArray
  , copyAPrimArrayToPtr
  , sizeofAPrimArray
  , getSizeofAPrimArray
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Type.Coercion
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Primitive.Ptr (Ptr (..))

import Data.Const.Unsafe

constPrimArray :: APrimArray s p => p a -> ConstPrimArray s a
constPrimArray = constant
{-# inline constPrimArray #-}

readAPrimArray :: forall a m p. (Prim a, PrimMonad m, APrimArray (PrimState m) p) => p a -> Int -> m a
readAPrimArray = gcoerceWith (unsafePrimArrayCoercion @(PrimState m) @p @a) $ coerce $ readPrimArray @a @m
{-# inline readAPrimArray #-}

samePrimArray :: forall s a p q. (APrimArray s p, APrimArray s q) => p a -> q a -> Bool
samePrimArray
  = gcoerceWith (unsafePrimArrayCoercion @s @p @a) 
  $ gcoerceWith (unsafePrimArrayCoercion @s @q @a) 
  $ coerce $ sameMutablePrimArray @s @a
{-# inline samePrimArray #-}

copyAPrimArray :: forall m a p. (Prim a, PrimMonad m, APrimArray (PrimState m) p) => MutablePrimArray (PrimState m) a -> Int -> p a -> Int -> Int -> m ()
copyAPrimArray = gcoerceWith (unsafePrimArrayCoercion @(PrimState m) @p @a) $ coerce $ copyMutablePrimArray @m @a
{-# inline copyAPrimArray #-}

copyAPrimArrayToPtr :: forall m a p. (Prim a, PrimMonad m, APrimArray (PrimState m) p) => Ptr a -> p a -> Int -> Int -> m ()
copyAPrimArrayToPtr = gcoerceWith (unsafePrimArrayCoercion @(PrimState m) @p @a) $ coerce $ copyMutablePrimArrayToPtr @m @a
{-# inline copyAPrimArrayToPtr #-}

sizeofAPrimArray :: forall s a p. (Prim a, APrimArray s p) => p a -> Int
sizeofAPrimArray = gcoerceWith (unsafePrimArrayCoercion @s @p @a) $ coerce $ sizeofMutablePrimArray @s @a
{-# inline sizeofAPrimArray #-}

getSizeofAPrimArray :: forall m a p. (Prim a, PrimMonad m, APrimArray (PrimState m) p) => p a -> m Int
getSizeofAPrimArray =  gcoerceWith (unsafePrimArrayCoercion @(PrimState m) @p @a) $ coerce $ getSizeofMutablePrimArray @m @a
{-# inline getSizeofAPrimArray #-}
