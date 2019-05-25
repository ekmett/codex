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

module Data.Const.Primitive.Array
  ( ConstArray
  , constArray
  , AnArray
  , readAnArray
  , sameArray
  , copyAnArray
  , cloneAnArray
  , sizeofAnArray
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Type.Coercion
import Data.Primitive.Array

import Data.Const.Unsafe

constArray :: AnArray s p => p a -> ConstArray s a
constArray = constant
{-# inline constArray #-}

readAnArray :: forall m p a. (PrimMonad m, AnArray (PrimState m) p) => p a -> Int -> m a
readAnArray = gcoerceWith (unsafeArrayCoercion @(PrimState m) @p @a) $ coerce $ readArray @m @a
{-# inline readAnArray #-}

sameArray :: forall s p q a. (AnArray s p, AnArray s q) => p a -> q a -> Bool
sameArray
  = gcoerceWith (unsafeArrayCoercion @s @p @a) 
  $ gcoerceWith (unsafeArrayCoercion @s @q @a) 
  $ coerce $ sameMutableArray @s @a
{-# inline sameArray #-}

copyAnArray :: forall m p a. (PrimMonad m, AnArray (PrimState m) p) => MutableArray (PrimState m) a -> Int -> p a -> Int -> Int -> m ()
copyAnArray = gcoerceWith (unsafeArrayCoercion @(PrimState m) @p @a) $ coerce $ copyMutableArray @m @a
{-# inline copyAnArray #-}

cloneAnArray :: forall m p a. (PrimMonad m, AnArray (PrimState m) p) => p a -> Int -> Int -> m (MutableArray (PrimState m) a)
cloneAnArray =  gcoerceWith (unsafeArrayCoercion @(PrimState m) @p @a) $ coerce $ cloneMutableArray @m @a
{-# inline cloneAnArray #-}

sizeofAnArray :: forall s p a. AnArray s p => p a -> Int
sizeofAnArray = gcoerceWith (unsafeArrayCoercion @s @p @a) $ coerce $ sizeofMutableArray @s @a
{-# inline sizeofAnArray #-}
