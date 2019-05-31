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
--
module Data.Const.Primitive.SmallArray
( SmallConstArray
, smallConstArray
, ASmallArray
, readASmallArray
, sameSmallArray
, copyASmallArray
, cloneASmallArray
, sizeofASmallArray
) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Type.Coercion
import Data.Primitive.SmallArray

import Data.Const.Unsafe

smallConstArray :: ASmallArray s p => p a -> SmallConstArray s a
smallConstArray = constant
{-# inline smallConstArray #-}

readASmallArray :: forall m p a. (PrimMonad m, ASmallArray (PrimState m) p) => p a -> Int -> m a
readASmallArray = gcoerceWith (unsafeSmallArrayCoercion @(PrimState m) @p @a) $ coerce $ readSmallArray @m @a
{-# inline readASmallArray #-}

sameSmallArray :: forall s p q a. (ASmallArray s p, ASmallArray s q) => p a -> q a -> Bool
sameSmallArray
  = gcoerceWith (unsafeSmallArrayCoercion @s @p @a) 
  $ gcoerceWith (unsafeSmallArrayCoercion @s @q @a) 
  $ coerce $ (==) @(SmallMutableArray s a)
{-# inline sameSmallArray #-}

copyASmallArray :: forall m p a. (PrimMonad m, ASmallArray (PrimState m) p) => SmallMutableArray (PrimState m) a -> Int -> p a -> Int -> Int -> m ()
copyASmallArray = gcoerceWith (unsafeSmallArrayCoercion @(PrimState m) @p @a) $ coerce $ copySmallMutableArray @m @a
{-# inline copyASmallArray #-}

cloneASmallArray :: forall m p a. (PrimMonad m, ASmallArray (PrimState m) p) => p a -> Int -> Int -> m (SmallMutableArray (PrimState m) a)
cloneASmallArray =  gcoerceWith (unsafeSmallArrayCoercion @(PrimState m) @p @a) $ coerce $ cloneSmallMutableArray @m @a
{-# inline cloneASmallArray #-}

sizeofASmallArray :: forall s p a. ASmallArray s p => p a -> Int
sizeofASmallArray = gcoerceWith (unsafeSmallArrayCoercion @s @p @a) $ coerce $ sizeofSmallMutableArray @s @a
{-# inline sizeofASmallArray #-}
