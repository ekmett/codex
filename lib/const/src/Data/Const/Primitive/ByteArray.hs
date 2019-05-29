{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
{-# language CPP #-}

-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable

module Data.Const.Primitive.ByteArray
  ( ConstByteArray
  , constByteArray
  , AByteArray

  , readAByteArray
  , sameByteArray
  , copyAByteArray
  , copyAByteArrayToAddr
  , moveAByteArray
  , sizeofAByteArray
  , getSizeofAByteArray
  , isAByteArrayPinned
  , constByteArrayContents
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Type.Coercion
import Data.Primitive.ByteArray
import Data.Primitive.Ptr (Ptr (..))
import Data.Primitive.Types
import Data.Word

import Data.Const.Unsafe

#if MIN_VERSION_primitive(0,7,0)
type Addr = Ptr ()
#endif

constByteArray :: AByteArray p => p s -> ConstByteArray s
constByteArray = constant
{-# inline constByteArray #-}

readAByteArray :: forall a m p. (Prim a, PrimMonad m, AByteArray p) => p (PrimState m) -> Int -> m a
readAByteArray = gcoerceWith (unsafeByteArrayCoercion @p @(PrimState m)) $ coerce $ readByteArray @a @m
{-# inline readAByteArray #-}

sameByteArray :: forall s p q. (AByteArray p, AByteArray q) => p s -> q s -> Bool
sameByteArray
  = gcoerceWith (unsafeByteArrayCoercion @p @s) 
  $ gcoerceWith (unsafeByteArrayCoercion @q @s) 
  $ coerce $ sameMutableByteArray @s
{-# inline sameByteArray #-}

copyAByteArray :: forall m p. (AByteArray p, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> p (PrimState m) -> Int -> Int -> m ()
copyAByteArray = gcoerceWith (unsafeByteArrayCoercion @p @(PrimState m)) $ coerce $ copyMutableByteArray @m
{-# inline copyAByteArray #-}

copyAByteArrayToAddr :: forall m p. (AByteArray p, PrimMonad m) => Addr -> p (PrimState m) -> Int -> Int -> m ()
copyAByteArrayToAddr = gcoerceWith (unsafeByteArrayCoercion @p @(PrimState m)) $ coerce $ copyMutableByteArrayToAddr @m
{-# inline copyAByteArrayToAddr #-}

moveAByteArray :: forall m p. (AByteArray p, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> p (PrimState m) -> Int -> Int -> m ()
moveAByteArray = gcoerceWith (unsafeByteArrayCoercion @p @(PrimState m)) $ coerce $ moveByteArray @m
{-# inline moveAByteArray #-}

sizeofAByteArray :: forall s p. AByteArray p => p s -> Int
sizeofAByteArray = gcoerceWith (unsafeByteArrayCoercion @p @s) $ coerce $ sizeofMutableByteArray @s
{-# inline sizeofAByteArray #-}

getSizeofAByteArray :: forall m p. (PrimMonad m, AByteArray p) => p (PrimState m) -> m Int
getSizeofAByteArray =  gcoerceWith (unsafeByteArrayCoercion @p @(PrimState m)) $ coerce $ getSizeofMutableByteArray @m
{-# inline getSizeofAByteArray #-}

isAByteArrayPinned :: forall s p. AByteArray p => p s -> Bool
isAByteArrayPinned = gcoerceWith (unsafeByteArrayCoercion @p @s) $ coerce $ isMutableByteArrayPinned @s
{-# inline isAByteArrayPinned #-}

-- | Only safe on a pinned ByteArray or pinned ConstByteArray
constByteArrayContents :: forall s p. AByteArray p => p s -> ConstPtr Word8
#if MIN_VERSION_primitive(0,7,0)
constByteArrayContents = gcoerceWith (unsafeByteArrayCoercion @p @s) $ coerce $ mutableByteArrayContents @s
#else
constByteArrayContents = gcoerceWith (unsafeByteArrayCoercion @p @s) $ coerce $ (\(Addr a) -> Ptr a) . mutableByteArrayContents @s
#endif
{-# inline constByteArrayContents #-}
