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

module Foreign.Const.Ptr
  ( ConstPtr
  , constPtr
  , APtr

  -- * const-agnostic operations
  , peekAt
  , peekAtElemOff
  , peekAtByteOff
  , minusAPtr

  -- * operations returning const pointers

  , nullConstPtr
  , castAPtr
  , castConstPtr
  , plusAPtr
  , plusConstPtr
  , alignAPtr
  , alignConstPtr
  ) where

import Data.Coerce
import Data.Type.Coercion
import Foreign.Ptr
import Foreign.Storable

import Data.Const.Unsafe

constPtr :: APtr p => p a -> ConstPtr a
constPtr = constant
{-# inline constPtr #-}

peekAt :: forall p a. (Storable a, APtr p) => p a -> IO a
peekAt = gcoerceWith (unsafePtrCoercion @p @a) (coerce (peek @a))
{-# inline peekAt #-}

peekAtElemOff :: forall p a. (Storable a, APtr p) => p a -> Int -> IO a
peekAtElemOff = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (peekElemOff @a)
{-# inline peekAtElemOff #-}

peekAtByteOff :: forall p a b. (Storable a, APtr p) => p b -> Int -> IO a
peekAtByteOff = gcoerceWith (unsafePtrCoercion @p @b) $ coerce (peekByteOff @a @b)
{-# inline peekAtByteOff #-}

nullConstPtr :: ConstPtr a 
nullConstPtr = ConstPtr nullPtr
{-# inline nullConstPtr #-}

castAPtr :: forall p a b. APtr p => p a -> p b
castAPtr = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @p @b) $ coerce (castPtr @a @b)
{-# inline castAPtr #-}

castConstPtr :: forall p a b. APtr p => p a -> ConstPtr b
castConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (castPtr @a @b)
{-# inline castConstPtr #-}

plusAPtr :: forall p a b. APtr p => p a -> Int -> p b
plusAPtr = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @p @b) $ coerce (plusPtr @a @b)
{-# inline plusAPtr #-}

plusConstPtr :: forall p a b. APtr p => p a -> Int -> ConstPtr b
plusConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (plusPtr @a @b)
{-# inline plusConstPtr #-}

alignAPtr :: forall p a. APtr p => p a -> Int -> p a
alignAPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (alignPtr @a)
{-# inline alignAPtr #-}

alignConstPtr :: forall p a. APtr p => p a -> Int -> ConstPtr a
alignConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (alignPtr @a)
{-# inline alignConstPtr #-}

minusAPtr :: forall p q a b. (APtr p, APtr q) => p a -> q b -> Int
minusAPtr = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @q @b) $ coerce (minusPtr @a @b)
{-# inline minusAPtr #-}
