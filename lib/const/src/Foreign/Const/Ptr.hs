{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}

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
  , castConstPtr
  , plusConstPtr
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

castConstPtr :: forall p a b. APtr p => p a -> ConstPtr b
castConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (castPtr @a @b)
{-# inline castConstPtr #-}

plusConstPtr :: forall p a b. APtr p => p a -> Int -> ConstPtr b
plusConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (plusPtr @a @b)
{-# inline plusConstPtr #-}

alignConstPtr :: forall p a. APtr p => p a -> Int -> ConstPtr a
alignConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (alignPtr @a)
{-# inline alignConstPtr #-}

minusAPtr :: forall p q a b. (APtr p, APtr q) => p a -> q b -> Int
minusAPtr = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @q @b) $ coerce (minusPtr @a @b)
{-# inline minusAPtr #-}
