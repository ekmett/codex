{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}

module Foreign.Const.Ptr
  ( ConstPtr
  , APtr

  -- * const-agnostic operations
  , peek'
  , peekElemOff'
  , peekByteOff'
  , minusPtr'

  -- * operations returning const pointers
  --
  , nullConstPtr
  , castConstPtr
  , plusConstPtr
  , alignConstPtr
  ) where

import Data.Coerce
import Data.Type.Coercion
import Foreign.Const.Unsafe
import Foreign.Ptr
import Foreign.Storable

peek' :: forall p a. (Storable a, APtr p) => p a -> IO a
peek' = gcoerceWith (unsafePtrCoercion @p @a) (coerce (peek @a))

peekElemOff' :: forall p a. (Storable a, APtr p) => p a -> Int -> IO a
peekElemOff' = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (peekElemOff @a)

peekByteOff' :: forall p a b. (Storable a, APtr p) => p b -> Int -> IO a
peekByteOff' = gcoerceWith (unsafePtrCoercion @p @b) $ coerce (peekByteOff @a @b)

nullConstPtr :: ConstPtr a 
nullConstPtr = ConstPtr nullPtr

castConstPtr :: forall p a b. APtr p => p a -> ConstPtr b
castConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (castPtr @a @b)

plusConstPtr :: forall p a b. APtr p => p a -> Int -> ConstPtr b
plusConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (plusPtr @a @b)

alignConstPtr :: forall p a. APtr p => p a -> Int -> ConstPtr a
alignConstPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce (alignPtr @a)

minusPtr' :: forall p q a b. (APtr p, APtr q) => p a -> q b -> Int
minusPtr' = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @q @b) $ coerce (minusPtr @a @b)
