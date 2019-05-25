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
import Foreign.Const.Internal
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

castConstPtr :: APtr p => p a -> ConstPtr b
castConstPtr = ConstPtr #. castPtr . unsafePtr

plusConstPtr :: APtr p => p a -> Int -> ConstPtr a
plusConstPtr p = ConstPtr #. plusPtr (unsafePtr p)

alignConstPtr :: APtr p => p a -> Int -> ConstPtr a
alignConstPtr p = ConstPtr #. alignPtr (unsafePtr p)

minusPtr' :: forall p q a b. (APtr p, APtr q) => p a -> q b -> Int
minusPtr' = gcoerceWith (unsafePtrCoercion @p @a) $ gcoerceWith (unsafePtrCoercion @q @b) $ coerce (minusPtr @a @b)
