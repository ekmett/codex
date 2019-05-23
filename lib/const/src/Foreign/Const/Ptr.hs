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

import Foreign.Const.Internal
import Foreign.Const.Unsafe
import Foreign.Ptr
import Foreign.Storable

peek' :: (Storable a, APtr p) => p a -> IO a
peek' = peek . unsafePtr

peekElemOff' :: (Storable a, APtr p) => p a -> Int -> IO a
peekElemOff' = peekElemOff . unsafePtr

peekByteOff' :: (Storable a, APtr p) => p a -> Int -> IO a
peekByteOff' = peekByteOff . unsafePtr

nullConstPtr :: ConstPtr a 
nullConstPtr = ConstPtr nullPtr

castConstPtr :: APtr p => p a -> ConstPtr b
castConstPtr = ConstPtr #. castPtr . unsafePtr

plusConstPtr :: APtr p => p a -> Int -> ConstPtr a
plusConstPtr p = ConstPtr #. plusPtr (unsafePtr p)

alignConstPtr :: APtr p => p a -> Int -> ConstPtr a
alignConstPtr p = ConstPtr #. alignPtr (unsafePtr p)

minusPtr' :: (APtr p, APtr q) => p a -> q b -> Int
minusPtr' p q = minusPtr (unsafePtr p) (unsafePtr q)
