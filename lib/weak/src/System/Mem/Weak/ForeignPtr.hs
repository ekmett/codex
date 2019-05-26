{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module System.Mem.Weak.ForeignPtr
  ( mkWeakForeignPtr
  , mkWeakForeignPtrPtr
  , mkWeakForeignPtrPair
  ) where

import GHC.Base
import GHC.ForeignPtr 
import GHC.Weak
import GHC.IORef
import GHC.STRef

-- | Make a 'Weak' reference from a 'ForeignPtr'. This attaches to an IORef down inside of the ForeignPtr.
mkWeakForeignPtr :: ForeignPtr k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakForeignPtr (ForeignPtr _ k) v fin = case finalizers k of
  IORef (STRef k#) -> case fin of
    Nothing       -> IO $ \s -> case mkWeakNoFinalizer# k# v s of (# s1, w #) -> (#s1, Weak w #)
    Just (IO finalizer) -> IO $ \s -> case mkWeak# k# v finalizer s of (# s1, w #) -> (#s1, Weak w #)
  where
    finalizers PlainPtr{} = error "mkWeakForeignPtr: PlainPtr encountered"
    finalizers (MallocPtr _ p) = p
    finalizers (PlainForeignPtr p) = p 
{-# inlinable mkWeakForeignPtr #-}

mkWeakForeignPtrPtr :: ForeignPtr a -> Maybe (IO ()) -> IO (Weak (ForeignPtr a))
mkWeakForeignPtrPtr k = mkWeakForeignPtr k k
{-# inlinable mkWeakForeignPtrPtr #-}

mkWeakForeignPtrPair :: ForeignPtr k -> v -> Maybe (IO ()) -> IO (Weak (ForeignPtr k, v))
mkWeakForeignPtrPair k v = mkWeakForeignPtr k (k,v)
{-# inlinable mkWeakForeignPtrPair #-}
