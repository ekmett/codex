-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module System.Mem.Weak.ForeignPtr
( mkWeakForeignPtr
, mkWeakForeignPtrPtr
, mkWeakForeignPtrPair
) where

import GHC.ForeignPtr 
import System.Mem.Weak
import System.Mem.Weak.IORef

-- | Make a 'Weak' reference from a 'ForeignPtr'. This attaches to an IORef down inside of the ForeignPtr
-- that holds onto the finalizers. This is safe than attaching to the ForeignPtr itself.
mkWeakForeignPtr :: ForeignPtr k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakForeignPtr (ForeignPtr _ k) v fin = mkWeakIORef' (finalizers k) v fin where
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
