{-# Language CPP #-}
-- |
-- Copyright :  (c) 2019-2021 Edward Kmett
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

-- | Make a 'Weak' reference from a 'Foreign.ForeignPtr.ForeignPtr'. 
-- This attaches to the 'Data.IORef.IORef' down inside of the 'Foreign.ForeignPtr.ForeignPtr' that
-- holds onto the finalizers. This is safer than attaching to the 'Foreign.ForeignPtr.ForeignPtr'
-- itself.
-- 
-- However: this does not work with 'GHC.ForeignPtr.PlainPtr' or 'GHC.ForeignPtr.FinalPtr'-based
-- 'Foreign.ForeignPtr.ForeignPtr's, so make sure you know how you built your 
-- 'Foreign.ForeignPtr.ForeignPtr'.
mkWeakForeignPtr :: ForeignPtr k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakForeignPtr (ForeignPtr _ k) v fin = mkWeakIORef' (finalizers k) v fin where
  finalizers PlainPtr{} = error "mkWeakForeignPtr: PlainPtr encountered"
#if MIN_VERSION_base(4,15,0)
  finalizers FinalPtr = error "mkWeakForeignPtr: FinalPtr encountered"
#endif
  finalizers (MallocPtr _ p) = p
  finalizers (PlainForeignPtr p) = p 
{-# inlinable mkWeakForeignPtr #-}

-- | Functions like 'System.Mem.Weak.mkWeakPtr' but for 'Foreign.ForeignPtr.ForeignPtr's.
--
-- A specialised version of 'mkWeakForeignPtr', where the key and the value are the same object:
--
-- @
-- 'mkWeakForeignPtrPtr' key finalizer = 'mkWeakForeignPtr' key key finalizer
-- @
mkWeakForeignPtrPtr :: ForeignPtr a -> Maybe (IO ()) -> IO (Weak (ForeignPtr a))
mkWeakForeignPtrPtr k = mkWeakForeignPtr k k
{-# inlinable mkWeakForeignPtrPtr #-}

-- | Functions like 'System.Mem.Weak.mkWeakPair' but for 'Foreign.ForeignPtr.ForeignPtr's.
--
-- A specialised version of 'mkWeakForeignPtr' where the value
-- is actually a pair of the key and value passed to 'mkWeakForeignPtrPair':
-- @
-- 'mkWeakForeignPtrPair' key val finalizer ≡ 'mkWeakForeignPtr' key (key,val) finalizer
-- @
-- The advantage of this is that the key can be retrieved by 'deRefWeak' in addition to the value.
mkWeakForeignPtrPair :: ForeignPtr k -> v -> Maybe (IO ()) -> IO (Weak (ForeignPtr k, v))
mkWeakForeignPtrPair k v = mkWeakForeignPtr k (k,v)
{-# inlinable mkWeakForeignPtrPair #-}
