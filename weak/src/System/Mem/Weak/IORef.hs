{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019-2021 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module System.Mem.Weak.IORef
( mkWeakIORef'
, mkWeakIORefPtr
, mkWeakIORefPair
) where

import GHC.Base
import GHC.Weak
import GHC.IORef
import GHC.STRef

-- | Make an arbitrary 'Weak' reference from a 'Data.IORef.IORef'.
--
-- Provides a more general API than the crippled one offered by 'Data.IORef.mkWeakIORef'
-- to match the power of 'mkWeak'.
mkWeakIORef' :: IORef k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakIORef' (IORef (STRef k#)) v = \case
  Nothing             -> IO $ \s -> case mkWeakNoFinalizer# k# v s of (# s1, w #) -> (#s1, Weak w #)
  Just (IO finalizer) -> IO $ \s -> case mkWeak# k# v finalizer s of (# s1, w #) -> (#s1, Weak w #)
{-# inlinable mkWeakIORef' #-}

-- | Functions like 'System.Mem.Weak.mkWeakPtr' but for 'Data.IORef.IORef's.
--
-- Make an arbitrary 'Weak' reference from an 'Data.IORef.IORef' and an optional finalizer.
--
-- A specialised version of 'mkWeakIORef'' where the key and value are the same.
mkWeakIORefPtr :: IORef k -> Maybe (IO ()) -> IO (Weak (IORef k))
mkWeakIORefPtr k = mkWeakIORef' k k
{-# inlinable mkWeakIORefPtr #-}

-- | 
--
-- A specialised version of 'mkWeakIORef'' where the value is actually a pair of the
-- key and value passed to 'mkWeakIORefPair':
--
-- @
-- 'mkWeakIORefPair' key val finalizer ≡ 'mkWeakIORef'' key (key,val) finalizer
-- @
--
-- The advantage of this is that the key can be retrieved by 'deRefWeak' in addition to the value.
mkWeakIORefPair :: IORef k -> v -> Maybe (IO ()) -> IO (Weak (IORef k, v))
mkWeakIORefPair k v = mkWeakIORef' k (k,v)
{-# inlinable mkWeakIORefPair #-}
