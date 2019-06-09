{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
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

-- | Make an arbitrary 'Weak' reference from a 'IORef'.
-- Provides a more general API than the crippled one offered by @Data.IORef@
mkWeakIORef' :: IORef k -> v -> Maybe (IO ()) -> IO (Weak v)
mkWeakIORef' (IORef (STRef k#)) v = \case
  Nothing             -> IO $ \s -> case mkWeakNoFinalizer# k# v s of (# s1, w #) -> (#s1, Weak w #)
  Just (IO finalizer) -> IO $ \s -> case mkWeak# k# v finalizer s of (# s1, w #) -> (#s1, Weak w #)
{-# inlinable mkWeakIORef' #-}

mkWeakIORefPtr :: IORef k -> Maybe (IO ()) -> IO (Weak (IORef k))
mkWeakIORefPtr k = mkWeakIORef' k k
{-# inlinable mkWeakIORefPtr #-}

mkWeakIORefPair :: IORef k -> v -> Maybe (IO ()) -> IO (Weak (IORef k, v))
mkWeakIORefPair k v = mkWeakIORef' k (k,v)
{-# inlinable mkWeakIORefPair #-}
