{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language Trustworthy #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Concurrent mini-adapton.
module Data.Watch
  ( Ref
  , MonadWatch(..)
  , Watch
  , writeRef
  , modifyRef
  , modifyRef'
  , atomicModifyRef
  , atomicModifyRef'
  , Thunk
  , IOThunk
  , delay
  , force
  ) where

import Control.Concurrent.Unique
import Control.Exception
import Control.Monad.Primitive
import Data.Foldable
import Data.HashMap.Strict as HashMap
import Data.Primitive.MutVar
import Data.Primitive.MVar
import Data.Watch.Internal

type IOThunk = Thunk RealWorld

newRef :: PrimMonad m => a -> m (Ref (PrimState m) a)
newRef a = stToPrim $ Ref . Deps
  <$> newMutVar HashMap.empty
  <*> unsafeIOToPrim newUnique
  <*> newMutVar a
{-# inlinable newRef #-}

-- don't perform in a thunk unless you understand the layering
writeRef :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef (Ref deps _ r) a = stToPrim $ do
  resetDeps deps
  writeMutVar r a
{-# inlinable writeRef #-}

modifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef (Ref deps _ r) f = stToPrim $ do
  resetDeps deps
  modifyMutVar r f
{-# inlinable modifyRef #-}

modifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef' (Ref deps _ r) f = stToPrim $ do
  resetDeps deps
  modifyMutVar' r f
{-# inlinable modifyRef' #-}

atomicModifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef (Ref deps _ r) f = stToPrim $ do
  resetDeps deps
  atomicModifyMutVar r f
{-# inlinable atomicModifyRef #-}

atomicModifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef' (Ref deps _ r) f = stToPrim $ do
  resetDeps deps
  atomicModifyMutVar' r f
{-# inlinable atomicModifyRef' #-}

resetDeps :: PrimMonad m => Deps (PrimState m) -> m ()
resetDeps (Deps deps) = do
  xs <- atomicModifyMutVar deps (HashMap.empty,)
  for_ xs $ \(Dep r) -> writeRef r Nothing
{-# inline resetDeps #-}
    
delay :: MonadWatch m => Watch (PrimState m) a -> m (Thunk (PrimState m) a)
delay m = stToPrim $ Thunk m <$> newEmptyMVar <*> newRef Nothing
{-# inlinable delay #-}

force :: MonadWatch m => Thunk (PrimState m) a -> m a
force (Thunk m mvar r@(Ref _ u mutvar)) = do
  takeMVar mvar -- I have the conch!
  readRef r >>= \case
    Just a -> pure a
    Nothing -> unsafeIOToPrim $
                 (unsafeSTToPrim $ do a <- runWatch m u (Dep r); a <$ writeMutVar mutvar (Just a))
       `finally` (unsafeSTToPrim $ putMVar mvar ())
{-# inlinable force #-}
