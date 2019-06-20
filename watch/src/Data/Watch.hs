{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
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
  , newRef
  , writeRef
  , modifyRef
  , modifyRef'
  , atomicModifyRef
  , atomicModifyRef'
  , Thunk
  , IOThunk
  , delay
  , delayWith
  , delayWithIO
  , force
  , release
  ) where

import Control.Concurrent.Unique
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Foldable
import Data.HashMap.Strict as HashMap
import Data.Primitive.MutVar
import Data.Primitive.MVar
import Data.Watch.Internal
import GHC.Stack

type IOThunk = Thunk RealWorld

newRef :: PrimMonad m => a -> m (Ref (PrimState m) a)
newRef a = stToPrim $ Ref . Deps
  <$> newMutVar HashMap.empty
  <*> newMutVar a
{-# inlinable newRef #-}

-- don't perform in a thunk unless you understand the layering
writeRef :: PrimMonad m => Ref (PrimState m) a -> a -> m ()
writeRef (Ref deps r) a = stToPrim $ do
  resetDeps deps
  writeMutVar r a
{-# inlinable writeRef #-}

modifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef (Ref deps r) f = stToPrim $ do
  resetDeps deps
  modifyMutVar r f
{-# inlinable modifyRef #-}

modifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> a) -> m ()
modifyRef' (Ref deps r) f = stToPrim $ do
  resetDeps deps
  modifyMutVar' r f
{-# inlinable modifyRef' #-}

atomicModifyRef :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef (Ref deps r) f = stToPrim $ do
  resetDeps deps
  atomicModifyMutVar r f
{-# inlinable atomicModifyRef #-}

atomicModifyRef' :: PrimMonad m => Ref (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyRef' (Ref deps r) f = stToPrim $ do
  resetDeps deps
  atomicModifyMutVar' r f
{-# inlinable atomicModifyRef' #-}

resetDeps :: PrimMonad m => Deps (PrimState m) -> m ()
resetDeps (Deps deps) = do
  xs <- atomicModifyMutVar deps (HashMap.empty,)
  stToPrim $ sequence_ xs
  -- TODO: add a persistent set of deps that can be used for onChange/Future-like behavior
{-# inline resetDeps #-}

delay :: PrimMonad m => Watch (PrimState m) a -> m (Thunk (PrimState m) a)
delay = delayWith (\_ -> pure ())
{-# inlinable delay #-}

delayWith :: PrimMonad m => (a -> ST (PrimState m) ()) -> Watch (PrimState m) a -> m (Thunk (PrimState m) a)
delayWith fin m = stToPrim do
  u <- unsafeIOToPrim newUnique
  Thunk fin <$> newMVar u <*> newRef (Left m)
{-# inlinable delayWith #-}

delayWithIO :: (PrimMonad m, PrimState m ~ RealWorld) => (a -> IO ()) -> Watch RealWorld a -> m (IOThunk a)
delayWithIO fin = delayWith (primToPrim . fin)
{-# inlinable delayWithIO #-}

withMVar :: forall m s a b. (PrimBase m, PrimState m ~ s) => MVar (PrimState m) a -> (a -> m b) -> m b
withMVar m io = unsafeIOToPrim $
  mask $ \restore -> do
    a <- unsafeSTToIO (takeMVar m :: ST s a)
    b <- restore (unsafePrimToIO $ io a) `onException` unsafeSTToIO (putMVar m a :: ST s ())
    b <$ do unsafeSTToIO (putMVar m a :: ST s ())

force :: MonadWatch m => Thunk (PrimState m) a -> m a
force (Thunk fin mvar r@(Ref _ _)) = do
  readRef r >>= \case
    Right a -> pure a
    Left _ -> stToPrim $ withMVar mvar $ \u -> do
      readRef r >>= \case -- unrecorded, but recorded above
        Right a -> pure a
        Left m -> do
          a <- runWatch m u $ atomicModifyRef r (Left m,) >>= traverse_ (stToPrim . fin)
          a <$ writeRef r (Right a)
{-# inlinable force #-}

-- after release, the thunk is destroyed (at least if it was previously evaluated.)
release :: (HasCallStack, PrimMonad m) => Thunk (PrimState m) a -> m ()
release (Thunk fin mvar (Ref deps mutvar)) = stToPrim $ do
  withMVar mvar $ \u -> do
    join $ atomicModifyMutVar mutvar $ \case
      Left m -> (Left m, putMVar mvar u) -- don't destroy ourselves if we don't have to
      Right a -> (Left $ error "released", resetDeps deps *> fin a)
