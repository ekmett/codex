{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
{-# language BlockArguments #-}
{-# language ScopedTypeVariables #-}
{-# language BangPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Concurrent mini-adapton.
module Data.Watch
  ( Var
  , MonadWatch(..)
  , Watch
  , newVar
  , writeVar
  , modifyVar
  , modifyVar'
  , atomicModifyVar
  , atomicModifyVar'
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

type IOThunk = Thunk RealWorld

newVar :: PrimMonad m => a -> m (Var (PrimState m) a)
newVar a = stToPrim $ Var <$> newMutVar (VarState HashMap.empty a)
{-# inlinable newVar #-}

resetDeps :: PrimMonad m => Deps (PrimState m) a -> a -> m ()
resetDeps deps a = stToPrim $ traverse_ ($a) deps
{-# inline resetDeps #-}

-- | don't perform in a thunk unless you understand the layering
writeVar :: PrimMonad m => Var (PrimState m) a -> a -> m ()
writeVar (Var r) a' = stToPrim $ join $
  atomicModifyMutVar' r \ (VarState hm _) -> 
    (VarState HashMap.empty a', resetDeps hm a')
{-# inlinable writeVar #-}

-- | don't perform in a thunk unless you understand the layering
modifyVar :: PrimMonad m => Var (PrimState m) a -> (a -> a) -> m ()
modifyVar (Var r) f = stToPrim $ join $
  atomicModifyMutVar' r \(VarState deps a) -> let a' = f a in
    (VarState HashMap.empty a', resetDeps deps a')
{-# inlinable modifyVar #-}

-- | don't perform in a thunk unless you understand the layering
modifyVar' :: PrimMonad m => Var (PrimState m) a -> (a -> a) -> m ()
modifyVar' (Var r) f = stToPrim $ join $
  atomicModifyMutVar' r \(VarState deps a) -> let !a' = f a in
    (VarState HashMap.empty a', resetDeps deps a')
{-# inlinable modifyVar' #-}

-- | don't perform in a thunk unless you understand the layering
atomicModifyVar :: PrimMonad m => Var (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyVar (Var r) f = stToPrim $ join $ 
  atomicModifyMutVar' r \(VarState deps a) -> let ~(a',b) = f a in
    (VarState HashMap.empty a', b <$ resetDeps deps a')
{-# inlinable atomicModifyVar #-}

-- | don't perform in a thunk unless you understand the layering
atomicModifyVar' :: PrimMonad m => Var (PrimState m) a -> (a -> (a, b)) -> m b
atomicModifyVar' (Var r) f = stToPrim $ join $
  atomicModifyMutVar' r \(VarState deps a) -> let (a',b) = f a in
    (VarState HashMap.empty a', b <$ resetDeps deps a')
{-# inlinable atomicModifyVar' #-}

delay :: PrimMonad m => Watch (PrimState m) a -> m (Thunk (PrimState m) a)
delay = delayWith (\_ -> pure ())
{-# inlinable delay #-}

delayWith :: PrimMonad m => (a -> ST (PrimState m) ()) -> Watch (PrimState m) a -> m (Thunk (PrimState m) a)
delayWith fin m = stToPrim do
  u <- unsafeIOToPrim newUnique
  Thunk fin <$> newMVar u <*> newVar (Delayed m)
{-# inlinable delayWith #-}

delayWithIO :: (PrimMonad m, PrimState m ~ RealWorld) => (a -> IO ()) -> Watch RealWorld a -> m (IOThunk a)
delayWithIO fin = delayWith (primToPrim . fin)
{-# inlinable delayWithIO #-}

-- Move into 'Data.Primitive.MVar'?
withMVar :: forall m s a b. (PrimBase m, PrimState m ~ s) => MVar (PrimState m) a -> (a -> m b) -> m b
withMVar m io = unsafeIOToPrim $
  mask $ \restore -> do
    a <- unsafeSTToIO (takeMVar m :: ST s a)
    b <- restore (unsafePrimToIO $ io a) `onException` unsafeSTToIO (putMVar m a :: ST s ())
    b <$ do unsafeSTToIO (putMVar m a :: ST s ())

force :: MonadWatch m => Thunk (PrimState m) a -> m a
force (Thunk fin mu r) = do
  readVar r >>= \case -- intentionally in the MonadWatch instance to record dep.
    Forced a -> pure a
    Released -> error "entering released thunk"
    Delayed{} -> stToPrim $ withMVar mu \u -> do
      readVar r >>= \case -- unrecorded, but recorded above, probably dangerous. use primitive-unlift?
        Forced a -> pure a -- we lost a race
        Released -> error "entering released thunk"
        d@(Delayed m) -> do
          a <- runWatch m $ WatchEnv u $ atomicModifyVar' r (d,) >>= \case
             Forced a -> fin a
             _        -> pure ()
          a <$ writeVar r (Forced a)
{-# inlinable force #-}

-- after a release, the thunk must never be forced again
release :: PrimMonad m => Thunk (PrimState m) a -> m ()
release (Thunk fin mu r) = stToPrim $ do
  withMVar mu \_ ->
    atomicModifyVar' r (Released,) >>= \case
      Forced a -> fin a
      _ -> pure ()
