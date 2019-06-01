{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}

-- | Generalizes Data.StateVar to arbitrary PrimMonads

module Data.Primitive.StateVar 
( HasGetter(..)
, HasSetter(..)
, HasUpdate(..)
, StateVar(..)
, SettableStateVar(..)
, mapStateVar
, makeStateVar
) where

import Control.Concurrent.STM
import Control.Monad.ST
import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.MutVar
-- import Data.Primitive.PrimRef
import Data.Primitive.Types
import qualified Data.StateVar as Simple
import Data.STRef
import Foreign.Storable

class HasGetter s a t | t -> a where
  get :: (PrimMonad m, PrimState m ~ s) => t -> m a

instance HasGetter RealWorld a (IO a) where
  get = ioToPrim

instance (s ~ s') => HasGetter s' a (ST s a) where
  get = stToPrim

instance Storable a => HasGetter s a (Ptr a) where
  get = unsafeIOToPrim . peek

instance s ~ RealWorld => HasGetter s a (STM a) where
  get = ioToPrim . atomically

instance s ~ RealWorld => HasGetter s a (TVar a) where
  get = ioToPrim . atomically . readTVar

instance s ~ RealWorld => HasGetter s a (IORef a) where
  get = ioToPrim . readIORef

-- instance HasGetter s a (PrimRef s a) where get = readPrimRef

instance s ~ s' => HasGetter s' a (STRef s a) where
  get = stToPrim . readSTRef

instance s ~ s' => HasGetter s' a (MutVar s a) where
  get = readMutVar

instance s ~ RealWorld => HasGetter s a (Simple.StateVar a) where
  get (Simple.StateVar g _ ) = ioToPrim g

class HasSetter s a t | t -> a where
  ($=) :: (PrimMonad m, PrimState m ~ s) => t -> a -> m ()

instance s ~ RealWorld => HasSetter s a (Simple.StateVar a) where
  Simple.StateVar _ s $= v = ioToPrim $ s v

instance s ~ RealWorld => HasSetter s a (Simple.SettableStateVar a) where
  Simple.SettableStateVar f $= v  = ioToPrim $ f v

instance Storable a => HasSetter s a (Ptr a) where
  p $= a = unsafeIOToPrim $ poke p a

instance s ~ RealWorld => HasSetter s a (IORef a) where
  p $= a = ioToPrim $ writeIORef p a

instance s ~ RealWorld => HasSetter s a (TVar a) where
  p $= a = ioToPrim $ atomically $ writeTVar p a

instance s ~ s' => HasSetter s' a (STRef s a) where
  p $= a = stToPrim $ writeSTRef p a

instance s ~ s' => HasSetter s' a (MutVar s a) where
  ($=) = writeMutVar

-- instance Prim a => HasSetter s a (PrimRef s a) where ($=) = writePrimRef

($=!) :: (HasSetter (PrimState m) a t, PrimMonad m) => t -> a -> m ()
p $=! a = (p $=) $! a

class HasSetter s b t => HasUpdate s a b t | t -> a b where
  ($~) :: (PrimMonad m, PrimState m ~ s) => t -> (a -> b) -> m ()
  default ($~) :: (PrimMonad m, PrimState m ~ s, a ~ b, HasGetter s a t) => t -> (a -> b) -> m ()
  ($~) = defaultUpdate

  ($~!) :: (PrimMonad m, PrimState m ~ s) => t -> (a -> b) -> m ()
  default ($~!) :: (PrimMonad m, PrimState m ~ s, a ~ b, HasGetter s a t) => t -> (a -> b) -> m ()
  ($~!) = defaultUpdateStrict

defaultUpdate :: (PrimMonad m, HasGetter (PrimState m) a t, HasSetter (PrimState m) a t) => t -> (a -> a) -> m ()
defaultUpdate r f = stToPrim $ do
  a <- get r
  r $= f a

defaultUpdateStrict :: (PrimMonad m, HasGetter (PrimState m) a t, HasSetter (PrimState m) a t) => t -> (a -> a) -> m ()
defaultUpdateStrict r f = stToPrim $ do
  a <- get r
  r $=! f a

instance s ~ RealWorld => HasUpdate s a a (Simple.StateVar a)
instance Storable a => HasUpdate s a a (Ptr a)
instance s ~ RealWorld => HasUpdate s a a (IORef a) where
  r $~  f = ioToPrim $ atomicModifyIORef r $ \a -> (f a, ())
  r $~! f = ioToPrim $ atomicModifyIORef' r $ \a -> (f a, ())
instance s ~ s' => HasUpdate s' a a (STRef s a)
instance s ~ s' => HasUpdate s' a a (MutVar s a)
-- instance HasUpdate s a a (PrimRef s a)
instance s ~ RealWorld => HasUpdate s a a (TVar a) where
  r $~ f = ioToPrim $ atomically $ do
    a <- readTVar r
    writeTVar r (f a)
  r $~! f = ioToPrim $ atomically $ do
    a <- readTVar r
    writeTVar r $! f a


data SettableStateVar s a = SettableStateVar (a -> ST s ())

instance s ~ s' => HasSetter s' a (SettableStateVar s a) where
  SettableStateVar f $= a = stToPrim $ f a

data StateVar s a = StateVar (ST s a) (a -> ST s ())

instance s ~ s' => HasGetter s' a (StateVar s a) where
  get (StateVar g _) = stToPrim g

instance s ~ s' => HasSetter s' a (StateVar s a) where
  StateVar _ s $= a = stToPrim $ s a

makeStateVar :: PrimBase m => m a -> (a -> m ()) -> StateVar (PrimState m) a
makeStateVar g s = StateVar (primToST g) (primToST . s)

mapStateVar :: (b -> a) -> (a -> b) -> StateVar s a -> StateVar s b
mapStateVar ba ab (StateVar ga sa) = StateVar (ab <$> ga) (sa . ba)
