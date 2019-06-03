{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
{-# language CPP #-}


#ifndef MIN_VERSION_StateVar
#define MIN_VERSION_StateVar(x,y,z) 1
#endif

#if !(MIN_VERSION_StateVar(1,2,0))
{-# options_ghc -Wno-orphans #-}
#endif

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
#if !(MIN_VERSION_StateVar(1,2,0))
import Control.Monad.IO.Class
#endif
import Control.Monad.ST
import Control.Monad.Primitive
import Data.IORef
import Data.Primitive.MutVar
import Data.Primitive.Ptr
import qualified Data.StateVar as Simple
import Data.STRef
import Foreign.ForeignPtr
import Foreign.Storable

#if !(MIN_VERSION_StateVar(1,2,0))
instance Storable a => Simple.HasGetter (ForeignPtr a) a where
  get p = liftIO $ withForeignPtr p get

instance Storable a => Simple.HasSetter (ForeignPtr a) a where
  p $= a = liftIO $ withForeignPtr p ($= a)

instance Storable a => Simple.HasUpdate (ForeignPtr a) a a where
  p $~ f = liftIO $ withForeignPtr p ($~ f)
  p $~! f = liftIO $ withForeignPtr p ($~! f)
#endif

------------------------------------------------------------------------------------------
-- * HasGetter
------------------------------------------------------------------------------------------

class HasGetter s a t | t -> s a where
  get :: (PrimMonad m, PrimState m ~ s) => t -> m a

instance HasGetter RealWorld a (IO a) where
  get = ioToPrim

instance HasGetter s a (ST s a) where
  get = stToPrim

instance Storable a => HasGetter RealWorld a (Ptr a) where
  get = unsafeIOToPrim . peek

instance HasGetter RealWorld a (STM a) where
  get = ioToPrim . atomically

instance HasGetter RealWorld a (TVar a) where
  get = ioToPrim . atomically . readTVar

instance HasGetter RealWorld a (IORef a) where
  get = ioToPrim . readIORef

instance Storable a => HasGetter RealWorld a (ForeignPtr a) where
  get p = unsafeIOToPrim $ withForeignPtr p get

instance HasGetter s a (STRef s a) where
  get = stToPrim . readSTRef

instance HasGetter s a (MutVar s a) where
  get = readMutVar

instance HasGetter RealWorld a (Simple.StateVar a) where
  get (Simple.StateVar g _ ) = ioToPrim g

------------------------------------------------------------------------------------------
-- * HasSetter
------------------------------------------------------------------------------------------

class HasSetter s a t | t -> s a where
  ($=) :: (PrimMonad m, PrimState m ~ s) => t -> a -> m ()

instance HasSetter RealWorld a (Simple.StateVar a) where
  Simple.StateVar _ s $= v = ioToPrim $ s v

instance HasSetter RealWorld a (Simple.SettableStateVar a) where
  Simple.SettableStateVar f $= v  = ioToPrim $ f v

instance Storable a => HasSetter RealWorld a (Ptr a) where
  p $= a = unsafeIOToPrim $ poke p a

instance HasSetter RealWorld a (IORef a) where
  p $= a = ioToPrim $ writeIORef p a

instance HasSetter RealWorld a (TVar a) where
  p $= a = ioToPrim $ atomically $ writeTVar p a

instance HasSetter s a (STRef s a) where
  p $= a = stToPrim $ writeSTRef p a

instance HasSetter s a (MutVar s a) where
  ($=) = writeMutVar

instance Storable a => HasSetter RealWorld a (ForeignPtr a) where
  p $= a = unsafeIOToPrim $ withForeignPtr p ($= a)

($=!) :: (HasSetter (PrimState m) a t, PrimMonad m) => t -> a -> m ()
p $=! a = (p $=) $! a

------------------------------------------------------------------------------------------
-- * HasUpdate
------------------------------------------------------------------------------------------

class HasSetter s b t => HasUpdate s a b t | t -> s a b where
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

instance HasUpdate RealWorld a a (Simple.StateVar a)

instance Storable a => HasUpdate RealWorld a a (Ptr a)

instance HasUpdate RealWorld a a (IORef a) where
  r $~  f = ioToPrim $ atomicModifyIORef r $ \a -> (f a, ())
  r $~! f = ioToPrim $ atomicModifyIORef' r $ \a -> (f a, ())

instance HasUpdate s a a (STRef s a)

instance HasUpdate s a a (MutVar s a)

instance HasUpdate RealWorld a a (TVar a) where
  r $~ f = ioToPrim $ atomically $ do
    a <- readTVar r
    writeTVar r (f a)
  r $~! f = ioToPrim $ atomically $ do
    a <- readTVar r
    writeTVar r $! f a

instance Storable a => HasUpdate RealWorld a a (ForeignPtr a) where
  p $~ a = unsafeIOToPrim $ withForeignPtr p ($~ a)
  p $~! a = unsafeIOToPrim $ withForeignPtr p ($~! a)

------------------------------------------------------------------------------------------
-- * SettableStateVar
------------------------------------------------------------------------------------------

data SettableStateVar s a = SettableStateVar (a -> ST s ())

instance HasSetter s a (SettableStateVar s a) where
  SettableStateVar f $= a = stToPrim $ f a

------------------------------------------------------------------------------------------
-- * StateVar
------------------------------------------------------------------------------------------

data StateVar s a = StateVar (ST s a) (a -> ST s ())

instance HasGetter s a (StateVar s a) where
  get (StateVar g _) = stToPrim g

instance HasSetter s a (StateVar s a) where
  StateVar _ s $= a = stToPrim $ s a

makeStateVar :: PrimBase m => m a -> (a -> m ()) -> StateVar (PrimState m) a
makeStateVar g s = StateVar (primToST g) (primToST . s)

mapStateVar :: (b -> a) -> (a -> b) -> StateVar s a -> StateVar s b
mapStateVar ba ab (StateVar ga sa) = StateVar (ab <$> ga) (sa . ba)
