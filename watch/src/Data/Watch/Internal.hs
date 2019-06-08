{-# language ScopedTypeVariables #-}
{-# language DefaultSignatures #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# options_ghc -Wno-deprecations #-} -- Control.Monad.Trans.Error
{-# options_haddock not-home #-}
-- | My take on mini-adapton.
module Data.Watch.Internal
  ( Dep(..)
  , Deps(..)
  , Ref(..)
  , MonadWatch(..)
  , Watch(..)
  , Thunk(..)
  ) where

import Control.Applicative
import Control.Concurrent.Unique
import Control.Monad.Fail as MonadFail
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Error
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Data.HashMap.Strict as HashMap
import Data.Primitive.MutVar
import Data.Primitive.MVar
import Data.Type.Coercion
import Unsafe.Coerce

data Dep s where
  Dep :: Ref s (Maybe a) -> Dep s

newtype Deps s = Deps (MutVar s (HashMap Unique (Dep s)))

data Ref s a = Ref (Deps s) Unique (MutVar s a)

instance Eq (Ref s a) where
  Ref _ _ s == Ref _ _ t = s == t

instance TestCoercion (Ref s) where
  testCoercion (Ref _ _ s :: Ref s a) (Ref _ _ t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise = Nothing

class PrimMonad m => MonadWatch m where
  readRef :: Ref (PrimState m) a -> m a
  default readRef :: (MonadTrans t, MonadWatch n, PrimState n ~ PrimState (t n), m ~ t n) => Ref (PrimState m) a -> m a
  readRef = lift . readRef
  {-# inline readRef #-}

instance MonadWatch IO where
  readRef (Ref _ _ m) = readMutVar m
  {-# inline readRef #-}

instance MonadWatch (ST s) where
  readRef (Ref _ _ m) = readMutVar m
  {-# inline readRef #-}

instance MonadWatch m => MonadWatch (ReaderT e m)
instance MonadWatch m => MonadWatch (Strict.StateT s m)
instance MonadWatch m => MonadWatch (Lazy.StateT s m)
instance (MonadWatch m, Monoid w) => MonadWatch (Strict.WriterT w m)
instance (MonadWatch m, Monoid w) => MonadWatch (Lazy.WriterT w m)
instance (MonadWatch m, Monoid w) => MonadWatch (Strict.RWST r w s m)
instance (MonadWatch m, Monoid w) => MonadWatch (Lazy.RWST r w s m)
instance (MonadWatch m, Monoid w) => MonadWatch (AccumT w m)
instance MonadWatch m => MonadWatch (ContT r m)
instance (MonadWatch m, Error e) => MonadWatch (ErrorT e m)
instance MonadWatch m => MonadWatch (ExceptT e m)
instance MonadWatch m => MonadWatch (IdentityT m)
instance MonadWatch m => MonadWatch (MaybeT m)

newtype Watch s a = Watch { runWatch :: Unique -> Dep s -> ST s a }
  deriving (Functor)

instance Applicative (Watch s) where
  pure a = Watch $ \ _ _ -> pure a
  {-# inlinable pure #-}
  Watch m <*> Watch n = Watch $ \u d -> m u d <*> n u d
  {-# inlinable (<*>) #-}
  Watch m *> Watch n = Watch $ \u d -> m u d *> n u d
  {-# inlinable (*>) #-}
  Watch m <* Watch n = Watch $ \u d -> m u d <* n u d
  {-# inlinable (<*) #-}
  liftA2 f (Watch m) (Watch n) = Watch $ \u d -> liftA2 f (m u d) (n u d)
  {-# inlinable liftA2 #-} 

instance Monad (Watch s) where
  Watch m >>= f = Watch $ \u d -> do
    a <- m u d 
    runWatch (f a) u d
  {-# inline (>>=) #-}
  Watch m >> Watch n = Watch $ \u d -> m u d >> n u d
  {-# inlinable (>>) #-}

  fail s = Watch $ \_ _ -> unsafeIOToST $ MonadFail.fail s
  {-# inlinable fail #-}
  
instance MonadFail (Watch s) where
  fail s = Watch $ \_ _ -> unsafeIOToST $ MonadFail.fail s
  {-# inlinable fail #-}

instance s ~ RealWorld => MonadIO (Watch s) where
  liftIO m = Watch $ \_ _ -> unsafeIOToST m
  {-# inlinable liftIO #-}

instance PrimMonad (Watch s) where
  type PrimState (Watch s) = s
  primitive m = Watch $ \_ _ -> primitive m
  {-# inlinable primitive #-}

instance MonadWatch (Watch s) where
  readRef (Ref (Deps deps) _ src) = Watch $ \ u d -> do
    modifyMutVar deps $ HashMap.insert u d
    readMutVar src
  {-# inlinable readRef #-}

data Thunk s a = Thunk (Watch s a) (MVar s ()) (Ref s (Maybe a))

instance Eq (Thunk s a) where
  Thunk _ _ x == Thunk _ _ y = x == y

instance TestCoercion (Thunk s) where
  testCoercion (Thunk _ _ s :: Thunk s a) (Thunk _ _ t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise = Nothing
