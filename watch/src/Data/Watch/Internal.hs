{-# language CPP #-}
{-# language ScopedTypeVariables #-}
{-# language DefaultSignatures #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language TypeFamilies #-}
{-# language StrictData #-}
{-# language GADTs #-}
{-# options_ghc -Wno-deprecations #-} -- Control.Monad.Trans.Error
{-# options_haddock not-home #-}
-- | My take on mini-adapton.
module Data.Watch.Internal
  ( Dep
  , Deps
  , Var(..)
  , VarState(..)
  , MonadWatch(..)
  , Watch(..)
  , WatchEnv(..)
  , Thunk(..)
  , ThunkState(..)
  ) where

import Control.Applicative
import Control.Concurrent.Unique
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Primitive
import Control.Monad.Primitive.Unlift
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
#if !MIN_VERSION_base(4,13,0)
import Prelude hiding (fail)
import qualified Prelude
#endif

type Dep s a = a -> ST s ()

type Deps s a = HashMap Unique (Dep s a)

data VarState s a = VarState
  { varDeps  :: !(Deps s a)
  , varValue :: a
  }

newtype Var s a = Var (MutVar s (VarState s a))

instance Eq (Var s a) where
  Var s == Var t = s == t

instance TestCoercion (Var s) where
  testCoercion (Var s :: Var s a) (Var t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise = Nothing

class PrimMonad m => MonadWatch m where
  readVar :: Var (PrimState m) a -> m a
  default readVar :: (MonadTrans t, MonadWatch n, PrimState n ~ PrimState (t n), m ~ t n) => Var (PrimState m) a -> m a
  readVar = lift . readVar
  {-# inline readVar #-}

instance MonadWatch IO where
  readVar (Var m) = varValue <$> readMutVar m
  {-# inline readVar #-}

instance MonadWatch (ST s) where
  readVar (Var m) = varValue <$> readMutVar m
  {-# inline readVar #-}

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

data WatchEnv s = WatchEnv !Unique (ST s ())

-- TODO: fuse Unique and action into one?
newtype Watch s a = Watch { runWatch :: WatchEnv s -> ST s a }
  deriving Functor

instance Applicative (Watch s) where
  pure a = Watch \_ -> pure a
  {-# inlinable pure #-}
  Watch m <*> Watch n = Watch \u -> m u <*> n u
  {-# inlinable (<*>) #-}
  Watch m *> Watch n = Watch \u -> m u *> n u
  {-# inlinable (*>) #-}
  Watch m <* Watch n = Watch \u -> m u <* n u
  {-# inlinable (<*) #-}
  liftA2 f (Watch m) (Watch n) = Watch \u -> liftA2 f (m u) (n u)
  {-# inlinable liftA2 #-}

instance Monad (Watch s) where
  Watch m >>= f = Watch \u -> do
    a <- m u
    runWatch (f a) u
  {-# inline (>>=) #-}
  Watch m >> Watch n = Watch \u -> m u >> n u
  {-# inlinable (>>) #-}

#if !MIN_VERSION_base(4,13,0)
  fail s = Watch \_ -> unsafeIOToST $ fail s
  {-# inlinable fail #-}
#endif

--instance MonadBase (ST s) (Watch s) where
--  liftBase m = Watch $ \_ -> m

instance s ~ RealWorld => MonadUnliftIO (Watch s) where
  askUnliftIO = Watch \u ->
    ioToPrim $ withUnliftIO \k ->
      return $ UnliftIO \ m ->
        unliftIO k $ stToIO $ runWatch m u
  {-# inline askUnliftIO #-}
  withRunInIO inner =
    Watch \u -> ioToPrim $ withRunInIO \run ->
      inner \m -> run $ stToIO $ runWatch m u
  {-# inline withRunInIO #-}

instance MonadUnliftPrim (Watch s) where
  askUnliftPrim = Watch \u -> do
    withUnliftIOST \k ->
      return $ UnliftPrim \ m ->
        unliftPrim k $ stToPrim $ runWatch m u
  {-# inline askUnliftPrim #-}
  withRunInPrim inner =
    Watch \u -> withRunInPrim \run ->
      inner \m -> run $ stToPrim $ runWatch m u
  {-# inline withRunInPrim #-}

instance MonadFail (Watch s) where
  fail s = Watch \_ -> unsafeIOToST $ fail s
  {-# inlinable fail #-}

instance s ~ RealWorld => MonadIO (Watch s) where
  liftIO m = Watch \_ -> unsafeIOToST m
  {-# inlinable liftIO #-}

instance PrimMonad (Watch s) where
  type PrimState (Watch s) = s
  primitive m = Watch \_ -> primitive m
  {-# inlinable primitive #-}

instance MonadWatch (Watch s) where
  readVar (Var v) = Watch \(WatchEnv u d) ->
    atomicModifyMutVar' v \(VarState hm a) ->
      (VarState (HashMap.insert u (\_ -> d) hm) a, a)
  {-# inlinable readVar #-}

data ThunkState s a
  = Released
  | Forced a
  | Delayed (Watch s a)

data Thunk s a = Thunk
  (Dep s a) -- on release / change
  (MVar s Unique) -- all writes to the ref take place through this lock?
  (Var s (ThunkState s a))

instance Eq (Thunk s a) where
  Thunk _ _ x == Thunk _ _ y = x == y

instance TestCoercion (Thunk s) where
  testCoercion (Thunk _ _ s :: Thunk s a) (Thunk _ _ t)
    | s == unsafeCoerce t = Just $ unsafeCoerce (Coercion :: Coercion a a)
    | otherwise = Nothing
