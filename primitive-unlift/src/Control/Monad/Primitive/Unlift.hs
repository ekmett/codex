{-# language RankNTypes #-}
{-# language TypeFamilies #-}
-- | Generalized 'MonadUnliftIO' for use with 'Control.Monad.Primitive'
module Control.Monad.Primitive.Unlift
( MonadUnliftPrim(..)
, UnliftPrim(..)
, unliftPrim
, askRunInPrim
, askRunInIOST
, withUnliftPrim
, toPrim
, toIOST
, wrappedWithRunInPrim
) where

import Control.Monad.IOST
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

newtype UnliftPrim m = UnliftPrim
  { unliftIOST :: forall a. m a -> IOST (PrimState m) a
  }

unliftPrim :: (PrimMonad n, PrimState m ~ PrimState n) => UnliftPrim m -> m a -> n a
unliftPrim m = primToPrim . unliftIOST m
{-# inline unliftPrim #-}

-- | Morally, this lies between PrimMonad, and PrimBase
class PrimMonad m => MonadUnliftPrim m where
  askUnliftPrim :: m (UnliftPrim m)
  askUnliftPrim = withRunInPrim (\run -> return (UnliftPrim run))
  {-# inline askUnliftPrim #-}

  withRunInPrim :: ((forall a. m a -> IOST (PrimState m) a) -> IOST (PrimState m) b) -> m b
  withRunInPrim inner = askUnliftPrim >>= \u -> primToPrim (inner (unliftPrim u))
  {-# inline withRunInPrim #-}

askRunInPrim :: (MonadUnliftPrim m, PrimMonad n, PrimState m ~ PrimState n) => m (m a -> n a)
askRunInPrim = withRunInPrim (\run -> (return (\ma -> primToPrim $ run ma)))
{-# inline askRunInPrim #-}

askRunInIOST :: MonadUnliftPrim m => m (m a -> IOST (PrimState m) a)
askRunInIOST = withRunInPrim (\run -> (return (\ma -> primToPrim $ run ma)))
{-# inline askRunInIOST #-}

withUnliftPrim :: (MonadUnliftPrim m, PrimBase n, PrimState m ~ PrimState n) => (UnliftPrim m -> n a) -> m a
withUnliftPrim inner = askUnliftPrim >>= primToPrim . inner
{-# inline withUnliftPrim #-}

withUnliftIOST :: MonadUnliftPrim m => (UnliftPrim m -> IOST (PrimState m) a) -> m a
withUnliftIOST inner = askUnliftPrim >>= primToPrim . inner
{-# inline withUnliftIOST #-}

toPrim :: (MonadUnliftPrim m, PrimMonad n, PrimState m ~ PrimState n) => m a -> m (n a)
toPrim m = primToPrim <$> toIOST m
{-# inline toPrim #-}

toIOST :: MonadUnliftPrim m => m a -> m (IOST (PrimState m) a)
toIOST m = withRunInPrim $ \run -> return $ run m
{-# inline toIOST #-}

wrappedWithRunInPrim
  :: (MonadUnliftPrim n, PrimState m ~ PrimState n)
  => (n b -> m b)
  -> (forall a. m a -> n a)
  -> ((forall a. m a -> IOST (PrimState m) a) -> IOST (PrimState m) b)
  -> m b
wrappedWithRunInPrim wrap unwrap inner = wrap $ withRunInPrim $ \run -> inner $ run . unwrap
  
instance MonadUnliftPrim IO where
  askUnliftPrim = return (UnliftPrim ioToPrim)
  {-# inline askUnliftPrim #-}
  withRunInPrim inner = primToIO $ inner ioToPrim
  {-# inline withRunInPrim #-}

instance MonadUnliftPrim (ST s) where
  askUnliftPrim = return (UnliftPrim stToPrim)
  {-# inline askUnliftPrim #-}
  withRunInPrim inner = primToST $ inner stToPrim
  {-# inline withRunInPrim #-}

instance MonadUnliftPrim (IOST s) where
  askUnliftPrim = return (UnliftPrim id)
  {-# inline askUnliftPrim #-}
  withRunInPrim inner = inner id
  {-# inline withRunInPrim #-}

instance MonadUnliftPrim m => MonadUnliftPrim (ReaderT e m) where
  askUnliftPrim = ReaderT $ \r -> withUnliftIOST $ \u -> return (UnliftPrim (unliftIOST u . flip runReaderT r))
  {-# inline askUnliftPrim #-}
  withRunInPrim inner = ReaderT $ \r -> withRunInPrim $ \run -> inner (run . flip runReaderT r)
  {-# inline withRunInPrim #-}

instance MonadUnliftPrim m => MonadUnliftPrim (IdentityT m) where
  askUnliftPrim = IdentityT $ withUnliftIOST $ \u -> return (UnliftPrim (unliftPrim u . runIdentityT))
  {-# inline askUnliftPrim #-}
  withRunInPrim inner = IdentityT $ withRunInPrim $ \run -> inner (run . runIdentityT)
  {-# inline withRunInPrim #-}
