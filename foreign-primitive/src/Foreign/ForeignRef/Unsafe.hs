{-# language DerivingStrategies #-}
{-# language DeriveDataTypeable #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language GeneralizedNewtypeDeriving #-}
module Foreign.ForeignRef.Unsafe
( ForeignRef(..)
, unsafeForeignPtrToForeignRef
, unsafeForeignRefToRef
, FinalizerRef
, FinalizerEnvRef
, newForeignRef
, newForeignRefEnv
, addForeignRefFinalizer
, addForeignRefFinalizerEnv
, newForeignRefConcurrent
, addForeignRefFinalizerConcurrent
) where

import Control.Monad.IOST
import Control.Monad.Primitive
import Data.Coerce
import Data.Data (Data)
import qualified Foreign.Concurrent as Concurrent
import Foreign.Ref.Unsafe
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe

-- TODO: ConstForeignRef

newtype ForeignRef s a = ForeignRef { unsafeForeignRefToForeignPtr :: ForeignPtr a }
  deriving stock Data
  deriving newtype (Eq,Ord,Show)

unsafeForeignPtrToForeignRef :: ForeignPtr a -> ForeignRef s a
unsafeForeignPtrToForeignRef = ForeignRef
{-# inline unsafeForeignPtrToForeignRef #-}

unsafeForeignRefToRef :: forall s a. ForeignRef s a -> Ref s a
unsafeForeignRefToRef = coerce (unsafeForeignPtrToPtr @a)
{-# inline unsafeForeignRefToRef #-}

-- * Behavior that depends on the garbage collector.

type FinalizerRef s a = FunRef s (Ref s a -> IOST s ())

type FinalizerEnvRef s env a = FunRef s (Ref s env -> Ref s a -> IOST s ())

newForeignRef :: PrimMonad m => FinalizerRef (PrimState m) a -> Ref (PrimState m) a -> m (ForeignRef (PrimState m) a)
newForeignRef (FunRef finalizer) (Ref p) = unsafeIOToPrim $ ForeignRef <$> newForeignPtr (castFunPtr finalizer) p

newForeignRefEnv :: PrimMonad m => FinalizerEnvRef (PrimState m) env a -> Ref (PrimState m) env -> Ref (PrimState m) a -> m (ForeignRef (PrimState m) a)
newForeignRefEnv (FunRef finalizer) (Ref env) (Ref p) = unsafeIOToPrim $ ForeignRef <$> newForeignPtrEnv (castFunPtr finalizer) env p

addForeignRefFinalizer :: PrimMonad m => FinalizerRef (PrimState m) a -> ForeignRef (PrimState m) a -> m ()
addForeignRefFinalizer (FunRef finalizer) (ForeignRef fp) = unsafeIOToPrim $ addForeignPtrFinalizer (castFunPtr finalizer) fp

addForeignRefFinalizerEnv :: PrimMonad m => FinalizerRef (PrimState m) a -> Ref s env -> ForeignRef (PrimState m) a -> m ()
addForeignRefFinalizerEnv (FunRef finalizer) (Ref env) (ForeignRef fp) = unsafeIOToPrim $ addForeignPtrFinalizerEnv (castFunPtr finalizer) env fp

-- * Behavior that _really_ depends on the garbage collector.

newForeignRefConcurrent :: PrimBase m => Ref (PrimState m) a -> m () -> m (ForeignRef (PrimState m) a)
newForeignRefConcurrent (Ref p) finalizer = unsafeIOToPrim $ ForeignRef <$> Concurrent.newForeignPtr p (unsafePrimToIO finalizer)

addForeignRefFinalizerConcurrent :: PrimBase m => ForeignRef (PrimState m) a -> m () -> m ()
addForeignRefFinalizerConcurrent (ForeignRef fp) finalizer = unsafeIOToPrim $ Concurrent.addForeignPtrFinalizer fp (unsafePrimToIO finalizer)
