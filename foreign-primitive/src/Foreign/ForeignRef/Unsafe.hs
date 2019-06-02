{-# language DerivingStrategies #-}
{-# language DeriveDataTypeable #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language ViewPatterns #-}
{-# language MultiParamTypeClasses #-}
{-# language ConstraintKinds #-}
{-# language FunctionalDependencies #-}
{-# language QuantifiedConstraints #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module Foreign.ForeignRef.Unsafe
( ForeignRef(..)
, unsafeForeignPtrToForeignRef
, unsafeForeignRefToRef
, FinalizerRef
, FinalizerEnvRef
, newForeignRef
, newConstForeignRef
, newForeignRefEnv
, newConstForeignRefEnv
, addForeignRefFinalizer
, addForeignRefFinalizerEnv
, newForeignRefConcurrent
, newConstForeignRefConcurrent
, addForeignRefFinalizerConcurrent
, AForeignRef
, ConstForeignRef(..)
, unsafeConstForeignPtrToConstForeignRef
, unsafeForeignRefCoercion
, unsafeForeignRef
, ConstForeignReference
, constForeignRef
, ForeignReference 
, foreignRef
, unsafeForeignReferenceCoercion
, unsafeForeignReferencePtr
, unsafeForeignPtrReference
) where

import Control.Category
import Control.Monad.IOST
import Control.Monad.Primitive
import Data.Coerce
import Data.Const.Unsafe
import Data.Data (Data)
import Data.Type.Coercion
import qualified Foreign.Concurrent as Concurrent
import Foreign.Const.ForeignPtr
import Foreign.Ref.Unsafe
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Prelude hiding (id,(.))

-- TODO: ConstForeignRef

newtype ForeignRef s a = ForeignRef { unsafeForeignRefToForeignPtr :: ForeignPtr a }
  deriving stock Data
  deriving newtype (Eq,Ord,Show,DiffTorsor)

unsafeForeignPtrToForeignRef :: ForeignPtr a -> ForeignRef s a
unsafeForeignPtrToForeignRef = ForeignRef
{-# inline unsafeForeignPtrToForeignRef #-}

unsafeForeignRefToRef :: forall s a. ForeignRef s a -> Ref s a
unsafeForeignRefToRef = coerce (unsafeForeignPtrToPtr @a)
{-# inline unsafeForeignRefToRef #-}

newtype ConstForeignRef s a = ConstForeignRef { unsafeConstForeignRefToForeignRef :: ForeignRef s a }
  deriving stock Data
  deriving newtype (Eq,Ord,Show,DiffTorsor)

type instance Unforeign (ForeignRef s) = Ref s
type instance Unforeign (ConstForeignRef s) = ConstRef s

instance Constable (ConstForeignRef s) (ForeignRef s)
instance Constable (ConstForeignRef s) (ConstForeignRef s)
type AForeignRef s = Constable (ConstForeignRef s)

unsafeConstForeignPtrToConstForeignRef :: ConstForeignPtr a -> ConstForeignRef s a
unsafeConstForeignPtrToConstForeignRef (ConstForeignPtr p) = ConstForeignRef (ForeignRef p)

unsafeForeignRefCoercion :: forall r a s. AForeignRef s r => Coercion (ForeignRef s a) (r a)
unsafeForeignRefCoercion = unsafeConstantCoercion @r @a . Coercion

uncoerce :: Coercible b a => a -> b
uncoerce = coerce

uncoerceWith :: Coercion b a -> a -> b
uncoerceWith = coerceWith . sym

unsafeForeignRef :: forall r a s. AForeignRef s r => r a -> ForeignRef s a
unsafeForeignRef = uncoerceWith (unsafeForeignRefCoercion @r @a @s)

class (forall a. Coercible (r a) (ForeignPtr a)) => ConstForeignReference s r | r -> s
instance ConstForeignReference s (ConstForeignRef s)
instance ConstForeignReference s (ForeignRef s)
instance ConstForeignReference RealWorld ForeignPtr
instance ConstForeignReference RealWorld ConstForeignPtr

class ConstForeignReference s r => ForeignReference s r | r -> s
instance ForeignReference s (ForeignRef s)
instance ForeignReference RealWorld ForeignPtr

constForeignRef :: forall r a s. ConstForeignReference s r => r a -> ConstForeignRef s a 
constForeignRef = coerceWith $ sym (unsafeForeignReferenceCoercion @(ConstForeignRef s) @a) . unsafeForeignReferenceCoercion @r @a @s

foreignRef :: forall r a s. ForeignReference s r => r a -> ForeignRef s a
foreignRef = coerceWith  $ sym (unsafeForeignReferenceCoercion @(ForeignRef s) @a) . unsafeForeignReferenceCoercion @r @a @s

unsafeForeignReferenceCoercion :: forall r a s. ConstForeignReference s r => Coercion (r a) (ForeignPtr a)
unsafeForeignReferenceCoercion = Coercion

unsafeForeignReferencePtr :: ConstForeignReference s r => r a -> ForeignPtr a
unsafeForeignReferencePtr = coerce

unsafeForeignPtrReference :: ConstForeignReference s r => ForeignPtr a -> r a
unsafeForeignPtrReference = uncoerce

-- * Behavior that depends on the garbage collector.

type FinalizerRef s a = FunRef s (Ref s a -> IOST s ())

type FinalizerEnvRef s env a = FunRef s (Ref s env -> Ref s a -> IOST s ())

newForeignRef :: (PrimMonad m, Reference (PrimState m) r) => FinalizerRef (PrimState m) a -> r a -> m (ForeignRef (PrimState m) a)
newForeignRef (FunRef finalizer) (unsafeReferencePtr -> p) = unsafeIOToPrim $ ForeignRef <$> newForeignPtr (castFunPtr finalizer) p

newConstForeignRef :: (PrimMonad m, ConstReference (PrimState m) r) => FinalizerRef (PrimState m) a -> r a -> m (ConstForeignRef (PrimState m) a)
newConstForeignRef (FunRef finalizer) (unsafeReferencePtr -> p) = unsafeIOToPrim $ ConstForeignRef . ForeignRef <$> newForeignPtr (castFunPtr finalizer) p

newForeignRefEnv :: (PrimMonad m, Reference (PrimState m) r) => FinalizerEnvRef (PrimState m) env a -> r env -> r a -> m (ForeignRef (PrimState m) a)
newForeignRefEnv (FunRef finalizer) (unsafeReferencePtr -> env) (unsafeReferencePtr -> p) = unsafeIOToPrim $ ForeignRef <$> newForeignPtrEnv (castFunPtr finalizer) env p

newConstForeignRefEnv :: (PrimMonad m, ConstReference (PrimState m) r) => FinalizerEnvRef (PrimState m) env a -> r env -> r a -> m (ConstForeignRef (PrimState m) a)
newConstForeignRefEnv (FunRef finalizer) (unsafeReferencePtr -> env) (unsafeReferencePtr -> p) = unsafeIOToPrim $ ConstForeignRef . ForeignRef <$> newForeignPtrEnv (castFunPtr finalizer) env p

addForeignRefFinalizer :: (PrimMonad m, ConstForeignReference (PrimState m) fr) => FinalizerRef (PrimState m) a -> fr a -> m ()
addForeignRefFinalizer (FunRef finalizer) (unsafeForeignReferencePtr -> fp) = unsafeIOToPrim $ addForeignPtrFinalizer (castFunPtr finalizer) fp

addForeignRefFinalizerEnv :: (PrimMonad m, ConstReference (PrimState m) r, ConstForeignReference (PrimState m) fr) => FinalizerRef (PrimState m) a -> r env -> fr a -> m ()
addForeignRefFinalizerEnv (FunRef finalizer) (unsafeReferencePtr -> env) (unsafeForeignReferencePtr -> fp) = unsafeIOToPrim $ addForeignPtrFinalizerEnv (castFunPtr finalizer) env fp

-- * Behavior that _really_ depends on the garbage collector.

newForeignRefConcurrent :: (PrimBase m, Reference (PrimState m) r) => r a -> m () -> m (ForeignRef (PrimState m) a)
newForeignRefConcurrent (unsafeReferencePtr -> p) finalizer = unsafeIOToPrim $ ForeignRef <$> Concurrent.newForeignPtr p (unsafePrimToIO finalizer)

newConstForeignRefConcurrent :: (PrimBase m, ConstReference (PrimState m) r) => r a -> m () -> m (ConstForeignRef (PrimState m) a)
newConstForeignRefConcurrent (unsafeReferencePtr -> p) finalizer = unsafeIOToPrim $ ConstForeignRef . ForeignRef <$> Concurrent.newForeignPtr p (unsafePrimToIO finalizer)

addForeignRefFinalizerConcurrent :: (PrimBase m, ConstForeignReference (PrimState m) fr) => fr a -> m () -> m ()
addForeignRefFinalizerConcurrent (unsafeForeignReferencePtr -> fp) finalizer = unsafeIOToPrim $ Concurrent.addForeignPtrFinalizer fp (unsafePrimToIO finalizer)
