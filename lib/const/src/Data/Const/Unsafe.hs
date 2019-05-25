{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language UndecidableSuperClasses #-}
#ifndef HLINT
{-# language FunctionalDependencies #-}
#endif
{-# language QuantifiedConstraints #-}
{-# language MultiParamTypeClasses #-}
{-# language AllowAmbiguousTypes #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language ConstraintKinds #-}
{-# language TypeOperators #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language PolyKinds #-}
module Data.Const.Unsafe
  ( Constable
  , ConstPtr(..)
  , ConstForeignPtr(..)
  , ConstArray(..)
  , ConstByteArray(..)
  , ConstPrimArray(..)
  , ConstMutVar(..)
  , ConstIORef(..)
  , ConstSTRef(..)
  , constant, unsafeConstantCoercion -- note, using the symmetry of this coercion is dangerous
  , APtr, unsafePtr, unsafePtrCoercion
  , AForeignPtr, unsafeForeignPtr, unsafeForeignPtrCoercion
  , AnArray, unsafeArray, unsafeArrayCoercion
  , AByteArray, unsafeByteArray, unsafeByteArrayCoercion
  , APrimArray, unsafePrimArray, unsafePrimArrayCoercion
  , AMutVar, unsafeMutVar, unsafeMutVarCoercion
  , AnIORef, unsafeIORef, unsafeIORefCoercion
  , AnSTRef, unsafeSTRef, unsafeSTRefCoercion
  ) where

import Data.Coerce
import Data.Data
import Data.IORef
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.MutVar
import Data.STRef
import Data.Type.Coercion
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Control.Category
import Prelude hiding (id,(.))

#ifndef HLINT
class (Constable q q, forall a. Coercible (q a) (p a)) => Constable q p | p -> q
#endif

-- this is the safe direction
constant :: forall p a q. Constable q p => p a -> q a
constant = coerceWith (sym unsafeConstantCoercion)

unsafeConstantCoercion :: forall p a q. Constable q p => Coercion (q a) (p a)
unsafeConstantCoercion = Coercion

uncoerceWith :: Coercion a b -> b -> a
uncoerceWith = coerceWith . sym

-- * pointers

newtype ConstPtr a = ConstPtr { unsafeConstPtrPtr :: Ptr a } deriving (Eq,Ord,Show,Data,Storable)
instance Constable ConstPtr Ptr
instance Constable ConstPtr ConstPtr
type APtr = Constable ConstPtr

-- note backwards
unsafePtrCoercion :: forall p a. APtr p => Coercion (Ptr a) (p a)
unsafePtrCoercion = unsafeConstantCoercion @p @a . Coercion

unsafePtr :: forall p a. APtr p => p a -> Ptr a
unsafePtr = uncoerceWith (unsafePtrCoercion @p)

-- * foreign pointers
--
newtype ConstForeignPtr a = ConstForeignPtr { unsafeConstForeignPtrForeignPtr :: ForeignPtr a } deriving (Eq,Ord,Show,Data)
instance Constable ConstForeignPtr ForeignPtr
instance Constable ConstForeignPtr ConstForeignPtr
type AForeignPtr = Constable ConstForeignPtr

unsafeForeignPtrCoercion :: forall p a. AForeignPtr p => Coercion (ForeignPtr a) (p a)
unsafeForeignPtrCoercion = unsafeConstantCoercion @p @a . Coercion

unsafeForeignPtr :: forall p a. AForeignPtr p => p a -> ForeignPtr a
unsafeForeignPtr = coerceWith (sym (unsafeForeignPtrCoercion @p))

-- Data.Primitive.IORef

newtype ConstIORef a = ConstIORef { unsafeConstIORefIORef :: IORef a } deriving Eq
instance Constable ConstIORef IORef
instance Constable ConstIORef ConstIORef
type AnIORef = Constable ConstIORef

unsafeIORefCoercion :: forall p a. AnIORef p => Coercion (IORef a) (p a)
unsafeIORefCoercion = unsafeConstantCoercion @p @a . Coercion

unsafeIORef :: forall p a. AnIORef p => p a -> IORef a
unsafeIORef = uncoerceWith (unsafeIORefCoercion @p)

-- Data.Primitive.Array

newtype ConstArray s a = ConstArray { unsafeConstArrayMutableArray :: MutableArray s a } deriving Eq
instance Constable (ConstArray s) (MutableArray s)
instance Constable (ConstArray s) (ConstArray s)
type AnArray s = Constable (ConstArray s)

unsafeArrayCoercion :: forall s p a. AnArray s p => Coercion (MutableArray s a) (p a)
unsafeArrayCoercion = unsafeConstantCoercion @p @a . Coercion

unsafeArray :: forall s p a. AnArray s p => p a -> MutableArray s a
unsafeArray = uncoerceWith (unsafeArrayCoercion @s @p)

-- Data.Primitive.ByteArray

-- this one trickily uses 's' as the 'a' parameter above. Blech, but it works
newtype ConstByteArray s = ConstByteArray { unsafeConstByteArrayMutableByteArray :: MutableByteArray s }

instance Eq (ConstByteArray s) where
  (==) = coerce (sameMutableByteArray @s)

instance Constable ConstByteArray MutableByteArray
instance Constable ConstByteArray ConstByteArray
type AByteArray = Constable ConstByteArray

unsafeByteArrayCoercion :: forall p s. AByteArray p => Coercion (MutableByteArray s) (p s)
unsafeByteArrayCoercion = unsafeConstantCoercion @p @s. Coercion

unsafeByteArray :: forall s p. AByteArray p => p s -> MutableByteArray s
unsafeByteArray = uncoerceWith (unsafeByteArrayCoercion @p @s)

-- Data.Primitive.PrimArray

newtype ConstPrimArray s a = ConstPrimArray { unsafeConstPrimArrayMutablePrimArray :: MutablePrimArray s a }

instance Eq (ConstPrimArray s a) where
  (==) = coerce (sameMutablePrimArray @s)

instance Constable (ConstPrimArray s) (MutablePrimArray s)
instance Constable (ConstPrimArray s) (ConstPrimArray s)
type APrimArray s = Constable (ConstPrimArray s)

unsafePrimArrayCoercion :: forall s p a. APrimArray s p => Coercion (MutablePrimArray s a) (p a)
unsafePrimArrayCoercion = unsafeConstantCoercion @p @a . Coercion

unsafePrimArray :: forall s p a. APrimArray s p => p a -> MutablePrimArray s a
unsafePrimArray = uncoerceWith (unsafePrimArrayCoercion @s @p)

-- Data.Primitive.MutVar

newtype ConstMutVar s a = ConstMutVar  { unsafeConstMutVarMutVar :: MutVar s a } deriving Eq
instance Constable (ConstMutVar s) (MutVar s)
instance Constable (ConstMutVar s) (ConstMutVar s)
type AMutVar s = Constable (ConstMutVar s)

unsafeMutVarCoercion :: forall s p a. AMutVar s p => Coercion (MutVar s a) (p a)
unsafeMutVarCoercion = unsafeConstantCoercion @p @a . Coercion

unsafeMutVar :: forall s p a. AMutVar s p => p a -> MutVar s a
unsafeMutVar = uncoerceWith (unsafeMutVarCoercion @s @p)

-- Data.STRef

newtype ConstSTRef s a = ConstSTRef { unsafeConstSTRefSTRef :: STRef s a } deriving Eq
instance Constable (ConstSTRef s) (STRef s)
instance Constable (ConstSTRef s) (ConstSTRef s)
type AnSTRef s = Constable (ConstSTRef s)

unsafeSTRefCoercion :: forall s p a. AnSTRef s p => Coercion (STRef s a) (p a)
unsafeSTRefCoercion = unsafeConstantCoercion @p @a . Coercion

unsafeSTRef :: forall s p a. AnSTRef s p => p a -> STRef s a
unsafeSTRef = uncoerceWith (unsafeSTRefCoercion @s @p)
