{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}
{-# language DeriveDataTypeable #-}
{-# language ConstraintKinds #-}
{-# language StandaloneDeriving #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language QuantifiedConstraints #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
--{-# options -Wno-orphans #-}

module Foreign.Ref.Unsafe
( Ref(..)
, unsafePtrToRef
, FunRef(..)
, unsafeFunPtrToFunRef
, IntRef(..)
, unsafeIntPtrToIntRef
, WordRef(..)
, unsafeWordPtrToWordRef
-- * Const-correctness
, ARef
, ConstRef(..)
, unsafeConstPtrToConstRef
, unsafeRefCoercion
, unsafeRef
, ConstReference
, constRef
, Reference
, ref
, unsafeReferenceCoercion
, unsafeReferencePtr
, unsafePtrReference
) where

import Control.Category
import Data.Bits
import Data.Coerce
import Data.Const.Unsafe
import Data.Data (Data)
import Data.Default
import Data.Primitive.Types
import Data.Type.Coercion
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.Storable
import GHC.Prim
import Prelude hiding (id,(.))

-- orphans
--deriving instance Prim IntPtr
--deriving instance Prim WordPtr 
--deriving instance Prim (IntRef s)
--deriving instance Prim (WordRef s)

newtype Ref s a = Ref { unsafeRefToPtr :: Ptr a }
 deriving stock (Data)
 deriving newtype (Eq,Ord,Show,Storable,Prim,DiffTorsor)

unsafePtrToRef :: Ptr a -> Ref s a
unsafePtrToRef = Ref
{-# inline unsafePtrToRef #-}

instance Default (Ref s a) where
  def = Ref nullPtr
  {-# inline def #-}

newtype FunRef s a = FunRef { unsafeFunRefToFunPtr :: FunPtr a }
  deriving newtype (Eq,Ord,Show,Storable,Prim,DiffTorsor)

unsafeFunPtrToFunRef :: FunPtr a -> FunRef s a
unsafeFunPtrToFunRef = FunRef
{-# inline unsafeFunPtrToFunRef #-}

newtype IntRef s = IntRef { unsafeIntRefToIntPtr :: IntPtr }
  deriving stock (Data)
  deriving newtype (Bounded,Enum,Eq,Integral,Num,Ord,Read,Real,Show,FiniteBits,Bits,Storable)

unsafeIntPtrToIntRef :: IntPtr -> IntRef s
unsafeIntPtrToIntRef = IntRef
{-# inline unsafeIntPtrToIntRef #-}

instance Default (IntRef s) where
  def = 0
  {-# inline def #-}

newtype WordRef s = WordRef { unsafeWordRefToWordPtr :: WordPtr }
  deriving stock (Data)
  deriving newtype (Bounded,Enum,Eq,Integral,Num,Ord,Read,Real,Show,FiniteBits,Bits,Storable)

unsafeWordPtrToWordRef :: WordPtr -> WordRef s
unsafeWordPtrToWordRef = WordRef
{-# inline unsafeWordPtrToWordRef #-}

instance Default (WordRef s) where
  def = 0
  {-# inline def #-}

newtype ConstRef s a = ConstRef { unsafeConstRefToRef :: Ref s a } deriving (Eq,Ord,Show,DiffTorsor)
instance Constable (ConstRef s) (Ref s)
instance Constable (ConstRef s) (ConstRef s)
type ARef s = Constable (ConstRef s) -- Ref s a or ConstRef s a

unsafeConstPtrToConstRef :: ConstPtr a -> ConstRef s a
unsafeConstPtrToConstRef (ConstPtr p) = ConstRef (Ref p)
{-# inline unsafeConstPtrToConstRef #-}

unsafeRefCoercion :: forall r a s. ARef s r => Coercion (Ref s a) (r a)
unsafeRefCoercion = unsafeConstantCoercion @r @a . Coercion

uncoerceWith :: Coercion b a -> a -> b
uncoerceWith = coerceWith . sym

-- const-incorrect
unsafeRef :: forall r a s. ARef s r => r a -> Ref s a
unsafeRef = uncoerceWith (unsafeRefCoercion @r @a @s)

class (forall a. Coercible (r a) (Ptr a)) => ConstReference s r | r -> s where
instance ConstReference s (ConstRef s)
instance ConstReference s (Ref s)
instance ConstReference RealWorld Ptr
instance ConstReference RealWorld ConstPtr

class ConstReference s r => Reference s r | r -> s
instance Reference s (Ref s)
instance Reference RealWorld Ptr

constRef :: forall r a s. ConstReference s r => r a -> ConstRef s a
constRef = coerceWith $ sym (unsafeReferenceCoercion @(ConstRef s) @a @s) . unsafeReferenceCoercion @r @a @s

ref :: forall r a s. Reference s r => r a -> Ref s a
ref = coerceWith $ sym (unsafeReferenceCoercion @(Ref s) @a) . unsafeReferenceCoercion @r @a

unsafeReferenceCoercion :: forall r a s. ConstReference s r => Coercion (r a) (Ptr a)
unsafeReferenceCoercion = Coercion

unsafeReferencePtr :: ConstReference s r => r a -> Ptr a
unsafeReferencePtr = coerce

uncoerce :: forall a b. Coercible b a => a -> b
uncoerce = coerceWith (Coercion :: Coercion a b)

unsafePtrReference :: ConstReference s r => Ptr a -> r a
unsafePtrReference = uncoerce
