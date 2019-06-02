{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Foreign.Ref
( 
-- * Refs
  Ref, ConstRef
, Reference, ref, ConstReference, constRef
, pattern Null
, castRef, castARef, castConstRef
, plusRef, plusARef, plusConstRef
, alignRef, alignARef, alignConstRef
, minusRef
-- * FunRefs
, FunRef(NullFunRef)
, nullFunRef
, castFunRef
, castFunRefToRef
, castRefToFunRef
-- * IntRefs
, IntRef
, refToIntRef
, intRefToRef
-- * WordRefs
, WordRef
, refToWordRef
, wordRefToRef
-- * Constness
) where

import Data.Coerce
import Foreign.Ptr
import Foreign.Ref.Unsafe

-- * Ref

castRef :: forall a b r s. Reference s r => r a -> Ref s b
castRef = unsafePtrReference . castPtr . unsafeReferencePtr
{-# inline castRef #-}

castARef :: forall a b r s. ConstReference s r => r a -> r b
castARef = unsafePtrReference . castPtr . unsafeReferencePtr
{-# inline castARef #-}

castConstRef :: forall a b r s. ConstReference s r => r a -> ConstRef s b
castConstRef = unsafePtrReference . castPtr . unsafeReferencePtr
{-# inline castConstRef #-}

plusRef :: forall a b r s. Reference s r => r a -> Int -> Ref s b
plusRef p a = unsafePtrReference $ plusPtr (unsafeReferencePtr p) a
{-# inline plusRef #-}

plusARef :: forall a b r s. ConstReference s r => r a -> Int -> r b
plusARef  p a = unsafePtrReference $ plusPtr (unsafeReferencePtr p) a
{-# inline plusARef #-}

plusConstRef :: forall a b r s. ConstReference s r => r a -> Int -> ConstRef s b
plusConstRef p a = unsafePtrReference $ plusPtr (unsafeReferencePtr p) a
{-# inline plusConstRef #-}

alignRef :: forall a r s. Reference s r => r a -> Int -> Ref s a
alignRef p a = unsafePtrReference $ alignPtr (unsafeReferencePtr p) a
{-# inline alignRef #-}

alignARef :: forall a r s. ConstReference s r => r a -> Int -> r a
alignARef p a = unsafePtrReference $ alignPtr (unsafeReferencePtr p) a
{-# inline alignARef #-}

alignConstRef :: forall a r s. ConstReference s r => r a -> Int -> ConstRef s a
alignConstRef p a = unsafePtrReference $ alignPtr (unsafeReferencePtr p) a
{-# inline alignConstRef #-}

minusRef :: forall a b p q s. (ConstReference s p, ConstReference s q) => p a -> q b -> Int
minusRef p q = minusPtr (unsafeReferencePtr p) (unsafeReferencePtr q)
{-# inline minusRef #-}

-- * FunRef

pattern NullFunRef :: FunRef s a
pattern NullFunRef <- FunRef ((nullFunPtr==) -> True) where
  NullFunRef = FunRef nullFunPtr

nullFunRef :: FunRef s a
nullFunRef = FunRef nullFunPtr
{-# inline nullFunRef #-}

castFunRef :: FunRef s a -> FunRef s b
castFunRef = coerce
{-# inline castFunRef #-}

castFunRefToRef :: forall s a b. FunRef s a -> Ref s b
castFunRefToRef = coerce (castFunPtrToPtr @a @b)
{-# inline castFunRefToRef #-}

castRefToFunRef :: forall s a b. Ref s a -> FunRef s b
castRefToFunRef = coerce (castPtrToFunPtr @a @b)
{-# inline castRefToFunRef #-}

-- * IntRef

refToIntRef :: forall s a. Ref s a -> IntRef s
refToIntRef = coerce (ptrToIntPtr @a)
{-# inline refToIntRef #-}

intRefToRef :: forall s a. IntRef s -> Ref s a
intRefToRef = coerce (intPtrToPtr @a)
{-# inline intRefToRef #-}

-- * WordRef

refToWordRef :: forall s a. Ref s a -> WordRef s
refToWordRef = coerce (ptrToWordPtr @a)
{-# inline refToWordRef #-}

wordRefToRef :: forall s a. WordRef s -> Ref s a
wordRefToRef = coerce (wordPtrToPtr @a)
{-# inline wordRefToRef #-}

-- * Convenience constness
--
-- Works for @Ptr a@, @Ref s a@, @ConstPtr a@, @ConstRef s a@
pattern Null :: ConstReference s r => r a
pattern Null <- ((nullPtr ==) . unsafeReferencePtr -> True) where
  Null = unsafePtrReference nullPtr
