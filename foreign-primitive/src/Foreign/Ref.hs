{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Foreign.Ref
( 
-- * Refs
  Ref
, nullRef
, castRef
, plusRef
, alignRef
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
, ConstRef
, Reference
, ref
, ConstReference
, pattern Null
, constRef
) where

import Data.Coerce
import Foreign.Ptr
import Foreign.Ref.Unsafe

-- * Ref

nullRef :: Ref s a
nullRef = Ref nullPtr
{-# inline nullRef #-}

castRef :: forall s a b. Ref s a -> Ref s b
castRef = coerce (castPtr @a)
{-# inline castRef #-}

plusRef :: forall s a b. Ref s a -> Int -> Ref s b
plusRef = coerce (plusPtr @a)
{-# inline plusRef #-}

alignRef :: forall s a. Ref s a -> Int -> Ref s a
alignRef = coerce (alignPtr @a)
{-# inline alignRef #-}

minusRef :: forall s a b. Ref s a -> Ref s b -> Int
minusRef = coerce (minusPtr @a @b)
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
