{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}
module Foreign.ForeignRef
( ForeignRef
, foreignRef
, ConstForeignRef
, constForeignRef
, ForeignReference
, ConstForeignReference
, newForeignRef_
, newConstForeignRef_
, withForeignRef
, withAForeignRef
, withConstForeignRef
, finalizeForeignRef
, castForeignRef
, castAForeignRef
, castConstForeignRef
, plusForeignRef
, plusAForeignRef
, plusConstForeignRef
, mallocForeignRef
, mallocForeignRefBytes
, mallocForeignRefArray
, mallocForeignRefArray0
) where

import Control.Monad.Primitive
import Foreign.Const.ForeignPtr
import Foreign.ForeignPtr
import Foreign.ForeignRef.Unsafe
import Foreign.Ref.Unsafe
import Foreign.Storable

newForeignRef_ :: (PrimMonad m, Reference (PrimState m) r) => r a -> m (ForeignRef (PrimState m) a)
newForeignRef_ p = unsafeIOToPrim $ ForeignRef <$> newForeignPtr_ (unsafeReferencePtr p)

newConstForeignRef_ :: (PrimMonad m, ConstReference (PrimState m) r) => r a -> m (ConstForeignRef (PrimState m) a)
newConstForeignRef_ p = unsafeIOToPrim $ ConstForeignRef . ForeignRef <$> newForeignPtr_ (unsafeReferencePtr p)

withForeignRef :: (PrimBase m, ForeignReference (PrimState m) fr) => fr a -> (Ref (PrimState m) a -> m b) -> m b
withForeignRef fp k = unsafeIOToPrim $ withForeignPtr (unsafeForeignReferencePtr fp) (unsafePrimToIO . k . Ref)

withAForeignRef :: (PrimBase m, ConstForeignReference (PrimState m) fr, ConstReference (PrimState m) (Unforeign fr)) => fr a -> (Unforeign fr a -> m b) -> m b
withAForeignRef fp k = unsafeIOToPrim $ withForeignPtr (unsafeForeignReferencePtr fp) (unsafePrimToIO . k . unsafePtrReference)

withConstForeignRef :: (PrimBase m, ConstForeignReference (PrimState m) fr) => fr a -> (ConstRef (PrimState m) a -> m b) -> m b
withConstForeignRef fp k = unsafeIOToPrim $ withForeignPtr (unsafeForeignReferencePtr fp) (unsafePrimToIO . k . unsafePtrReference)

finalizeForeignRef :: (PrimMonad m, ConstForeignReference (PrimState m) fr) => fr a -> m ()
finalizeForeignRef fp = unsafeIOToPrim $ finalizeForeignPtr $ unsafeForeignReferencePtr fp

castForeignRef :: ForeignReference s fr => fr a -> ForeignRef s b
castForeignRef = unsafeForeignPtrReference . castForeignPtr . unsafeForeignReferencePtr

castAForeignRef :: ForeignReference s fr => fr a -> fr b
castAForeignRef = unsafeForeignPtrReference . castForeignPtr . unsafeForeignReferencePtr

castConstForeignRef :: ConstForeignReference s fr => fr a -> ConstForeignRef s b
castConstForeignRef = unsafeForeignPtrReference . castForeignPtr . unsafeForeignReferencePtr

plusForeignRef :: forall a b fr s. ConstForeignReference s fr => fr a -> Int -> ForeignRef s b
plusForeignRef fp i = unsafeForeignPtrReference $ plusForeignPtr (unsafeForeignReferencePtr fp) i

plusAForeignRef :: forall a b fr s. ConstForeignReference s fr => fr a -> Int -> fr b
plusAForeignRef fp i = unsafeForeignPtrReference $ plusForeignPtr (unsafeForeignReferencePtr fp) i

plusConstForeignRef :: forall a b fr s. ConstForeignReference s fr => fr a -> Int -> ConstForeignRef s b
plusConstForeignRef fp i = unsafeForeignPtrReference $ plusForeignPtr (unsafeForeignReferencePtr fp) i

-- * Allocating managed memory

mallocForeignRef :: (PrimMonad m, Storable a) => m (ForeignRef (PrimState m) a)
mallocForeignRef = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtr

mallocForeignRefBytes :: PrimMonad m => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefBytes n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrBytes n

mallocForeignRefArray :: (PrimMonad m, Storable a) => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefArray n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrArray n

mallocForeignRefArray0 :: (PrimMonad m, Storable a) => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefArray0 n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrArray0 n

