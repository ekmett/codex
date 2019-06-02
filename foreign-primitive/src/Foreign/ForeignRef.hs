{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Foreign.ForeignRef
( ForeignRef
, newForeignRef_
, withForeignRef
, finalizeForeignRef
, castForeignRef
, plusForeignRef
, mallocForeignRef
, mallocForeignRefBytes
, mallocForeignRefArray
, mallocForeignRefArray0
) where

import Control.Monad.Primitive
import Data.Coerce
import Foreign.ForeignPtr
import Foreign.ForeignRef.Unsafe
import Foreign.Ref.Unsafe
import Foreign.Storable

newForeignRef_ :: PrimMonad m => Ref (PrimState m) a -> m (ForeignRef (PrimState m) a)
newForeignRef_ (Ref p) = unsafeIOToPrim $ ForeignRef <$> newForeignPtr_ p

withForeignRef :: PrimBase m => ForeignRef (PrimState m) a -> (Ref (PrimState m) a -> m b) -> m b
withForeignRef (ForeignRef fp) k = unsafeIOToPrim $ withForeignPtr fp (unsafePrimToIO . k . Ref)

finalizeForeignRef :: PrimMonad m => ForeignRef (PrimState m) a -> m ()
finalizeForeignRef (ForeignRef fp) = unsafeIOToPrim $ finalizeForeignPtr fp

castForeignRef :: ForeignRef s a -> ForeignRef s b
castForeignRef = coerce

plusForeignRef :: forall s a b. ForeignRef s a -> Int -> ForeignRef s b
plusForeignRef = coerce (plusForeignPtr @a @b)

-- * Allocating managed memory

mallocForeignRef :: (PrimMonad m, Storable a) => m (ForeignRef (PrimState m) a)
mallocForeignRef = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtr

mallocForeignRefBytes :: PrimMonad m => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefBytes n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrBytes n

mallocForeignRefArray :: (PrimMonad m, Storable a) => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefArray n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrArray n

mallocForeignRefArray0 :: (PrimMonad m, Storable a) => Int -> m (ForeignRef (PrimState m) a)
mallocForeignRefArray0 n = unsafeIOToPrim $ ForeignRef <$> mallocForeignPtrArray0 n

