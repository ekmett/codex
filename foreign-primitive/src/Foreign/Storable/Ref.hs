{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
module Foreign.Storable.Ref
( peekAt
, peekAtElemOff
, peekAtByteOff
, peekAtDiff
, pokeAt
, pokeAtElemOff
, pokeAtByteOff
, pokeAtDiff
) where

import Control.Monad.Primitive
import Foreign.Ptr.Diff
import Foreign.Ref
import Foreign.Ref.Unsafe
import Foreign.Storable

peekAt :: forall a p m. (PrimMonad m, ConstReference (PrimState m) p, Storable a) => p a -> m a
peekAt p = unsafeIOToPrim $ peek $ unsafeReferencePtr p

peekAtByteOff :: forall b a p m. (PrimMonad m, ConstReference (PrimState m) p, Storable b) => p a -> Int -> m b
peekAtByteOff p i = unsafeIOToPrim $ peekByteOff (unsafeReferencePtr p) i

peekAtElemOff :: forall a p m. (PrimMonad m, ConstReference (PrimState m) p, Storable a) => p a -> Int -> m a
peekAtElemOff p i = unsafeIOToPrim $ peekElemOff (unsafeReferencePtr p) i

peekAtDiff :: forall p m a b. (PrimMonad m, ConstReference (PrimState m) p, Storable b) => p a -> Diff a b -> m b
peekAtDiff p (Diff d) = peekAtByteOff p d

pokeAt :: forall a p m. (PrimMonad m, Reference (PrimState m) p, Storable a) => p a -> a -> m ()
pokeAt p = unsafeIOToPrim . poke (unsafeReferencePtr p)

pokeAtByteOff :: forall b a p m. (PrimMonad m, Reference (PrimState m) p, Storable b) => p a -> Int -> b -> m ()
pokeAtByteOff p i = unsafeIOToPrim . pokeByteOff (unsafeReferencePtr p) i

pokeAtElemOff :: forall a p m. (PrimMonad m, Reference (PrimState m) p, Storable a) => p a -> Int -> a -> m ()
pokeAtElemOff p i = unsafeIOToPrim . pokeElemOff (unsafeReferencePtr p) i

pokeAtDiff :: forall p m a b. (PrimMonad m, Reference (PrimState m) p, Storable b) => p a -> Diff a b -> b -> m ()
pokeAtDiff p (Diff d) b = pokeAtByteOff p d b

