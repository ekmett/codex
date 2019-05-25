{-# language UndecidableSuperClasses #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
module Foreign.Const.ForeignPtr
  ( ConstForeignPtr
  , AForeignPtr

  -- * const foreign pointer operations

  , newConstForeignPtr
  , newConstForeignPtr_
  , castConstForeignPtr
  , plusConstForeignPtr
  , newConstForeignPtrEnv
  , withConstForeignPtr

  -- * const agnostic foreign pointer operations

  , addFinalizerPtr'
  , addForeignPtrFinalizerEnv'
  , finalizeForeignPtr'
  , touchForeignPtr'

  -- * @Foreign.Concurrent@-style finalizers

  , newConstForeignPtrConcurrent
  , addConcurrentForeignPtrFinalizer'
  ) where

import qualified Foreign.Concurrent as Concurrent
import Foreign.Const.Internal
import Foreign.Const.Ptr
import Foreign.Const.Unsafe
import Foreign.ForeignPtr
import Foreign.Ptr

newConstForeignPtr :: forall p a. APtr p => FinalizerPtr a -> p a -> IO (ConstForeignPtr a)
newConstForeignPtr f p = ConstForeignPtr <$> newForeignPtr f (unsafePtr p)

newConstForeignPtr_ :: forall p a. APtr p => p a -> IO (ConstForeignPtr a)
newConstForeignPtr_ p = ConstForeignPtr <$> newForeignPtr_ (unsafePtr p)

addFinalizerPtr' :: forall fp a. AForeignPtr fp => FinalizerPtr a -> fp a -> IO ()
addFinalizerPtr' f fp = addFinalizerPtr' f (unsafeForeignPtr fp)

newConstForeignPtrEnv :: forall p env a. APtr p => FinalizerEnvPtr env a -> Ptr env -> p a -> IO (ConstForeignPtr a)
newConstForeignPtrEnv fep e p = ConstForeignPtr <$> newForeignPtrEnv fep e (unsafePtr p)

addForeignPtrFinalizerEnv' :: forall fp env a. AForeignPtr fp => FinalizerEnvPtr env a -> Ptr env -> fp a -> IO ()
addForeignPtrFinalizerEnv' fep e fp = addForeignPtrFinalizerEnv fep e (unsafeForeignPtr fp)

withConstForeignPtr :: forall fp a r. AForeignPtr fp => fp a -> (ConstPtr a -> IO r) -> IO r
withConstForeignPtr p f = withForeignPtr (unsafeForeignPtr p) (f .# ConstPtr)

finalizeForeignPtr' :: forall fp a. AForeignPtr fp => fp a -> IO ()
finalizeForeignPtr' fp = finalizeForeignPtr (unsafeForeignPtr fp)

touchForeignPtr' :: forall fp a. AForeignPtr fp => fp a -> IO ()
touchForeignPtr' fp = touchForeignPtr (unsafeForeignPtr fp)

castConstForeignPtr :: forall fp a b. AForeignPtr fp => fp a -> ConstForeignPtr b
castConstForeignPtr = ConstForeignPtr #. castForeignPtr . unsafeForeignPtr

plusConstForeignPtr :: forall fp a b. AForeignPtr fp => fp a -> Int -> ConstForeignPtr b
plusConstForeignPtr fp = ConstForeignPtr #. plusForeignPtr (unsafeForeignPtr fp)

-- | Analogous to Foreign.Concurrent.newForeignPtr
newConstForeignPtrConcurrent :: forall p a. APtr p => p a -> IO () -> IO (ConstForeignPtr a)
newConstForeignPtrConcurrent p f = ConstForeignPtr <$> Concurrent.newForeignPtr (unsafePtr p) f

addConcurrentForeignPtrFinalizer' :: forall fp a. AForeignPtr fp => fp a -> IO () -> IO ()
addConcurrentForeignPtrFinalizer' fp f = Concurrent.addForeignPtrFinalizer (unsafeForeignPtr fp) f
