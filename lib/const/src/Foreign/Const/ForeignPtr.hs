{-# language UndecidableSuperClasses #-}
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

newConstForeignPtr :: APtr p => FinalizerPtr a -> p a -> IO (ConstForeignPtr a)
newConstForeignPtr f p = ConstForeignPtr <$> newForeignPtr f (unsafePtr p)

newConstForeignPtr_ :: APtr p => p a -> IO (ConstForeignPtr a)
newConstForeignPtr_ p = ConstForeignPtr <$> newForeignPtr_ (unsafePtr p)

addFinalizerPtr' :: AForeignPtr fp => FinalizerPtr a -> fp a -> IO ()
addFinalizerPtr' f fp = addFinalizerPtr' f (unsafeForeignPtr fp)

newConstForeignPtrEnv :: APtr p => FinalizerEnvPtr env a -> Ptr env -> p a -> IO (ConstForeignPtr a)
newConstForeignPtrEnv fep e p = ConstForeignPtr <$> newForeignPtrEnv fep e (unsafePtr p)

addForeignPtrFinalizerEnv' :: AForeignPtr fp => FinalizerEnvPtr env a -> Ptr env -> fp a -> IO ()
addForeignPtrFinalizerEnv' fep e fp = addForeignPtrFinalizerEnv fep e (unsafeForeignPtr fp)

withConstForeignPtr :: AForeignPtr fp => fp a -> (ConstPtr a -> IO r) -> IO r
withConstForeignPtr p f = withForeignPtr (unsafeForeignPtr p) (f .# ConstPtr)

finalizeForeignPtr' :: AForeignPtr fp => fp a -> IO ()
finalizeForeignPtr' fp = finalizeForeignPtr (unsafeForeignPtr fp)

touchForeignPtr' :: AForeignPtr fp => fp a -> IO ()
touchForeignPtr' fp = touchForeignPtr (unsafeForeignPtr fp)

castConstForeignPtr :: AForeignPtr fp => fp a -> ConstForeignPtr b
castConstForeignPtr = ConstForeignPtr #. castForeignPtr . unsafeForeignPtr

plusConstForeignPtr :: AForeignPtr fp => fp a -> Int -> ConstForeignPtr b
plusConstForeignPtr fp = ConstForeignPtr #. plusForeignPtr (unsafeForeignPtr fp)

-- | Analogous to Foreign.Concurrent.newForeignPtr
newConstForeignPtrConcurrent :: APtr p => p a -> IO () -> IO (ConstForeignPtr a)
newConstForeignPtrConcurrent p f = ConstForeignPtr <$> Concurrent.newForeignPtr (unsafePtr p) f

addConcurrentForeignPtrFinalizer' :: AForeignPtr fp => fp a -> IO () -> IO ()
addConcurrentForeignPtrFinalizer' fp f = Concurrent.addForeignPtrFinalizer (unsafeForeignPtr fp) f
