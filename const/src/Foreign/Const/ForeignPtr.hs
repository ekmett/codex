{-# language UndecidableSuperClasses #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language NoStarIsType #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Foreign.Const.ForeignPtr
( ConstForeignPtr
, constForeignPtr

, AForeignPtr

-- * const foreign pointer operations

, newConstForeignPtr
, newConstForeignPtr_
, castConstForeignPtr
, plusConstForeignPtr
, newConstForeignPtrEnv
, withConstForeignPtr

-- * const-preserving foreign pointer operations

, Unforeign
, plusAForeignPtr
, withAForeignPtr

-- * const agnostic foreign pointer operations

, addAForeignPtrFinalizer
, addAForeignPtrFinalizerEnv
, finalizeAForeignPtr
, touchAForeignPtr
) where

import Data.Coerce
import Data.Kind
import Data.Type.Coercion
import Foreign.Const.Ptr
import Foreign.ForeignPtr
import Foreign.Ptr

import Data.Const.Unsafe

type family Unforeign (fp :: Type -> Type) :: Type -> Type

type instance Unforeign ForeignPtr = Ptr
type instance Unforeign ConstForeignPtr = ConstPtr

constForeignPtr :: AForeignPtr fp => fp a -> ConstForeignPtr a
constForeignPtr = constant
{-# inline constForeignPtr #-}

newConstForeignPtr :: forall p a. APtr p => FinalizerPtr a -> p a -> IO (ConstForeignPtr a)
newConstForeignPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ newForeignPtr @a
{-# inline newConstForeignPtr #-}

newConstForeignPtr_ :: forall p a. APtr p => p a -> IO (ConstForeignPtr a)
newConstForeignPtr_ = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ newForeignPtr_ @a
{-# inline newConstForeignPtr_ #-}

addAForeignPtrFinalizer :: forall fp a. AForeignPtr fp => FinalizerPtr a -> fp a -> IO ()
addAForeignPtrFinalizer = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ addForeignPtrFinalizer @a
{-# inline addAForeignPtrFinalizer #-}

newConstForeignPtrEnv :: forall p env a. APtr p => FinalizerEnvPtr env a -> Ptr env -> p a -> IO (ConstForeignPtr a)
newConstForeignPtrEnv = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ newForeignPtrEnv @env @a
{-# inline newConstForeignPtrEnv #-}

addAForeignPtrFinalizerEnv :: forall fp env a. AForeignPtr fp => FinalizerEnvPtr env a -> Ptr env -> fp a -> IO ()
addAForeignPtrFinalizerEnv = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ addForeignPtrFinalizerEnv @env @a
{-# inline addAForeignPtrFinalizerEnv #-}

withConstForeignPtr :: forall fp a r. AForeignPtr fp => fp a -> (ConstPtr a -> IO r) -> IO r
withConstForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ withForeignPtr @a @r
{-# inline withConstForeignPtr #-}

finalizeAForeignPtr :: forall fp a. AForeignPtr fp => fp a -> IO ()
finalizeAForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ finalizeForeignPtr @a
{-# inline finalizeAForeignPtr #-}

touchAForeignPtr :: forall fp a. AForeignPtr fp => fp a -> IO ()
touchAForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ touchForeignPtr @a
{-# inline touchAForeignPtr #-}

castConstForeignPtr :: forall fp a b. AForeignPtr fp => fp a -> ConstForeignPtr b
castConstForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ castForeignPtr @a @b
{-# inline castConstForeignPtr #-}

plusAForeignPtr :: forall fp a b. AForeignPtr fp => fp a -> Int -> fp b
plusAForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ gcoerceWith (unsafeForeignPtrCoercion @fp @b) $ coerce $ plusForeignPtr @a @b
{-# inline plusAForeignPtr #-}

plusConstForeignPtr :: forall fp a b. AForeignPtr fp => fp a -> Int -> ConstForeignPtr b
plusConstForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ plusForeignPtr @a @b
{-# inline plusConstForeignPtr #-}

withAForeignPtr :: forall fp a r. (AForeignPtr fp, APtr (Unforeign fp)) => fp a -> (Unforeign fp a -> IO r) -> IO r
withAForeignPtr = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ gcoerceWith (unsafePtrCoercion @(Unforeign fp) @a) $ coerce $ withForeignPtr @a @r
{-# inline withAForeignPtr #-}
