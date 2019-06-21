{-# language CPP #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
module Control.Monad.IOST.Unsafe
( IOST(..)
, unsafeIOToIOST
) where

import Control.Applicative
import Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif
import Control.Monad.Fix
import Control.Monad.Primitive
import Data.Primitive.StateVar
import GHC.Prim

-- | This monad is an implementation of the 'ST' monad that is built as a newtype over 'IO'.
--
-- Why? This allows GHC's liberal @ForeignFunctionInterface@ extensions to perform foreign imports
-- where this type is involved!
newtype IOST s a = IOST { unsafeIOSTToIO :: IO a }
  deriving (Monad,Functor,MonadFix,MonadFail,Applicative,MonadPlus,Alternative,Semigroup,Monoid)

instance PrimMonad (IOST s) where
  type PrimState (IOST s) = s
  primitive = unsafeCoerce#

instance PrimBase (IOST s) where
  internal = unsafeCoerce#

instance HasGetter s a (IOST s a) where
  get = primToPrim

unsafeIOToIOST :: IO a -> IOST s a
unsafeIOToIOST = IOST
