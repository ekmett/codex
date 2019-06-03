{-# language Trustworthy #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
module Control.Monad.IOST
( IOST
, iostToPrim
, primToIOST
, iostToIO
, ioToIOST
) where

import Control.Monad.IOST.Unsafe
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce

iostToPrim :: forall m a. PrimMonad m => IOST (PrimState m) a -> m a
iostToPrim = coerce (unsafeIOToPrim @m @a)

primToIOST :: forall s a. ST s a -> IOST s a
primToIOST = coerce (unsafePrimToIO @(ST s) @a)

iostToIO :: IOST RealWorld a -> IO a
iostToIO = unsafeIOSTToIO

ioToIOST :: IO a -> IOST RealWorld a
ioToIOST = IOST
