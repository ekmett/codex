{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable<Paste>
--
module Data.Const.Primitive.MutVar
  ( ConstMutVar
  , constMutVar
  , AMutVar

  , readAMutVar
  ) where

import Control.Monad.Primitive
import Data.Coerce
import Data.Type.Coercion
import Data.Primitive.MutVar

import Data.Const.Unsafe

constMutVar :: AMutVar s p => p a -> ConstMutVar s a
constMutVar = constant
{-# inline constMutVar #-}

readAMutVar :: forall m p a. (PrimMonad m, AMutVar (PrimState m) p) => p a -> m a
readAMutVar = gcoerceWith (unsafeMutVarCoercion @(PrimState m) @p @a) $ coerce $ readMutVar @m @a
{-# inline readAMutVar #-}
