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
-- Portability: non-portable

module Data.Const.IORef
  ( ConstIORef
  , constIORef
  , AnIORef

  , readAnIORef
  ) where

import Data.Coerce
import Data.IORef
import Data.Type.Coercion

import Data.Const.Unsafe

constIORef :: AnIORef p => p a -> ConstIORef a
constIORef = constant
{-# inline constIORef #-}

readAnIORef :: forall p a. AnIORef p => p a -> IO a
readAnIORef = gcoerceWith (unsafeIORefCoercion @p @a) $ coerce $ readIORef @a
{-# inline readAnIORef #-}
