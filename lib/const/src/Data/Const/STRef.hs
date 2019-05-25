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

module Data.Const.STRef
  ( ConstSTRef
  , constSTRef
  , AnSTRef
  , readAnSTRef
  ) where

import Control.Monad.ST
import Data.Coerce
import Data.STRef
import Data.Type.Coercion

import Data.Const.Unsafe

constSTRef :: AnSTRef s p => p a -> ConstSTRef s a
constSTRef = constant
{-# inline constSTRef #-}

readAnSTRef :: forall s p a. AnSTRef s p => p a -> ST s a
readAnSTRef = gcoerceWith (unsafeSTRefCoercion @s @p @a) $ coerce $ readSTRef @s @a
{-# inline readAnSTRef #-}
