{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}

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
