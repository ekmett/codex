{-# language ConstraintKinds #-}
{-# language TypeFamilies #-}

-- | This should polyfill a set of more @mtl@-like types
-- until @primitive@ gets around to adding them.

module Control.Monad.Primitive.Class
( type MonadPrim
, type MonadPrimBase
) where

import Control.Monad.Primitive

type MonadPrim s m = (PrimMonad m, PrimState m ~ s)
type MonadPrimBase s m = (PrimBase m, PrimState m ~ s)
