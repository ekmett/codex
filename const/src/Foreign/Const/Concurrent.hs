{-# language UndecidableSuperClasses #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
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
module Foreign.Const.Concurrent
( newConstForeignPtr
, addAForeignPtrFinalizer
) where

import Data.Coerce
import Data.Type.Coercion
import Foreign.Concurrent

import Data.Const.Unsafe

-- | Analogous to 'Concurrent.newForeignPtr'
newConstForeignPtr :: forall p a. APtr p => p a -> IO () -> IO (ConstForeignPtr a)
newConstForeignPtr = gcoerceWith (unsafePtrCoercion @p @a) $ coerce $ newForeignPtr @a
{-# inline newConstForeignPtr #-}

-- | Analogous to 'Concurrent.addForeignPtrFinalizer'
addAForeignPtrFinalizer :: forall fp a. AForeignPtr fp => fp a -> IO () -> IO ()
addAForeignPtrFinalizer = gcoerceWith (unsafeForeignPtrCoercion @fp @a) $ coerce $ addForeignPtrFinalizer @a
{-# inline addAForeignPtrFinalizer #-}
