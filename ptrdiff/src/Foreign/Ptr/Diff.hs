{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Foreign.Ptr.Diff
( Diff(..)
, inv
, next, prev
, (.*)
, advance
, DiffTorsor(..)
) where

import Control.Category
import Data.Coerce
import Data.Data (Data)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Prelude hiding (id,(.))

newtype Diff a b = Diff { getDiff :: Int } -- so long as minusPtr returns an Int, we're stuck with size of Int = size of Ptrdiff_t
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Category Diff where
  id = Diff 0
  Diff a . Diff b = Diff (a + b)

-- Diff is a groupoid, so @Diff a a@ is a group, and as such it forms a module over the integers

(.*) :: Int -> Diff a a -> Diff a a
n .* Diff a = Diff (n*a)

infixr 6 .*

inv :: Diff a b -> Diff b a
inv (Diff a) = Diff (negate a)
  
next :: forall a. Storable a => Diff a a
next = Diff (sizeOf @a undefined)

prev :: Storable a => Diff a a
prev = inv next

advance :: Storable a => Int -> Diff a a
advance n = n .* next

class DiffTorsor t where
  act :: t a -> Diff a b -> t b
  diff :: t b -> t a -> Diff a b

-- Ptr is a Torsor w/ structure category Diff
instance DiffTorsor Ptr where
  act = coerce plusPtr
  diff = coerce minusPtr

-- | due to finalizers this doesn't _quite_ satisfy ForeignPtr a * Diff a b <-> ForeignPtr a * ForeignPtr b
instance DiffTorsor ForeignPtr where
  act = coerce plusForeignPtr
  diff p q = diff (unsafeForeignPtrToPtr p) (unsafeForeignPtrToPtr q)
