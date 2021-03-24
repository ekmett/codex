{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language DeriveGeneric #-}
{-# language DeriveDataTypeable #-}
-- |
-- Copyright :  (c) 2019-2021 Edward Kmett
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
, peekDiffOff
, pokeDiffOff
) where

import Control.Category
import Control.Monad.IO.Class
import Data.Coerce
import Data.Data (Data)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import Prelude hiding (id,(.))

-- | So long as 'minusPtr' returns an 'Int', we're stuck with size of 'Int' = size of 'ptrdiff_t'
newtype Diff a b = Diff { getDiff :: Int } 
  deriving (Eq,Ord,Show,Read,Data,Generic)

instance Category Diff where
  id = Diff 0
  {-# inline id #-}
  Diff a . Diff b = Diff (a + b)
  {-# inline (.) #-}

-- | 'Diff' is a groupoid, so @Diff a a@ is a group, and as such it forms a module over the integers
(.*) :: Int -> Diff a a -> Diff a a
n .* Diff a = Diff (n*a)
{-# inline (.*) #-}

infixr 6 .*

-- | Invert a pointer 'Diff', e.g. convert a pointer to a field member to one that computes the location
-- of the parent, or one that computes the offset of the next item into one that computes the offset of
-- the previous element.
inv :: Diff a b -> Diff b a
inv (Diff a) = Diff (negate a)
{-# inline inv #-}

-- | Calculate the position of the next elment of a given type when they are packed in densely.
next :: forall a. Storable a => Diff a a
next = Diff (sizeOf @a undefined)
{-# inline next #-}

-- | Calculate the position of the previous element of a given type when they are packed in densely.
prev :: Storable a => Diff a a
prev = inv next
{-# inline prev #-}

-- | Advance @n@ (possibly negative) items in a dense packing.
advance :: Storable a => Int -> Diff a a
advance n = n .* next
{-# inline advance #-}

class DiffTorsor t where
  act :: Diff a b -> t a -> t b
  diff :: t b -> t a -> Diff a b

-- 'Ptr' is a torsor w/ structure category 'Diff'
instance DiffTorsor Ptr where
  act = coerce (flip plusPtr)
  diff = coerce minusPtr
  {-# inline act #-}
  {-# inline diff #-}

instance DiffTorsor FunPtr where
  act d p = castPtrToFunPtr $ act d (castFunPtrToPtr p)
  diff p q = diff (castFunPtrToPtr p) (castFunPtrToPtr q)
  {-# inline act #-}
  {-# inline diff #-}

-- | due to finalizers this doesn't _quite_ satisfy ForeignPtr a * Diff a b <-> ForeignPtr a * ForeignPtr b
instance DiffTorsor ForeignPtr where
  act = coerce (flip plusForeignPtr)
  diff p q = diff (unsafeForeignPtrToPtr p) (unsafeForeignPtrToPtr q)
  {-# inline act #-}
  {-# inline diff #-}

-- | 'peek' using a 'Diff' between pointers to calculate the offset.
peekDiffOff :: (MonadIO m, Storable b) => Ptr a -> Diff a b -> m b
peekDiffOff p (Diff d) = liftIO $ peekByteOff p d
{-# inline peekDiffOff #-}

-- | 'poke' using a 'Diff' between pointers to calculate the offset.
pokeDiffOff :: (MonadIO m, Storable b) => Ptr a -> Diff a b -> b -> m ()
pokeDiffOff p (Diff d) a = liftIO $ pokeByteOff p d a
{-# inline pokeDiffOff #-}
