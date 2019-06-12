{-# language MagicHash #-}
{-# language KindSignatures #-}
module Text.Parsnip.Internal.Reflection
( Base(..), bytes, ReifiesBase(..)
) where

import GHC.ForeignPtr
import GHC.Prim
import Data.ByteString.Internal
import Data.Kind
import Text.Parsnip.Internal.Private (mkBS)

data Base s = Base 
  Addr# -- the start of a valid bytestring
  ForeignPtrContents -- memory management for that bytestring
  Addr# -- the start of our null terminated copy of the bytestring
  Addr# -- the end of our null terminated copy (points to the '\0')

bytes :: Base s -> ByteString
bytes (Base b g p q) = mkBS b g (minusAddr# q p)
{-# inline bytes #-}

class ReifiesBase (s :: Type) where
  reflectBase :: Base s

