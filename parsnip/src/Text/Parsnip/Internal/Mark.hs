{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PatternSynonyms #-}
module Text.Parsnip.Internal.Mark
( Mark(Mark,Mk)
, minusMark
) where

import Data.Word
import GHC.Arr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import Text.Parsnip.Internal.Reflection
 
---------------------------------------------------------------------------------------
-- * Marks
---------------------------------------------------------------------------------------

newtype Mark s = Mark (Ptr Word8) -- unexposed, so known valid addresses
  deriving (Eq,Ord,Show)

pattern Mk :: Addr# -> Mark s
pattern Mk a = Mark (Ptr a)
{-# complete Mk #-}

instance ReifiesBase s => Bounded (Mark s) where
  minBound = Mk l where Base _ _ l _ = reflectBase @s
  maxBound = Mk h where Base _ _ _ h = reflectBase @s

instance ReifiesBase s => Enum (Mark s) where
  fromEnum p = minusMark p minBound
  toEnum (I# i)
    | isTrue# (0# <=# i) && isTrue# (i <=# minusAddr# h l) = Mk (plusAddr# l i)
    | otherwise = error "Mark.toEnum: Out of bounds"
    where Base _ _ l h = reflectBase @s
  succ (Mk p)
    | isTrue# (ltAddr# p h) = Mk (plusAddr# p 1#)
    | otherwise = error "Mark.succ: Out of bounds"
    where Base _ _ _ h = reflectBase @s
  pred (Mk p)
    | isTrue# (ltAddr# l p) = Mk (plusAddr# p (negateInt# 1#))
    | otherwise = error "Mark.pred: Out of bounds"
    where Base _ _ l _ = reflectBase @s
  enumFrom (Mk p) = ptrs1 p h
    where Base _ _ l h = reflectBase @s
  enumFromTo (Mk p) (Mk q) = ptrs1 p q
  enumFromThen (Mk p) (Mk q)
    | isTrue# (gtAddr# p q) = dptrs p (minusAddr# q p) l
    | otherwise = ptrs p (minusAddr# q p) h
    where Base _ _ l h = reflectBase @s
  enumFromThenTo (Mk p) (Mk q) (Mk r)
    | isTrue# (gtAddr# p q) = dptrs p (minusAddr# q p) r
    | otherwise = ptrs p (minusAddr# q p) r

instance Ix (Mark s) where
  range (Mk p, Mk q) = ptrs1 p q
  unsafeIndex (p,_) r = minusMark r p
  inRange (Mk p, Mk q) (Mk r) = isTrue# (leAddr# p r) && isTrue# (leAddr# r q)
  unsafeRangeSize = uncurry minusMark

ptrs1 :: Addr# -> Addr# -> [Mark s]
ptrs1 l h
  | isTrue# (leAddr# l h) = Mk l : ptrs1 (plusAddr# l 1#) h
  | otherwise = []

ptrs :: Addr# -> Int# -> Addr# -> [Mark s]
ptrs l d h
  | isTrue# (leAddr# l h) = Mk l : ptrs (plusAddr# l d) d h
  | otherwise = []

dptrs :: Addr# -> Int# -> Addr# -> [Mark s]
dptrs h d l
  | isTrue# (leAddr# l h) = Mark (Ptr h) : ptrs (plusAddr# h d) d l
  | otherwise = []

minusMark :: Mark s -> Mark s -> Int
minusMark (Mk p) (Mk q) = I# (minusAddr# p q)
