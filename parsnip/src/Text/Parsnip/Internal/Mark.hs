{-# language MagicHash #-}
{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
{-# language PatternSynonyms #-}
{-# language BlockArguments #-}
{-# language BangPatterns #-}
{-# language UnboxedTuples #-}
module Text.Parsnip.Internal.Mark
( Mark(Mark,Mk)
, minusMark
, mark, release
, snip, snipping
) where

import Data.ByteString
import Data.Word
import GHC.Arr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Private
 
---------------------------------------------------------------------------------------
-- * Marks
---------------------------------------------------------------------------------------

newtype Mark s = Mark (Ptr Word8) -- unexposed, so known valid addresses
  deriving (Eq,Ord,Show)

pattern Mk :: Addr# -> Mark s
pattern Mk a = Mark (Ptr a)
{-# complete Mk #-} -- if only...

instance KnownBase s => Bounded (Mark s) where
  minBound = case reflectBase @s of
    !(Base _ _ l _) -> Mk l
  maxBound = case reflectBase @s of
    !(Base _ _ _ h) -> Mk h

instance KnownBase s => Enum (Mark s) where
  fromEnum p = minusMark p minBound
  toEnum = case reflectBase @s of
    !(Base _ _ l h) -> \(I# i) -> if isTrue# (0# <=# i) && isTrue# (i <=# minusAddr# h l)
      then Mk (plusAddr# l i)
      else error "Mark.toEnum: Out of bounds"
  succ = case reflectBase @s of
    !(Base _ _ _ h) -> \(Mk p) -> if isTrue# (ltAddr# p h)
      then Mk (plusAddr# p 1#)
      else error "Mark.succ: Out of bounds"
  pred = case reflectBase @s of
    !(Base _ _ l _) -> \(Mk p) -> if isTrue# (ltAddr# l p) 
      then Mk (plusAddr# p (negateInt# 1#))
      else error "Mark.pred: Out of bounds"
  enumFrom = case reflectBase @s of
    !(Base _ _ _ h) -> \(Mk p) -> ptrs1 p h
  enumFromTo (Mk p) (Mk q) = ptrs1 p q
  enumFromThen = case reflectBase @s of
    !(Base _ _ l h) -> \(Mk p) (Mk q) -> if isTrue# (gtAddr# p q)
      then dptrs p (minusAddr# q p) l
      else ptrs p (minusAddr# q p) h
  enumFromThenTo (Mk p) (Mk q) (Mk r) = if isTrue# (gtAddr# p q) 
    then dptrs p (minusAddr# q p) r
    else ptrs p (minusAddr# q p) r

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

-- | Record the current position
mark :: Parser s (Mark s)
mark = Parser \p s -> OK (Mk p) p s

-- | Return to a previous location.
release :: Mark s -> Parser s ()
release (Mk q) = Parser \_ s -> OK () q s

-- | To grab all the text covered by a given parser, consider using @snipping@
-- and applying it to a combinator simply recognizes the content rather than returns
-- it. 'snipping' a 'ByteString' is significantly cheaper than assembling one from
-- smaller fragments.
snip :: forall s. KnownBase s => Mark s -> Mark s -> Parser s ByteString
snip = case reflectBase @s of
  !(Base x g _ _) -> \(Mk i) (Mk j) -> Parser \p s -> OK (mkBS x g (minusAddr# i j)) p s

snipping :: forall s a. KnownBase s => Parser s a -> Parser s ByteString
snipping = case reflectBase @s of
  !(Base b g r _) -> \(Parser m) -> Parser \p s -> case m p s of
    (# o, q, t #) -> (# setOption (mkBS (b `plusAddr#` minusAddr# p r) g (minusAddr# q p)) o, q, t #)

