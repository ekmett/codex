{-# language MagicHash #-}
{-# language BlockArguments #-}
{-# language UnboxedTuples #-}
{-# language BangPatterns #-}
{-# language TypeApplications #-}
{-# language NegativeLiterals #-}
{-# language UnliftedFFITypes #-}
{-# language ScopedTypeVariables #-}
{-# language ForeignFunctionInterface #-}

-- | Note: @parsnip@ will still be assuming that the input is null terminated
-- even if you use these combinators.
module Text.Parsnip.Word8
( satisfy
, word8
, notWord8
, anyWord8
, while, whileSome
, till, tillSome, tillWord8
, skipWhile, skipWhileSome
, skipTill, skipTillSome, skipTillWord8
, previousWord8, previousWord8'
, nextWord8, nextWord8'
) where

import Data.ByteString (ByteString)
import Data.Word
import GHC.Prim
import GHC.Ptr
import GHC.Types
import GHC.Word
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Private
import Text.Parsnip.Parser

--------------------------------------------------------------------------------
-- * Word8 parsers
--------------------------------------------------------------------------------

satisfy :: (Word8 -> Bool) -> Parser s Word8
satisfy f = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c #) -> if isTrue# (0## `neWord#` c) && f (W8# c)
    then OK (W8# c) (plusAddr# p 1#) t
    else Fail p t
{-# inline satisfy #-}

notWord8 :: Word8 -> Parser s Word8
notWord8 0 = anyWord8
notWord8 (W8# c) = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c' #) -> if isTrue# (0## `neWord#` c') && isTrue# (c `neWord#` c')
    then OK (W8# c') (plusAddr# p 1#) t
    else Fail p t
{-# inline notWord8 #-}

nextWord8 :: Parser s (Maybe Word8)
nextWord8 = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c #) -> OK (if isTrue# (0## `neWord#` c) then Just (W8# c) else Nothing) p t
{-# inline nextWord8 #-}

nextWord8' :: Parser s Word8
nextWord8' = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c #) -> if isTrue# (0## `neWord#` c)
    then OK (W8# c) p t
    else Fail p t
{-# inline nextWord8' #-}

anyWord8 :: Parser s Word8
anyWord8 = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c #) -> if isTrue# (0## `neWord#` c)
    then OK (W8# c) (plusAddr# p 1#) t
    else Fail p t
{-# inline anyWord8 #-}

scan :: (Word8 -> Bool) -> Addr# -> State# s -> (# State# s, Addr# #)
scan f = go where
  go p s = case readWord8OffAddr# p 0# s of
    (# t, c #) -> if isTrue# (0## `neWord#` c) && f (W8# c)
      then (# t, p #)
      else scan f (plusAddr# p 1#) t
{-# inline scan #-}

skipWhile :: (Word8 -> Bool) -> Parser s ()
skipWhile f = Parser \p s -> case scan f p s of
  (# t, q #) -> OK () q t
{-# inline [1] skipWhile #-}

{-# RULES
"skipWhile (x/=)" forall x.
  skipWhile (x `neWord8`) = skipTillWord8 x
"skipWhile (/=x)" forall x.
  skipWhile (`neWord8` x) = skipTillWord8 x
  #-}

skipTill :: (Word8 -> Bool) -> Parser s ()
skipTill p = skipWhile (not . p)
{-# inline [1] skipTill #-}

{-# RULES
"skipTill (x==)" forall x.
  skipTill (x `eqWord8`) = skipTillWord8 x
"skipWhile (==x)" forall x.
  skipWhile (`eqWord8` x) = skipTillWord8 x
  #-}

skipTillSome :: (Word8 -> Bool) -> Parser s ()
skipTillSome p = skipWhileSome (not . p)
{-# inline skipTillSome #-}

foreign import ccall "parsnip.h" strchr0 :: Addr# -> Char# -> IO (Ptr Word8) -- lazy reimport is lazy

skipTillWord8 :: Word8 -> Parser s ()
skipTillWord8 (W8# c) = Parser $ \p s -> case io (strchr0 p (chr# (word2Int# c))) s of -- lazy cast is lazy
  (# t, Ptr q #) -> OK () q t
{-# inline skipTillWord8 #-}

skipWhileSome :: (Word8 -> Bool) -> Parser s ()
skipWhileSome p = satisfy p *> skipWhile p
{-# inline skipWhileSome #-}

while :: KnownBase s => (Word8 -> Bool) -> Parser s ByteString
while f = snipping (skipWhile f)
{-# inline while #-}

till :: KnownBase s => (Word8 -> Bool) -> Parser s ByteString
till p = snipping (skipTill p)
{-# inline till #-}

tillWord8 :: KnownBase s => Word8 -> Parser s ByteString
tillWord8 c = snipping (skipTillWord8 c)
{-# inline tillWord8 #-}

whileSome :: KnownBase s => (Word8 -> Bool) -> Parser s ByteString
whileSome p = snipping (skipWhileSome p)
{-# inline whileSome #-}

tillSome :: KnownBase s => (Word8 -> Bool) -> Parser s ByteString
tillSome p = snipping (skipTillSome p)
{-# inline tillSome #-}

-- | Peek at the previous character. Always succeeds.
previousWord8 :: forall s. KnownBase s => Parser s (Maybe Word8)
previousWord8 = case reflectBase @s of
  !(Base _ _ l _) -> Parser \p s ->
    if isTrue# (ltAddr# l p)
    then case readWord8OffAddr# p (-1#) s of
      (# t, c #) -> OK (Just (W8# c)) p t
    else OK Nothing p s
{-# inline previousWord8 #-}

-- | Peek at the previous character. Fails if we're at the start of input.
previousWord8' :: forall s. KnownBase s => Parser s Word8
previousWord8' = case reflectBase @s of
  !(Base _ _ l _) -> Parser \p s ->
    if isTrue# (ltAddr# l p)
    then case readWord8OffAddr# p (-1#) s of
      (# t, c #) -> OK (W8# c) p t
    else Fail p s
{-# inline previousWord8' #-}
