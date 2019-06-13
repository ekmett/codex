{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}
{-# language UnliftedFFITypes #-}
{-# language BangPatterns #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language LambdaCase #-}
{-# language AllowAmbiguousTypes #-}
{-# language PolyKinds #-}
{-# language CPP #-}
module Text.Parsnip.Parser
( Parser, KnownBase
, parse
----------------------------
, try
, atEnd
, endOfInput
----------------------------
, tillSubstring
, skipTillSubstring
, skip
, skip0
, take
----------------------------
, Mark
, mark
, release
, snip
, snipping
----------------------------
, input
, pos
, betwixt
, rest
----------------------------
, loc
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Foreign.C.Types
import GHC.ForeignPtr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import Prelude hiding (take)
import Text.Parsnip.Internal.Mark
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Private
import Text.Parsnip.Internal.Simple
import Text.Parsnip.Location

--------------------------------------------------------------------------------
-- * Combinators
--------------------------------------------------------------------------------

atEnd :: Parser s Bool
atEnd = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> OK (isTrue# do chr# 0# `eqChar#` c) p t

endOfInput :: Parser s ()
endOfInput = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> (# if isTrue# do chr# 0# `eqChar#` c then Some () else None, p, t #)

take :: forall s. KnownBase s => Int -> Parser s ByteString
take = case reflectBase @s of
  !(Base b g q r) -> \(I# i) -> Parser \p s ->
    if isTrue# (minusAddr# r p <# i)
    then Fail p s
    else OK (B.PS (ForeignPtr (b `plusAddr#` minusAddr# p q) g) 0 (I# i)) (plusAddr# p i) s

-- | We can do this two ways, this way is O(1) but needs KnownBase.
skip :: forall s. KnownBase s => Int -> Parser s ()
skip = \(I# i) -> Parser \p s ->
    if isTrue# (minusAddr# r p <# i)
    then Fail p s
    else OK () (plusAddr# p i) s
  where r = end @s

-- | Linear time, but no @KnownBase@ dependency.
skip0 :: Int -> Parser s ()
skip0 n@(I# i) = Parser \p s -> case io (c_memchr p 0 (fromIntegral n)) s of
  (# t, Ptr q #) -> if isTrue# (q `eqAddr#` nullAddr#)
    then OK () (plusAddr# p i) t
    else Fail p s

tillSubstring :: KnownBase s => ByteString -> Parser s ByteString
tillSubstring needle = relative \bs -> case p bs of
    (r, _) -> SimpleOK r (B.length r)
  where p = B.breakSubstring needle

foreign import ccall unsafe "string.h" strstr :: Addr# -> Addr# -> IO (Ptr ())
foreign import ccall unsafe "string.h" strlen :: Addr# -> IO CSize

skipTillSubstring :: ByteString -> Parser s ()
skipTillSubstring bneedle = case B.length bneedle of
  0 -> pure ()
  1 -> () <$ word8 (B.unsafeHead bneedle)
  _ -> let fneedle = packForeignString bneedle
    in Parser \p s -> case io
      ( withForeignString fneedle \(Ptr cneedle)->
          strstr p cneedle >>= \q -> if q == nullPtr
            then plusPtr (Ptr p) . fromIntegral <$> strlen p
            else pure q
      ) s of (# t, Ptr r #) -> OK () r t


--skipTillSubstring :: ByteString -> Parser s ()
--skipTillSubstring needle = relative \bs -> case p bs of
--    (r, _) -> SimpleOK r (B.length r)
--  where p = B.breakSubstring needle


-- | @input = snip minBound maxBound@
input :: KnownBase s => Parser s ByteString
input = absolute \b _ -> SimpleOK b 0

-- | @rest = mark >>= \p -> snip p maxBound@
rest :: KnownBase s => Parser s ByteString
rest = relative \b -> SimpleOK b 0

-- | 'snip' is a smidge faster, easier to type, if less fun to say, and
-- doesn't need you to fiddle with explicit type application to actually
-- apply.
--
-- The benefit of this combinator is that it is easy to come up with numbers
-- of bytes into a file, and this combinator will automatically trim the
-- result to the actual range of bytes available, whereas constructing an
-- illegal 'Mark' will error in 'toEnum'/'fromEnum'/'succ' or whatever other
-- combinator tries to produce one out of range to maintain the invariant
-- that a mark is always a well formed location in the content.
betwixt :: forall s. KnownBase s => Int -> Int -> ByteString
betwixt i j = B.take (j-i) $ B.drop i $ bytes @s

-- | 'mark' is generally faster
pos :: forall s. KnownBase s => Parser s Int
pos = Parser \ p s -> OK (I# (minusAddr# p (start @s))) p s
{-# inline pos #-}

loc :: KnownBase s => Parser s Location
loc = markLocation <$> mark
{-# inline loc #-}

-- | Actually looking at one of these is pretty slow, as it has to do a linear
-- scan to figure out its line number for display.
markLocation :: forall s. KnownBase s => Mark s -> Location
markLocation (Mark (Ptr p)) = location (bytes @s) (I# (minusAddr# p (start @s)))
{-# inline markLocation #-}
