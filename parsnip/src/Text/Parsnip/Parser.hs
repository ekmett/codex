{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language LambdaCase #-}
module Text.Parsnip.Parser
( Parser, ReifiesBase
, parse
----------------------------
, try
, atEnd
, endOfInput
----------------------------
, breakSubstring
, drop
, drop0
, take
----------------------------
, Mark
, mark
, release
, betwixt
, input
, pos
, rest
, snip
, snipping
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.ForeignPtr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import Prelude hiding (take, drop)
import System.IO.Unsafe
import Text.Parsnip.Internal.Mark
import Text.Parsnip.Internal.Parser
import Text.Parsnip.Internal.Private
import Text.Parsnip.Internal.Reflection
import Text.Parsnip.Internal.Option
import Text.Parsnip.Internal.Result
import Text.Parsnip.Internal.Simple
import Text.Parsnip.Location

--------------------------------------------------------------------------------
-- * Combinators
--------------------------------------------------------------------------------
--
try :: Parser s a -> Parser s a
try (Parser m) = Parser $ \p s -> case m p s of
  OK a q t -> OK a q t
  Fail q t -> Fail p t

atEnd :: Parser s Bool
atEnd = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> OK (isTrue# do chr# 0# `eqChar#` c) p t

endOfInput :: Parser s ()
endOfInput = Parser \p s -> case readCharOffAddr# p 0# s of
  (# t, c #) -> (# if isTrue# do chr# 0# `eqChar#` c then Some () else None, p, t #)

take :: forall s. ReifiesBase s => Int -> Parser s ByteString
take (I# i) = Parser \p s ->
  if isTrue# (minusAddr# r p <# i)
  then Fail p s
  else OK (B.PS (ForeignPtr (b `plusAddr#` minusAddr# p q) g) 0 (I# i)) (plusAddr# p i) s
  where Base b g q r = reflectBase @s

-- | We can do this two ways, this way is O(1) but needs ReifiesBase.
drop :: forall s. ReifiesBase s => Int -> Parser s ()
drop (I# i) = Parser \p s ->
  if isTrue# (minusAddr# r p <# i)
  then Fail p s
  else OK () (plusAddr# p i) s
  where Base _ _ _ r = reflectBase @s

-- | Linear time, but no @ReifiesBase@ dependency.
drop0 :: Int -> Parser s ()
drop0 n@(I# i) = Parser \p s -> case io (c_memchr p 0 (fromIntegral n)) s of
  (# t, Ptr q #) -> if isTrue# (q `eqAddr#` nullAddr#)
    then OK () (plusAddr# p i) t
    else Fail p s

breakSubstring :: ReifiesBase s => ByteString -> Parser s ByteString
breakSubstring needle = relative \bs -> case p bs of
    (r, _) -> SimpleOK r (B.length r)
  where p = B.breakSubstring needle

--------------------------------------------------------------------------------
-- * Positioning
--------------------------------------------------------------------------------

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
snip :: forall s. ReifiesBase s => Mark s -> Mark s -> Parser s ByteString
snip (Mk i) (Mk j) = Parser \p s -> OK (mkBS x g (minusAddr# i j)) p s
  where Base x g q r = reflectBase @s

-- | @input = snip minBound maxBound@
input :: ReifiesBase s => Parser s ByteString
input = absolute \b _ -> SimpleOK b 0

-- | @rest = mark >>= \p -> snip p maxBound@
rest :: ReifiesBase s => Parser s ByteString
rest = relative \b -> SimpleOK b 0

snipping :: forall s a. ReifiesBase s => Parser s a -> Parser s ByteString
snipping (Parser m) = Parser \p s -> case m p s of
    (# o, q, t #) -> (# setOption (mkBS (b `plusAddr#` minusAddr# p r) g (minusAddr# q p)) o, q, t #)
  where Base b g r _ = reflectBase @s

-- | 'snip' is a smidge faster, and easier to type, if less fun to say.
-- 
-- The benefit of this combinator is that it is easy to come up with numbers
-- of bytes into a file, and this combinator will automatically trim the
-- result to the actual range of bytes available, whereas constructing an
-- illegal 'Mark' will error in 'toEnum'/'fromEnum'/'succ' or whatever other
-- combinator tries to produce one out of range to maintain the invariant
-- that a mark is always a well formed location in the content.
betwixt :: ReifiesBase s => Int -> Int -> Parser s ByteString
betwixt i j = absolute $ \bs _ -> SimpleOK (B.take (j - i) $ B.drop i bs) 0

-- | 'mark' is generally faster
pos :: forall s. ReifiesBase s => Parser s Int
pos = Parser \ p s -> OK (I# (minusAddr# p q)) p s where
  Base _ _ q _ = reflectBase @s
{-# inline pos #-}

--------------------------------------------------------------------------------
-- * Parsing
--------------------------------------------------------------------------------

parse :: (forall s. ReifiesBase s => Parser s a) -> ByteString -> Either Location a
parse m bs@(B.PS (ForeignPtr b g) (I# o) (I# len)) = unsafeDupablePerformIO $
  B.useAsCString bs \(Ptr p) -> -- now it is null terminated
    IO \s -> let base = Base (plusAddr# b o) g p (plusAddr# p len) in
      case runParser (withBase (\_ -> m) base proxy#) p s of
        (# m, q, t #) -> (# t, finish base q m #)

finish :: Base s -> Addr# -> Option a -> Either Location a
finish (Base b g q r) p = \case
  Some a -> Right a
  None   -> Left (location (mkBS b g (minusAddr# r q)) (I# (minusAddr# p q)))
{-# inline finish #-}

data Wrap s a = Wrap (ReifiesBase s => Proxy# s -> Parser s a)

withBase :: (ReifiesBase s => Proxy# s -> Parser s a) -> Base s -> Proxy# s -> Parser s a
withBase f x y = magicDict (Wrap f) x y
{-# inline withBase #-}
