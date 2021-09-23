{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language UnboxedSums #-}
{-# language StandaloneDeriving #-}
{-# language UnboxedTuples #-}
{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language BangPatterns #-}
{-# language ForeignFunctionInterface #-}
{-# language KindSignatures #-}
{-# language UnliftedFFITypes #-}
{-# language TypeApplications #-}
{-# language AllowAmbiguousTypes #-}
{-# language BlockArguments #-}
{-# language ViewPatterns #-}
{-# language UnboxedTuples #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}

#if __GLASGOW_HASKELL__ >= 810
{-# language UnliftedNewtypes #-}
#endif

{-# options_ghc -O2 #-}

module Text.Parsnip.Internal.Parser
(
-- * Parser
  Parser(..)
, Option
, pattern Some
, pattern None
, mapOption, setOption
, Result, pattern OK, pattern Fail
, mapResult, setResult
, try
-- * Unsafe literals
, lit, litN, word8
-- * Guts
, Base(..), bytes, start, end
, KnownBase(..)
, parse
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Internal as B
import Data.Primitive.ByteArray
import Data.String
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.ForeignPtr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import GHC.Word
import System.IO.Unsafe

import Text.Parsnip.Location
import Text.Parsnip.Internal.Private

--------------------------------------------------------------------------------
-- * Option
--------------------------------------------------------------------------------

-- | Unlifted 'Maybe'

#if __GLASGOW_HASKELL__ >= 810

newtype Option a = Option# (# a | (##) #)

pattern Some :: a -> Option a
pattern Some a = Option# (# a | #)

pattern None :: Option a
pattern None = Option# (# | (##) #)

#else

type Option a = (# a | (##) #)

pattern Some :: a -> Option a
pattern Some a = (# a | #)

pattern None :: Option a
pattern None = (# | (##) #)

#endif

{-# complete Some, None #-} -- these don't work outside this module =(

mapOption :: (a -> b) -> Option a -> Option b
mapOption f (Some a) = Some $! f a
mapOption _ None = None
{-# inline mapOption #-}

setOption :: b -> Option a -> Option b
setOption b (Some _) = Some b
setOption _ None = None
{-# inline setOption #-}

--------------------------------------------------------------------------------
-- * Result
--------------------------------------------------------------------------------

type Result s a = (# Option a, Addr#, State# s #)

pattern OK :: a -> Addr# -> State# s -> Result s a
pattern OK a p s = (# Some a, p, s #)

pattern Fail :: Addr# -> State# s -> Result s a
pattern Fail p s = (# None, p, s #)

{-# complete OK, Fail #-}

mapResult :: (a -> b) -> Result s a -> Result s b
mapResult f (# o, p, s #) = (# mapOption f o, p, s #)
{-# inline mapResult #-}

setResult :: b -> Result s a -> Result s b
setResult b (# o, p, s #) = (# setOption b o, p, s #)
{-# inline setResult #-}

--------------------------------------------------------------------------------
-- * Result
--------------------------------------------------------------------------------

newtype Parser s a = Parser
 { runParser :: Addr# -> State# s -> Result s a
 }

instance Functor (Parser s) where
  fmap f (Parser m) = Parser \ p s -> mapResult f (m p s)
  {-# inline fmap #-}
  b <$ Parser m = Parser \ p s -> case m p s of
    OK _ q t -> OK b q t
    Fail q t -> Fail q t
  {-# inline (<$) #-}

instance Applicative (Parser s) where
  pure a = Parser \ p s -> OK a p s
  {-# inline pure #-}
  Parser m <*> Parser n = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK f q t -> mapResult f (n q t)
  {-# inline (<*>) #-}
  Parser m *> Parser n = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK _ q t -> n q t
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser \p s -> case m p s of
    OK a q t -> setResult a (n q t)
    x -> x
  {-# inline (<*) #-}

instance Monad (Parser s) where
  Parser m >>= f = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK a q t -> runParser (f a) q t
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}
#if !MIN_VERSION_base(4,13,0)
  fail _ = Parser Fail
  {-# inline fail #-}
#endif

instance Alternative (Parser s) where
  Parser m <|> Parser n = Parser \ p s -> case m p s of
    Fail _ t -> n p t
    OK a q t -> OK a q t
  {-# inline (<|>) #-}
  empty = Parser Fail
  {-# inline empty #-}

instance MonadPlus (Parser s) where
  mplus = (<|>)
  {-# inline mplus #-}
  mzero = empty
  {-# inline mzero #-}

instance PrimMonad (Parser s) where
  type PrimState (Parser s) = s
  primitive f = Parser \p s -> case f s of
    (# t, a #) -> OK a p t
  {-# inline primitive #-}

-- perhaps this interface is a little low level. hrmm
instance a ~ ByteString => IsString (Parser s a) where
  fromString "" = pure B.empty
  fromString xs = Parser \p s -> case sizeofMutableByteArray# ba of
    n -> case io (c_strncmp (mutableByteArrayContents# ba) p (fromIntegral $ I# n)) s of
      (# t, i #)
        | i /= 0    -> Fail p t
        | otherwise -> OK bs (plusAddr# p n) t
    where !(MutableByteArray ba) = pinnedByteArrayFromString0 xs
          bs = B.PS (ForeignPtr (mutableByteArrayContents# ba) (PlainPtr ba)) 0 (I# (sizeofMutableByteArray# ba))

try :: Parser s a -> Parser s a
try (Parser m) = Parser $ \p s -> case m p s of
  OK a q t -> OK a q t
  Fail _ t -> Fail p t

word8 :: Word8 -> Parser s Word8
word8 0 = empty
word8 r@(W8# c) = Parser \p s -> case readWord8OffAddr# p 0# s of
  (# t, c' #) -> if isTrue# (c `eqWord#` c')
    then OK r (plusAddr# p 1#) t
    else Fail p t
{-# inline word8 #-}

---------------------------------------------------------------------------------------
-- * Super-unsafe literal parsers
---------------------------------------------------------------------------------------

-- | super-duper unsafe. Fabricates bytestrings that directly reference constant memory
litN :: Addr# -> CSize -> Parser s ByteString
litN q n = Parser \p s -> case io (c_strncmp p q n) s of
    (# t, 0 #) -> OK bs (p `plusAddr#` csize n) t
    (# t, _ #) -> Fail p t
  where bs = unsafeLiteralByteStringN q n

-- | Super unsafe. Fabricates a bytestring that directly reference constant memory.
--
-- Usage:
--
-- @
-- hello = lit "hello"#
-- @
lit :: Addr# -> Parser s ByteString
lit q = litN q (pure_strlen q)

literalForeignPtrContents :: ForeignPtrContents
literalForeignPtrContents = unsafeDupablePerformIO $ primitive \s -> case newByteArray# 0# s of
  (# t, a #) -> (# t, PlainPtr a #)
-- {-# noinline literalForeignPtrContents #-}

unsafeLiteralForeignPtr :: Addr# -> ForeignPtr Word8
unsafeLiteralForeignPtr addr = ForeignPtr addr literalForeignPtrContents

unsafeLiteralByteStringN :: Addr# -> CSize -> ByteString
unsafeLiteralByteStringN p n = PS (unsafeLiteralForeignPtr p) 0 (fromIntegral n)
{-# noinline unsafeLiteralByteStringN #-}

--unsafeLiteralByteString :: Addr# -> ByteString
--unsafeLiteralByteString p = unsafeLiteralByteStringN p (pure_strlen p)

-- Given a 'Base' you can do two things with it. While in a Parser, you're allowed to
-- access the memory between the start and end addresses, as they'll be alive.
--
-- However, you can always reconstruct a bytestring from the oriignal (non-0 terminated
-- data using 'bytes', and that will remain valid forever or until appropriately
-- garbage collected.
--
-- In general, in a Parser you should try to access the memory in the null-terminated
-- region for cache locality.
--
-- Afterwards, or to report bytestrings, you should trim them off the original, this
-- way, no additional memory needs to be copied, and the garbage collector will just
-- manage the storage of the bytestrings you cut off of the parent for you.

data Base s = Base
  { baseOriginal  :: Addr# -- the start of a valid bytestring
  , baseContents  :: ForeignPtrContents -- memory management for that bytestring
  , baseStart :: Addr# -- the start of our null terminated copy of the bytestring
  , baseEnd :: Addr# -- the end of our null terminated copy (points to the '\0')
  }

bytes :: forall s. KnownBase s => ByteString
bytes = case reflectBase @s of
  !(Base b g p q) -> mkBS b g (minusAddr# q p)
{-# inline bytes #-}

start :: forall s. KnownBase s => Addr#
start = baseStart (reflectBase @s)
{-# inline start #-}

end :: forall s. KnownBase s => Addr#
end = baseEnd (reflectBase @s)
{-# inline end #-}

class KnownBase (s :: Type) where
  reflectBase :: Base s

--------------------------------------------------------------------------------
-- * Parsing
--------------------------------------------------------------------------------

parse :: (forall s. KnownBase s => Parser s a) -> ByteString -> Either Location a
parse m bs@(B.PS (ForeignPtr b g) (I# o) (I# len)) = unsafeDupablePerformIO $
  B.useAsCString bs \(Ptr p) -> -- now it is null terminated
    IO \s -> let base = Base (plusAddr# b o) g p (plusAddr# p len) in
      case runParser (withBase (\_ -> m) base proxy#) p s of
        (# n, q, t #) -> (# t, finish base q n #)

finish :: Base s -> Addr# -> Option a -> Either Location a
finish (Base b g q r) p = \case
  Some a -> Right a
  None -> Left (location (mkBS b g (minusAddr# r q)) (I# (minusAddr# p q)))
{-# inline finish #-}

data Wrap s a = Wrap (KnownBase s => Proxy# s -> Parser s a)

withBase :: (KnownBase s => Proxy# s -> Parser s a) -> Base s -> Proxy# s -> Parser s a
withBase f x y = magicDict (Wrap f) x y
{-# inline withBase #-}
