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
{-# options_ghc -O2 #-}

module Text.Parsnip.Internal.Parser
( 
-- * Parser
  Parser(..)
-- * Unsafe literals
, lit, litN
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.Primitive.ByteArray
import Data.Kind
import Data.String
import Foreign.C.Types
import Foreign.ForeignPtr
import GHC.ForeignPtr
import GHC.Arr
import GHC.Prim
import GHC.Ptr
import GHC.Types
import GHC.Word
import System.IO.Unsafe

import Text.Parsnip.Location
import Text.Parsnip.Internal.Private
import Text.Parsnip.Internal.Option
import Text.Parsnip.Internal.Result

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
    OK a q t -> OK b q t
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
    OK f q t -> n q t
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser \p s -> case m p s of
    OK a q t -> setResult a (n q t)
    p -> p
  {-# inline (<*) #-}

instance Monad (Parser s) where
  Parser m >>= f = Parser \p s -> case m p s of
    Fail q t -> Fail q t
    OK a q t -> runParser (f a) q t
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}
  fail _ = Parser Fail
  {-# inline fail #-}

instance Alternative (Parser s) where
  Parser m <|> Parser n = Parser \ p s -> case m p s of
    Fail _ t -> m p t
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
    where MutableByteArray ba = pinnedByteArrayFromString0 xs
          bs = B.PS (ForeignPtr (mutableByteArrayContents# ba) (PlainPtr ba)) 0 (I# (sizeofMutableByteArray# ba))

---------------------------------------------------------------------------------------
-- * Super-unsafe literal parsers
---------------------------------------------------------------------------------------

-- super unsafe
litN :: Addr# -> CSize -> Parser s ByteString
litN q n = Parser \p s -> case io (c_strncmp p q n) s of
    (# t, 0 #) -> OK (unsafeLiteralByteStringN q n) (p `plusAddr#` csize n) s
    (# t, _ #) -> Fail p t

lit :: Addr# -> Parser s ByteString
lit q = litN q (pure_strlen q)

literalForeignPtrContents :: ForeignPtrContents
literalForeignPtrContents = unsafeDupablePerformIO $ primitive \s -> case newByteArray# 0# s of
  (# t, a #) -> (# t, PlainPtr a #)
-- {-# noinline literalForeignPtrContents #-}

unsafeLiteralForeignPtr :: Addr# -> ForeignPtr Word8
unsafeLiteralForeignPtr addr = ForeignPtr addr undefined -- literalForeignPtrContents

unsafeLiteralByteStringN :: Addr# -> CSize -> ByteString
unsafeLiteralByteStringN p n = PS (unsafeLiteralForeignPtr p) 0 (fromIntegral n)

unsafeLiteralByteString :: Addr# -> ByteString
unsafeLiteralByteString p = unsafeLiteralByteStringN p (pure_strlen p)

