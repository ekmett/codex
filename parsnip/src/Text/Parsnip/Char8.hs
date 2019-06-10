{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}

module Text.Parsnip.Char8
( satisfy
, char
, anyChar
, space
, digit
, letter_ascii
, letter_iso8859_15
, takeWhile
, takeUntilChar
, takeWhile1 
, dropWhile
, dropUntilChar
, dropWhile1 
, previousChar
) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.ByteString (ByteString)
import GHC.Char
import Prelude hiding (takeWhile, dropWhile, take, drop)

import Text.Parsnip.Internal
import Text.Parsnip.Parser

-- * Char8

digit :: Parser Char
digit = satisfy A.isDigit
{-# inline digit #-}

space :: Parser Char
space = satisfy A.isSpace
{-# inline space #-}

letter_ascii :: Parser Char
letter_ascii = satisfy A.isAlpha_ascii
{-# inline letter_ascii #-}

letter_iso8859_15 :: Parser Char
letter_iso8859_15 = satisfy A.isAlpha_iso8859_15
{-# inline letter_iso8859_15 #-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \bs i -> if 
  | B.length bs > i, a <- B.w2c (B.unsafeIndex bs i), p a -> OK a (i + 1)
  | otherwise -> Fail i
{-# inline satisfy #-}

char :: Char -> Parser Char
char x = Parser $ \bs i -> if
  | B.length bs > i, B.w2c (B.unsafeIndex bs i) == x -> OK x (i + 1)
  | otherwise -> Fail i
{-# inline char #-}

anyChar :: Parser Char
anyChar = satisfy $ const True
{-# inline anyChar #-}

takeWhile :: (Char -> Bool) -> Parser ByteString
takeWhile p = Parser $ \bs i -> let r = C.takeWhile p $ B.drop i bs in OK r (i + B.length r)
{-# inline [1] takeWhile #-}

{-# RULES
"takeWhile (x/=)" forall x. 
  takeWhile (x `neChar`)  = takeUntilChar x
"takeWhile (/=x)" forall x. 
  takeWhile (`neChar` x)  = takeUntilChar x
  #-}

takeUntilChar :: Char -> Parser ByteString
takeUntilChar c = Parser $ \bs i -> case B.unsafeDrop i bs of
  rest -> case B.elemIndex (B.c2w c) rest of
    Nothing -> OK rest (B.length bs)
    Just n -> OK (B.unsafeTake n rest) (i + n)

dropWhile :: (Char -> Bool) -> Parser ()
dropWhile p = Parser $ \bs i -> OK () $ case C.findIndex (not . p) (B.unsafeDrop i bs) of
  Nothing -> B.length bs
  Just j -> j + i
{-# inline [1] dropWhile #-}

{-# RULES
"dropWhile (x/=)" forall x. 
  dropWhile (x `neChar`)  = dropUntilChar x
"dropWhile (/=x)" forall x. 
  dropWhile (`neChar` x)  = dropUntilChar x
  #-}

dropUntilChar :: Char -> Parser ()
dropUntilChar c = Parser $ \bs i -> OK () $ case B.elemIndex (B.c2w c) $ B.unsafeDrop i bs of 
  Nothing -> B.length bs
  Just n -> n + i
{-# inline dropUntilChar #-}

dropWhile1 :: (Char -> Bool) -> Parser ()
dropWhile1 p = satisfy p *> dropWhile p
{-# inline takeWhile1 #-}

takeWhile1 :: (Char -> Bool) -> Parser ByteString
takeWhile1 p = snipping (dropWhile1 p)
{-# inline dropWhile1 #-}

previousChar :: Parser (Maybe Char)
previousChar = Parser $ \bs i -> if i == 0 then OK Nothing 0 else OK (Just $ B.w2c $ B.unsafeIndex bs $ i-1) i -- non-consuming choice 
{-# inline previousChar #-}
