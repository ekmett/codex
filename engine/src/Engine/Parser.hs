{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}

module Engine.Parser
( Parser(..)
, Result(..)
, parse
, atEnd
, endOfInput
, breakSubstring
, mark
, take
, drop
, between
, release

-- * Char8
, satisfy
, char
, anyChar
, space
, digit
, letter_ascii
, letter_iso8859_15
, takeWhile
, takeUntilChar
, dropWhile
, dropUntilChar
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.ByteString (ByteString)
import Data.String
import GHC.Char
import Prelude hiding (takeWhile, dropWhile, take, drop)

parse :: Parser a -> ByteString -> Result a
parse p bs = runParser p bs 0

-- tiniest parser
data Result a
   = OK a {-# unpack #-} !Int
   | Fail
   deriving (Show, Functor, Foldable, Traversable)

-- non-backtracking parser
newtype Parser a = Parser { runParser :: ByteString -> Int -> Result a }
  deriving Functor

instance Applicative Parser where
  pure a = Parser $ \_ -> OK a
  {-# inline pure #-}
  Parser m <*> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      OK b k -> OK (a b) k
      Fail -> Fail
  {-# inline (<*>) #-}
  Parser m *> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK _ j -> n bs j
  {-# inline (*>) #-}
  Parser m <* Parser n = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      Fail -> Fail
      OK _ k -> OK a k
  {-# inline (<*) #-} 
  liftA2 f (Parser m) (Parser n) = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> case n bs j of
      Fail -> Fail
      OK b k -> OK (f a b) k

instance Monad Parser where
  Parser m >>= f = Parser $ \bs i -> case m bs i of
    Fail -> Fail
    OK a j -> runParser (f a) bs j
  {-# inline (>>=) #-}
  (>>) = (*>)
  {-# inline (>>) #-}

instance MonadFail Parser where
  fail _ = Parser $ \_ _ -> Fail
  {-# inline fail #-}

instance Alternative Parser where
  empty = Parser $ \_ _ -> Fail
  {-# inline empty #-}
  Parser m <|> Parser n = Parser $ \bs i -> case m bs i of
    Fail -> n bs i
    r@OK{} -> r
  {-# inline (<|>) #-}

instance MonadPlus Parser where
  mzero = empty
  {-# inline mzero #-}
  mplus = (<|>)
  {-# inline mplus #-}

instance a ~ ByteString => IsString (Parser a) where
  fromString s = Parser $ \ bs i ->
    if needle `C.isPrefixOf` B.unsafeDrop i bs
    then OK needle (i + B.length needle)
    else Fail
    where needle = fromString s
  {-# inline fromString #-}

endOfInput :: Parser ()
endOfInput = Parser $ \bs i -> if B.length bs == i then OK () i else Fail
{-# inline endOfInput #-}

atEnd :: Parser Bool
atEnd = Parser $ \bs i -> OK (B.length bs == i) i
{-# inline atEnd #-}

take :: Int -> Parser ByteString
take n = Parser $ \bs i -> if B.length bs < i + n then Fail else OK (B.unsafeTake n $ B.unsafeDrop i bs) (i + n)
{-# inline take #-}

drop :: Int -> Parser ()
drop n = Parser $ \bs i -> if B.length bs < i + n then Fail else OK () (i + n)
{-# inline drop #-}

-- | Note: this will always succeed, just like 'B.breakSubstring'
breakSubstring :: ByteString -> Parser ByteString
breakSubstring needle = Parser $ \bs i -> let r = fst $ B.breakSubstring needle (B.unsafeDrop i bs) in 
  OK r (i + B.length r)
{-# inline breakSubstring #-}  

mark :: Parser Int
mark = Parser $ \_ i -> OK i i
{-# inline mark #-}

between :: Int -> Int -> Parser ByteString
between i j = Parser $ \bs k -> OK (B.take (j-i) $ B.drop i bs) k
{-# inline between #-}

-- | Teleport back to an earlier position.
release :: Int -> Parser ()
release i = Parser $ \bs _ -> if 0 <= i && i <= B.length bs then OK () i else Fail
{-# inline release #-}

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
  | otherwise -> Fail
{-# inline satisfy #-}

char :: Char -> Parser Char
char x = Parser $ \bs i -> if
  | B.length bs > i, B.w2c (B.unsafeIndex bs i) == x -> OK x (i + 1)
  | otherwise -> Fail
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
