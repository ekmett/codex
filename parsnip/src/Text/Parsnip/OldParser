{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}
{-# language BlockArguments #-}

module Text.Parsnip.Parser
( Parser
, Result(..)
, parse
, atEnd
, endOfInput
, breakSubstring
, mark
, take
, drop
, snip
, release
, snipping
, input
, try
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Prelude hiding (takeWhile, dropWhile, take, drop)

import Text.Parsnip.Internal
import Text.Parsnip.Location

parse :: Parser a -> ByteString -> Either Location a
parse p bs = case runParser p bs 0 of
  OK a _ -> Right a
  Fail j -> Left $ location bs j

endOfInput :: Parser ()
endOfInput = Parser \bs i -> if B.length bs == i then OK () i else Fail i
{-# inline endOfInput #-}

atEnd :: Parser Bool
atEnd = Parser \bs i -> OK (B.length bs == i) i
{-# inline atEnd #-}

take :: Int -> Parser ByteString
take n = Parser \bs i -> if B.length bs < i + n then Fail i else OK (B.unsafeTake n $ B.unsafeDrop i bs) (i + n)
{-# inline take #-}

drop :: Int -> Parser ()
drop n = Parser \bs i -> if B.length bs < i + n then Fail i else OK () (i + n)
{-# inline drop #-}

-- | Note: this will always succeed, just like 'B.breakSubstring'
breakSubstring :: ByteString -> Parser ByteString
breakSubstring needle = Parser \bs i -> let r = fst $ B.breakSubstring needle (B.unsafeDrop i bs) in 
  OK r (i + B.length r)
{-# inline breakSubstring #-}  

mark :: Parser Int
mark = Parser \_ i -> OK i i
{-# inline mark #-}

snip :: Int -> Int -> Parser ByteString
snip i j = Parser \bs k -> OK (B.take (j-i) $ B.drop i bs) k
{-# inline snip #-}

-- | Teleport back to an earlier position.
release :: Int -> Parser ()
release i = Parser \bs j -> if 0 <= i && i <= B.length bs then OK () i else Fail j
{-# inline release #-}

snipping :: Parser a -> Parser ByteString
snipping (Parser p) = Parser \ bs i -> case p bs i of
  Fail j -> Fail j
  OK _ j -> OK (B.unsafeTake (max (j - i) 0) $ B.unsafeDrop i bs) j
{-# inline snipping #-}

try :: Parser a -> Parser a
try (Parser p) = Parser \bs i -> case p bs i of
  Fail _ -> Fail i
  x -> x
{-# inline try #-}

input :: Parser ByteString
input = Parser OK
{-# inline input #-}
