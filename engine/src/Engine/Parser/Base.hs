{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language DeriveTraversable #-}

module Engine.Parser.Base
( Parser
, Result(..)
, parse
, atEnd
, endOfInput
, breakSubstring
, mark
, take
, drop
, span
, release
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Prelude hiding (takeWhile, dropWhile, take, drop, span)

import Engine.Parser.Internal

parse :: Parser a -> ByteString -> Result a
parse p bs = runParser p bs 0

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

span :: Int -> Int -> Parser ByteString
span i j = Parser $ \bs k -> OK (B.take (j-i) $ B.drop i bs) k
{-# inline span #-}

-- | Teleport back to an earlier position.
release :: Int -> Parser ()
release i = Parser $ \bs _ -> if 0 <= i && i <= B.length bs then OK () i else Fail
{-# inline release #-}
