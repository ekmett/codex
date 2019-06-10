{-# language StrictData #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
-- | In order to properly "watch" the changes to a shader
-- we need to know what it depends on. There is no callback
-- in OpenGL to determine which NamedStrings a shader uses.
--
-- So let's scan through the files and build the index
-- ourself. This can also allow us the ability to badly
-- polyfill 'glCompileShaderIncludeARGB'.
--
-- @parse (directives include)@ breaks a file into includes
-- and everything else in such a fashion that in theory
-- if we splice the include contents at the positions
-- indicated, it'll behave mostly like a c preprocessor.
--
-- (Modulo handling X-Macros and recursion correctly.)
module Engine.Include
( directives
, skip
, include
) where

import Control.Monad.Combinators as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Functor ((<&>))
import Engine.Parser as P
import Data.Maybe

-- space within a line
simple :: Parser ()
simple = skipSome $ choice 
  [ () <$ satisfy (\c -> A.isSpace c && c `Prelude.notElem` vspace)
  , () <$ do "/*" *> P.breakSubstring "*/" *> "*/"
  , () <$ "\\\n"
  , () <$ "\\\r\n"
  ] where vspace = "\r\n" :: [Char]

token :: Parser a -> Parser a
token p = p <* optional simple

newline :: Parser ()
newline = () <$ ("\n" <|> "\r\n")

nonsimple :: Parser ()
nonsimple = skipSome $ token $ choice
  [ "//" *> skip
  , newline
  ]

skip :: Parser ()
skip = token anyChar `skipManyTill` nonsimple

nondirective :: Parser ()
nondirective = satisfy ('#'/=) *> skip

directive :: Parser a -> Parser (Maybe (Int,a,Int))
directive p = do
  a <- mark
  mp <- token (char '#' *> optional p) -- try'd before
  optional nonsimple
  b <- mark
  pure $ (a,,b) <$> mp
  
-- parse (directives include) some_bytestring

directives :: Parser a -> Parser [Either ByteString a]
directives p = do
  skipMany simple
  skipMany nonsimple
  skipMany nondirective
  result <- directive p `sepEndBy` skipMany nondirective
  endOfInput
  cut (catMaybes result)

include :: Parser FilePath
include = token "include" *> token file

file :: Parser FilePath
file = C.unpack <$ char '<' <*> takeUntilChar '>' <* char '>'
   <|> C.unpack <$ char '"' <*> takeUntilChar '"' <* char '"'

-- directives skip -- gets all directives

trim :: Int -> Int -> ByteString -> ByteString
trim lo hi = B.unsafeTake (hi - lo) . B.unsafeDrop lo

cut :: [(Int,a,Int)] -> Parser [Either ByteString a]
cut xs0 = input <&> \bs -> go bs 0 xs0 where
  go bs i [] | bs' <- B.unsafeDrop i bs = [Left bs' | not $ B.null bs']
    
  go bs i ((j,a,k):xs) 
    | bs' <- trim i j bs, not $ B.null bs' = Left bs' : Right a : go bs k xs
    | otherwise = Right a : go bs k xs
