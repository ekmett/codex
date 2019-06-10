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
module Engine.Include.Parser
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
import Text.Parsnip as P
import Data.Maybe

-- space within a line
spaces :: Parser ()
spaces = skipMany $ choice 
  [ () <$ satisfy (\c -> A.isSpace c && c `Prelude.notElem` vspace)
  , () <$ do "/*" *> P.breakSubstring "*/" *> "*/"
  , () <$ "\\\n"
  , () <$ "\\\r\n"
  ] where vspace = "\r\n" :: [Char]

token :: Parser a -> Parser a
token p = p <* spaces

newline :: Parser ()
newline = () <$ ("\n" <|> "\r\n")

-- actually must succeed, as we use this to delimit with skipManyTill, etc.
newlines :: Parser ()
newlines = skipSome $ token $ choice
  [ "//" *> skip
  , newline
  ]

skip :: Parser ()
skip = token anyChar `skipManyTill` newlines

nondirectives :: Parser ()
nondirectives = skipSome (satisfy ('#'/=) *> skip)

data Run a = Run Int Int a

directive :: Parser a -> Parser (Maybe (Run a))
directive p = do
  a <- mark
  _ <- token (char '#')
  m <- optional (token p)
  _ <- optional newlines
  b <- mark
  pure $ Run a b <$> m
  
-- parse (directives include) some_bytestring
-- parse (directives skip) gets all directives

directives :: Parser a -> Parser [Either ByteString a]
directives p = do
  spaces
  _ <- optional newlines
  _ <- optional nondirectives
  result <- directive p `sepEndBy` optional nondirectives
  endOfInput
  cut (catMaybes result)

include :: Parser FilePath
include = token "include" *> file

file :: Parser FilePath
file = token $ C.unpack <$> choice
  [ char '<' *> takeUntilChar '>' <* char '>'
  , char '"' *> takeUntilChar '"' <* char '"'
  ] 

trim :: Int -> Int -> ByteString -> ByteString
trim lo hi = B.unsafeTake (hi - lo) . B.unsafeDrop lo

cut :: [Run a] -> Parser [Either ByteString a]
cut xs0 = input <&> \bs -> go bs 0 xs0 where
  go bs i [] | bs' <- B.unsafeDrop i bs = [Left bs' | not $ B.null bs']
    
  go bs i (Run j k a:xs) 
    | bs' <- trim i j bs, not $ B.null bs' = Left bs' : Right a : go bs k xs
    | otherwise = Right a : go bs k xs
