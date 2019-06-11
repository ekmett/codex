{-# language ConstraintKinds #-}
{-# language DeriveTraversable #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language StrictData #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
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
( absolve, absolves
, flattenIncludes
, paths
, Body(..)
, deps
, IncludeCache
, GivenIncludeCache
, directives
, skip
, include
) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Combinators as P
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import Data.Either.Validation
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Watch
import Data.Watch.Directory
import System.FilePath
import Text.Parsnip as P

--------------------------------------------------------------------------------
-- * Body
--------------------------------------------------------------------------------

-- | Shader paths start with "/", so peel that off and attach a given base path.
shaderPathToRealPath :: FilePath -> FilePath -> FilePath 
shaderPathToRealPath base path = base </> makeRelative "/" path

-- | Contains a set of bytestrings which together form the shader
-- along with a list of interleaved bytestrings and include paths
-- that represent the same.
--
-- Note: the filepaths here are absolute paths, not shader paths
data Body = Body 
  { bodySource   :: [ByteString] -- ^ raw bytestring used to construct the body, for 'glCompileShaderIncludeARB'
  , bodyErrors   :: [String] -- ^ any parse errors encountered during construction
  , bodyIncludes :: [Either ByteString FilePath] -- ^ parsed body, with include directives separated out
  } deriving Show

instance Semigroup Body where
  Body a u b <> Body c v d = Body (a <> c) (u <> v) (b <> d)

instance Monoid Body where
  mempty = Body [] [] []

paths :: Traversal' Body FilePath
paths f (Body xs v ys) = Body xs v <$> traverse (traverse f) ys

-- |
-- Used for shaders made out of multiple bytestrings.
--
-- Currently assumes each one can be parsed independently.
absolves :: FilePath -> [ByteString] -> Body
absolves = foldMap . absolve

-- | Produce a body containing absolute filepaths
--
-- @
-- shaderDir <- canonicalizePath "shaders"
-- absolve shaderDir "some shader body"
-- @
absolve :: FilePath -> ByteString -> Body
absolve f bs = case parse (directives include) bs of
  Left j        -> Body [bs] [located j "preprocessing failed, dependencies incomplete"] [Left bs]
  Right content -> Body [bs] [] $ fmap (shaderPathToRealPath f) <$> content

-- | Used to polyfill an approximation of 'glCompileShaderIncludeARB'.
----
-- Returns 'Nothing' on an include cycle or on encountering a body we
-- could not parse with our approximate @#include@ scanner
flattenIncludes :: HashMap FilePath Body -> Body -> Validation [String] [ByteString]
flattenIncludes env = getAp . go [] where
  go :: [FilePath] -> Body -> Ap (Validation [String]) [ByteString]
  go m (Body _ es xs)
    | not (null es) = Ap $ Failure es -- include messages from the parser
    | otherwise = flip foldMap xs $ \case
      Left bs -> Ap $ Success [bs] -- bytestring snippet
      Right fp
        | elem fp m -> Ap $ Failure
          [ "include cycle: " ++ do intercalate "=>" $ fp:reverse (fp:Prelude.takeWhile (/= fp) m) ]
        | otherwise -> case HashMap.lookup fp env of
          Nothing -> Ap $ Failure ["environment is missing the file " ++ show fp]
          Just body -> go (fp:m) body

--------------------------------------------------------------------------------
-- * Cache
--------------------------------------------------------------------------------

type IncludeCache = HashMap FilePath (IOThunk Body)

type GivenIncludeCache = (?includes :: MVar IncludeCache, GivenDirectoryWatcher)

-- returns a thunk that has data dependencies on all includes mentioned
deps :: (GivenIncludeCache, MonadIO m) => FilePath -> Body -> m (IOThunk IncludeCache)
deps base body = liftIO $ delay $ execStateT (forOf_ paths body $ cache base) HashMap.empty

-- | Path should be absolute
includes :: (GivenIncludeCache, MonadIO m) => FilePath -> FilePath -> m (IOThunk Body)
includes base path = liftIO $ do
  modifyMVar ?includes $ \m -> case m^.at path of
    Just body -> pure (m,body)
    Nothing -> do
      body <- delay $ do
        thunk <- readWatchedFile path
        absolve base <$> force thunk
      pure (HashMap.insert path body m,body)

cache :: (GivenIncludeCache, MonadWatch m, PrimState m ~ RealWorld) => FilePath -> FilePath -> StateT IncludeCache m ()
cache base = go where
  go path = use (at path) >>= \case
    Just _ -> pure ()
    Nothing -> do
      bt <- ioToPrim $ includes base path
      at path ?= bt
      body <- force bt
      forOf_ paths body go -- walk the body
{-# inline cache #-}

--------------------------------------------------------------------------------
-- * Parser
--------------------------------------------------------------------------------

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
