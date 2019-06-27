{-# language ConstraintKinds #-}
{-# language DeriveTraversable #-}
{-# language ImplicitParams #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language StrictData #-}
{-# language TupleSections #-}
{-# language TypeFamilies #-}
{-# language BlockArguments #-}
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
module Engine.Shader.Include
( absolve, absolves
, GivenShaderDir
, shaderPathToRealPath
, realPathToShaderPath
, flattenIncludes
, paths
, Body(..)
, deps
, newIncludeCache
, withIncludeCache
, IncludeMap
, IncludeCache
, GivenIncludeCache
, directives
, skipped
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
import Data.Either.Validation
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

type GivenShaderDir = (?shaderDir :: FilePath)

-- | Shader paths start with "/", so peel that off and attach a given base path.
shaderPathToRealPath :: GivenShaderDir => FilePath -> FilePath
shaderPathToRealPath path = ?shaderDir </> makeRelative "/" path

realPathToShaderPath :: GivenShaderDir => FilePath -> FilePath
realPathToShaderPath path = "/" </> makeRelative ?shaderDir path

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
absolves :: GivenShaderDir => [ByteString] -> Body
absolves = foldMap absolve

-- | Produce a body containing absolute filepaths
--
-- @
-- shaderDir <- canonicalizePath "shaders"
-- absolve shaderDir "some shader body"
-- @
absolve :: GivenShaderDir => ByteString -> Body
absolve bs = case parse (directives include) bs of
  Left j        -> Body [bs] [located j "preprocessing failed, dependencies incomplete"] [Left bs]
  Right content -> Body [bs] [] $ fmap shaderPathToRealPath <$> content

-- | Used to polyfill an approximation of 'glCompileShaderIncludeARB'.
----
-- Returns 'Nothing' on an include cycle or on encountering a body we
-- could not parse with our approximate @#include@ scanner
flattenIncludes :: HashMap FilePath Body -> Body -> Validation [String] [ByteString]
flattenIncludes env = getAp . go [] where
  go :: [FilePath] -> Body -> Ap (Validation [String]) [ByteString]
  go m (Body _ es xs)
    | not (null es) = Ap $ Failure es -- include messages from the parser
    | otherwise = flip foldMap xs \case
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

type IncludeMap = HashMap FilePath (IOThunk Body)
type IncludeCache = MVar IncludeMap
type GivenIncludeCache = (?includes :: IncludeCache)

newIncludeCache :: MonadIO m => m IncludeCache
newIncludeCache = liftIO $ newMVar HashMap.empty

withIncludeCache :: MonadIO m => (GivenIncludeCache => m a) -> m a
withIncludeCache m = do
  c <- newIncludeCache
  let ?includes = c
  m

-- returns a thunk that has data dependencies on all includes mentioned
deps :: (GivenIncludeCache, GivenShaderDir, GivenDirectoryWatcher, MonadIO m) => Body -> m (IOThunk IncludeMap)
deps body = liftIO $ delay $ execStateT (forOf_ paths body cache) HashMap.empty

-- | Path should be absolute
includes :: (GivenIncludeCache, GivenShaderDir, GivenDirectoryWatcher) => FilePath -> IO (IOThunk Body)
includes path = modifyMVar ?includes \m -> case m^.at path of
  Just body -> pure (m,body)
  Nothing -> do
    body <- delay do
      thunk <- readWatchedFile path
      absolve <$> force thunk
    pure (HashMap.insert path body m,body)

cache :: (GivenIncludeCache, GivenShaderDir, GivenDirectoryWatcher) => FilePath -> StateT IncludeMap (Watch RealWorld) ()
cache path = use (at path) >>= \case
  Just _ -> pure ()
  Nothing -> do
    bt <- ioToPrim $ includes path
    at path ?= bt
    body <- force bt
    forOf_ paths body cache -- walk the body
{-# inline cache #-}

--------------------------------------------------------------------------------
-- * Parser
--------------------------------------------------------------------------------

-- space within a line
spaces :: Parser s ()
spaces = skipMany $ choice
  [ () <$ satisfy \c -> A.isSpace c && c `Prelude.notElem` vspace
  , () <$ do "/*" *> P.skipTillSubstring "*/" *> "*/"
  , () <$ "\\\n"
  , () <$ "\\\r\n"
  ] where vspace = "\r\n" :: [Char]

token :: Parser s a -> Parser s a
token p = p <* spaces

newline :: Parser s ()
newline = () <$ ("\n" <|> "\r\n")

-- actually must succeed, as we use this to delimit with skipManyTill, etc.
newlines :: Parser s ()
newlines = skipSome $ token $ choice
  [ "//" *> skipped
  , newline
  ]

skipped :: Parser s ()
skipped = token anyChar `skipManyTill` newlines

nondirectives :: Parser s ()
nondirectives = skipSome (satisfy ('#'/=) *> skipped)

data Run s a = Run (Mark s) (Mark s) a

directive :: Parser s a -> Parser s (Maybe (Run s a))
directive p = do
  a <- mark
  _ <- token (char '#')
  m <- optional (token p)
  _ <- optional newlines
  b <- mark
  pure $ Run a b <$> m

-- parse (directives include) some_bytestring
-- parse (directives skipped) gets all directives

directives :: KnownBase s => Parser s a -> Parser s [Either ByteString a]
directives p = do
  spaces
  _ <- optional newlines
  _ <- optional nondirectives
  result <- directive p `sepEndBy` optional nondirectives
  endOfInput
  pure $ cut $ catMaybes result

include :: KnownBase s => Parser s FilePath
include = token "include" *> file

file :: KnownBase s => Parser s FilePath
file = token $ C.unpack <$> choice
  [ char '<' *> tillChar '>' <* char '>'
  , char '"' *> tillChar '"' <* char '"'
  ]

cut :: KnownBase s => [Run s a] -> [Either ByteString a]
cut = go minBound where
  go i [] | bs <- snip i maxBound = [Left bs | not $ B.null bs]
  go i (Run j k a:xs)
    | bs <- snip i j, not (B.null bs) = Left bs : ys
    | otherwise = ys
    where ys = Right a : go k xs
