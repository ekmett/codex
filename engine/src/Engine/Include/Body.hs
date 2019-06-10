{-# language DeriveTraversable #-}
{-# language LambdaCase #-}
module Engine.Include.Body
( absolve, absolves
, paths
, Body(..)
) where

import Control.Lens.Type
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid
import Engine.Include.Parser
import System.Directory
import System.IO
import Text.Parsnip.Internal
import Text.Parsnip.Location

-- | Contains a set of bytestrings which together form the shader
-- along with a list of interleaved bytestrings and include paths
-- that represent the same.
data Body = Body [ByteString] [Either ByteString FilePath]
  deriving Show

paths :: Traversal' Body FilePath
paths f (Body xs ys) = Body xs <$> traverse (traverse f) ys

instance Semigroup Body where
  Body a b <> Body c d = Body (a <> c) (b <> d)

instance Monoid Body where
  mempty = Body mempty mempty

absolves :: MonadIO m => [ByteString] -> m Body
absolves = liftIO . getAp . foldMap (Ap . absolve)

-- | Produce a body containing absolute filepaths
absolve :: MonadIO m => ByteString -> m Body
absolve bs = liftIO $ case runParser (directives include) bs 0 of
  Fail j -> do
    hPutStrLn stderr $ located (location bs j) "Warning: preprocessing failed"
    pure $ Body [bs] [Left bs]
  OK content _ -> Body [bs] <$> traverse (traverse makeAbsolute) content
