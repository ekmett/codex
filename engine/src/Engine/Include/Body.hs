{-# language DeriveTraversable #-}
module Engine.Include.Body
( absolve, absolves
, Body(..)
) where

import Control.Monad.IO.Class
import Engine.Include.Parser
import System.Directory

-- | Contains a set of bytestrings which together form the shader
-- along with a list of interleaved bytestrings and include paths
-- that represent the same.
data Body = Body [ByteString] [Either ByteString FilePath]
  deriving (Show, Functor, Foldable, Traversable)

instance Each Body Body FilePath FilePath where
  each f (Body xs ys) = Body xs <$> traverse (traverse f) ys

instance Semigroup Body where
  Body a b <> Body c d = Body (a <> c) (b <> d)

instance Monoid Body where
  mempty = Body mempty mempty

absolves :: MonadIO m => [ByteString] -> m Body
absolves = liftIO . getAp . foldMap (Ap . absolve)

-- | Produce a body containing absolute filepaths
absolve :: MonadIO m => ByteString -> m Body
absolve bs = liftIO $ do
  OK content _ <- parse (directives includes) bs
  Body bs <$> traverse (traverse makeAbsolute) content
