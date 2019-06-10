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
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Monoid
import Engine.Include.Parser
import Engine.Parser.Internal
import System.Directory
import System.IO

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

data Loc = Loc Int Int ByteString

loc :: ByteString -> Int -> Loc
loc bs j = Loc (B.count '\n' before) (j - start) content where
  (before, after) = B.splitAt j bs
  start = fromMaybe 0 $ B.elemIndexEnd '\n' before
  end = maybe (B.length bs) (B.length before +) (B.elemIndex '\n' after)
  content = B.take (end - start) $ B.drop start bs

-- | Til we get a pretty printer in here
located :: Loc -> String -> String
located (Loc l c bs) msg = Prelude.unlines
  [ show l ++ ":" ++ show c ++ " " ++ msg
  , ls
  , show l ++ " | " ++ B.unpack bs
  , ls ++ Prelude.replicate c ' ' ++ "^"
  ] where ls = Prelude.replicate (length (show l) + 1) ' ' ++ "|"

-- | Produce a body containing absolute filepaths
absolve :: MonadIO m => ByteString -> m Body
absolve bs = liftIO $ case runParser (directives include) bs 0 of
  Fail j -> do
    hPutStrLn stderr $ located (loc bs j) "Warning: preprocessing failed"
    pure $ Body [bs] [Left bs]
  OK content _ -> Body [bs] <$> traverse (traverse makeAbsolute) content
