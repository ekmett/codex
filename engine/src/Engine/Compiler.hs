module Engine.Compiler
(
) where

import Data.Watch
import Data.Watch.Directory
import System.Directory

data BODY a = Body [ByteString] [Either ByteString a]
  deriving (Show, Functor, Foldable, Traversable)

type Body = BODY FilePath

type Cache = MVar (Map FilePath (ThunkIO Body)) -- TODO: hold weak thunks?

type GivenCache = (?cache :: Cache, GivenDirectoryWatcher)

instance Semigroup (Body a) where
  Body a b <> Body c d = Body (a <> c) (b <> d)

instance Monoid (Body a) where
  mempty = Body mempty mempty

absolve :: MonadIO m => ByteString -> m (Body FilePath)
absolve bs = liftIO $ do
  OK content _ <- parse (directives includes) bs
  Body bs <$> traverse (traverse makeAbsolute) content
 
includes :: (GivenCache, MonadWatch m) => FilePath -> m (Body FilePath)
includes file = do
  modifyMVar cache $ \m ->
  readWatchedFile file >>= absolve

-- returns a thunk that has data dependencies on all includes mentioned
trace :: (GivenDirectoryWatcher, MonadIO m) => [ByteString] -> m (ThunkIO (Body FilePath))
trace seed = do
  body <- getAp $ foldMap (Ap . absolve) seed
  execStateT (for_ body cache) Map.empty

cache :: FilePath -> StateT Cache IO ()
cache path = use (at path) >>= \case
  Nothing -> do
    body <- delay $ includes path
    at path ?= body
    traverse_ cache body
  Just body -> pure ()

execState (Map.empty)
  go cache fp = case cache^.at fp of
    Nothing -> do
      content <- includes fp
      cache & at fp .~ content


