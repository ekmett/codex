module Engine.Compiler
( watchNamedStrings
) where

import System.Directory

watchNamedStrings :: (MonadIO m, HasDirectoryWatcher) => FilePath -> m ()
watchNamedStrings filepath = do
  listenToTree "shaders"
