{-# language CPP #-}
module Graphics.Glow.Data where

import Paths_codex
import Control.Monad.IO.Class

-- load any file in the codex project

#if EMBED
import Data.FileEmbed
import Data.ByteString.Char8 as Strict

files :: [(FilePath, Char8.ByteString)]
files = $(makeRelativeToProject "shaders/glow" >>= embedDir)
#endif

readData :: MonadIO m => FilePath -> m Strict.ByteString
readData fp = liftIO $ -- should we prefer loading from the disk when present instead?
#if EMBED
  case lookup fp files of
    Just c -> pure c
    Nothing -> -- fall back on reading files
#endif
      Paths_codex.getDataFileName fp >>= Strict.readFile
