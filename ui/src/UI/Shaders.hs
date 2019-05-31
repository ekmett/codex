{-# language CPP #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett and Sean Chalmers
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module UI.Shaders
( namedStrings
, shader
) where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Char8 as Char8
import Data.FileEmbed
import qualified Paths_ui

#if EMBED
shaders :: [(FilePath, Char8.ByteString)]
shaders = $(makeRelativeToProject "shaders" >>= embedDir)
#endif

shader :: MonadIO m => FilePath -> m Char8.ByteString
shader fp = liftIO $ 
#if EMBED
  case lookup fp files of
    Just c -> pure c
    Nothing -> error "missing file"
#else
  Paths_ui.getDataFileName fp >>= Char8.readFile
#endif

-- TODO: add options to configure this to read from current dir, data dir or from embedded shaders
-- with various fallback schemes and dynamic loaders?

-- retrieve the shader set either statically or dynamically
namedStrings :: MonadIO m => m [(FilePath, Char8.ByteString)]
#if EMBED
namedStrings = pure shaders
#else
namedStrings = liftIO $ Paths_ui.getDataFileName "shaders" >>= getDir
#endif
