{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.FreeType.Version 
( library_version
, library_version_string
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH
) where

import Control.Monad.IO.Class
import Data.Version
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Language.C.Inline as C
import Graphics.FreeType.Internal

C.context $ C.baseCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_TYPES_H"

-- | Useful when dynamic linking, as we can't rely on the patterns above which were determined at compile time.
library_version  :: MonadIO m => Library -> m Version
library_version library = liftIO $ allocaArray 3 $ \ver -> do
  [C.block|void {
     FT_Int * ver = $(FT_Int * ver);
     FT_Library_Version($fptr-ptr:(FT_Library library),ver,ver+1,ver+2);
  }|]
  a <- peek ver
  b <- peek (advancePtr ver 1)
  c <- peek (advancePtr ver 2)
  pure $ makeVersion [fromIntegral a, fromIntegral b, fromIntegral c]

library_version_string :: MonadIO m => Library -> m String
library_version_string library = showVersion <$> library_version library

