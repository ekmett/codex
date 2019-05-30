{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Graphics.FreeType.Library
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

-- this will use fixed memory allocation functions, but allows us to avoid the FT_Init_FreeType and FT_Done_FreeType global mess.
new_library :: MonadIO m => m Library

-- new_library :: MonadIO m => Memory -> m (Either Error Library)
