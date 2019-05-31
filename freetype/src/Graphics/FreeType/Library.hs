{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.FreeType.Library
( Library
-- * create a reference counted library with all of the usual defaults
, init_library

-- * reference counting
, reference_library
, done_library

-- * properties
, property_set
, property_get

-- * raw library initialization
, new_library
, add_default_modules
, set_default_properties
) where

import Control.Monad.IO.Class
import Data.ByteString
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import Graphics.FreeType.Internal

C.context $ C.baseCtx <> C.bsCtx <> C.fptrCtx <> freeTypeCtx
C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.include "ft.h"

-- this will use fixed memory allocation functions, but allows us to avoid the FT_Init_FreeType and FT_Done_FreeType global mess.
new_library :: MonadIO m => m Library
new_library = liftIO $
  alloca $ \p -> do
    [C.exp|FT_Error { FT_New_Library(&hs_memory,$(FT_Library * p))}|] >>= ok
    peek p >>= foreignLibrary

add_default_modules :: MonadIO m => Library -> m ()
add_default_modules library = liftIO [C.block|void { FT_Add_Default_Modules($fptr-ptr:(FT_Library library)); }|]

set_default_properties :: MonadIO m => Library -> m ()
set_default_properties library = liftIO [C.block|void { FT_Set_Default_Properties($fptr-ptr:(FT_Library library)); }|]

init_library :: MonadIO m => m Library
init_library = liftIO $ do
  l <- new_library
  add_default_modules l
  set_default_properties l
  return l

-- | Add a reference to a library.
-- 
-- For the most part this should already be done for you through the API provided in Haskell.
reference_library :: MonadIO m => Library -> m ()
reference_library library = liftIO $ [C.exp|FT_Error { FT_Reference_Library($fptr-ptr:(FT_Library library))}|] >>= ok

-- | Remove a reference to a library, destroying the object if none remain.
--
-- For the most part this should already be done for you through the API provided in Haskell.
done_library :: MonadIO m => Library -> m ()
done_library library = liftIO $ [C.exp|FT_Error { FT_Done_Library($fptr-ptr:(FT_Library library))}|] >>= ok

property_set :: MonadIO m => Library -> ByteString -> ByteString -> Ptr a -> m ()
property_set library module_name property_name (castPtr -> value) = liftIO $
  [C.exp|FT_Error { FT_Property_Set($fptr-ptr:(FT_Library library),$bs-cstr:module_name,$bs-cstr:property_name,$(void * value))}|] >>= ok
  
property_get :: MonadIO m => Library -> ByteString -> ByteString -> Ptr a -> m ()
property_get library module_name property_name (castPtr -> value) = liftIO $
  [C.exp|FT_Error { FT_Property_Get($fptr-ptr:(FT_Library library),$bs-cstr:module_name,$bs-cstr:property_name,$(void * value))}|] >>= ok
  

