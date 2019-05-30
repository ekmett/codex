{-# language PatternSynonyms #-}
{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}
module Graphics.FreeType.Internal
( Face(..)
, Library(..)
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH
, freeTypeCtx
) where

import Data.Data (Data)
import qualified Data.Map as Map
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.FreeType.Private

#ifndef HLINT
#include <ft2build.h>
#include FT_FREETYPE_H
#endif

newtype Face = Face (ForeignPtr Face) deriving (Eq,Ord,Show,Data)

pattern FREETYPE_MAJOR = (#const FREETYPE_MAJOR) :: Int
pattern FREETYPE_MINOR = (#const FREETYPE_MINOR) :: Int
pattern FREETYPE_PATCH = (#const FREETYPE_PATCH) :: Int

newtype Library = Library (ForeignPtr Library) deriving (Eq,Ord,Show,Data)

freeTypeCtx :: C.Context
freeTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Library", [t|Ptr Library|])
    , (C.TypeName "FT_Int", [t|CInt|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char (Just C.Unsigned))) [t|Ptr CUChar|] [|withCUString|])
    , ("str", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char Nothing)) [t|Ptr CChar|] [|withCString|])
    , ("library", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Library") [t|Ptr Library|] [|withForeignPtr|])
    ]
  }
