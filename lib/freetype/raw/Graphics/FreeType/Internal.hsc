{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}
module Graphics.FreeType.Internal
( Face(..)
, Library(..)
, MemoryRec(..)
, AllocFunc
, FreeFunc
, ReallocFunc
, Memory(..)
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
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.FreeType.Private

#ifndef HLINT
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SYSTEM_H
#endif

data FaceRec
newtype Face = Face (ForeignPtr FaceRec) deriving (Eq,Ord,Show)

pattern FREETYPE_MAJOR = (#const FREETYPE_MAJOR) :: Int
pattern FREETYPE_MINOR = (#const FREETYPE_MINOR) :: Int
pattern FREETYPE_PATCH = (#const FREETYPE_PATCH) :: Int

data LibraryRec
newtype Library = Library (ForeignPtr LibraryRec) deriving (Eq,Ord,Show)

type AllocFunc = Memory -> CLong -> IO (Ptr ())
type FreeFunc = Memory -> Ptr () -> IO ()
type ReallocFunc = Memory -> CLong -> CLong -> Ptr () -> IO (Ptr ())

data MemoryRec = MemoryRec
  { memory_user :: Ptr ()
  , memory_alloc :: FunPtr AllocFunc
  , memory_free :: FunPtr FreeFunc
  , memory_realloc :: FunPtr ReallocFunc
  } deriving (Eq,Show)

instance Storable MemoryRec where
  sizeOf _ = #size struct FT_MemoryRec_
  alignment _ = #alignment struct FT_MemoryRec_
  peek p = MemoryRec
    <$> (#peek struct FT_MemoryRec_, user) p
    <*> (#peek struct FT_MemoryRec_, alloc) p
    <*> (#peek struct FT_MemoryRec_, free) p
    <*> (#peek struct FT_MemoryRec_, realloc) p
  poke p MemoryRec{..} = do
    (#poke struct FT_MemoryRec_, user) p memory_user
    (#poke struct FT_MemoryRec_, alloc) p memory_alloc
    (#poke struct FT_MemoryRec_, free) p memory_free
    (#poke struct FT_MemoryRec_, realloc) p memory_realloc

newtype Memory = Memory (ForeignPtr MemoryRec) deriving (Eq,Ord,Show)

freeTypeCtx :: C.Context
freeTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Face", [t|CInt|])
    , (C.TypeName "FT_Int", [t|CInt|])
    , (C.TypeName "FT_Library", [t|Ptr LibraryRec|])
    , (C.TypeName "FT_LibraryRec_", [t|Ptr LibraryRec|])
    , (C.TypeName "FT_Memory", [t|Ptr MemoryRec|])
    , (C.TypeName "FT_MemoryRec_", [t|MemoryRec|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char (Just C.Unsigned))) [t|Ptr CUChar|] [|withCUString|])
    , ("str", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char Nothing)) [t|Ptr CChar|] [|withCString|])
    , ("library", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Library") [t|Ptr LibraryRec|] [|withForeignPtr|])
    , ("face", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Face") [t|Ptr FaceRec|] [|withForeignPtr|])
    ]
  }
