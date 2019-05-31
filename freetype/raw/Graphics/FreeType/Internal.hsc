{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language ForeignFunctionInterface #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language CPP #-}

-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--

#ifndef HLINT
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_MODULE_H
#include FT_SYSTEM_H
#include FT_TYPES_H
#include "hsc-err.h"
#let err_exports = err_exports()
#let err_patterns = err_patterns()
#endif

module Graphics.FreeType.Internal
( Error
  ( Error
#ifndef HLINT
#err_exports
#endif
  ) 
, ok
, Face(..)
, FaceRec
, Library(..)
, LibraryRec
, Memory(..)
, MemoryRec(..)
, AllocFunc
, FreeFunc
, ReallocFunc
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

, foreignLibrary
, foreignFace
-- * contexts
, freeTypeCtx
) where

import Control.Exception
import Data.Data (Data)
import qualified Data.Map as Map
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as Concurrent
import Foreign.Marshal.Unsafe
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.FreeType.Private

-- | By convention the library will throw any non-0 FT_Error encountered.
newtype Error = Error CInt deriving (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

#ifndef HLINT
#err_patterns
#endif

foreign import ccall unsafe "ft.h" hs_get_error_string :: Error -> CString

instance Exception Error where
  displayException = unsafeLocalState . peekCString . hs_get_error_string

-- | Ensure that the 'Error' is ok, otherwise throw it.
ok :: Error -> IO ()
ok Err_Ok = return ()
ok e = throwIO e

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

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Error", [t|Error|])
    , (C.TypeName "FT_Face", [t|Ptr FaceRec|])
    , (C.TypeName "FT_Library", [t|Ptr LibraryRec|])
    ]
  }

C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"

foreignFace :: Ptr FaceRec -> IO Face
foreignFace p = Face <$> Concurrent.newForeignPtr p ([C.exp|FT_Error { FT_Done_Face($(FT_Face p)) }|] >>= ok)

-- | Assumes we are using @FT_New_Library@ management rather than @FT_Init_FreeType@ management
foreignLibrary :: Ptr LibraryRec -> IO Library
foreignLibrary p = Library <$> Concurrent.newForeignPtr p ([C.exp|FT_Error { FT_Done_Library($(FT_Library p)) }|] >>= ok)

freeTypeCtx :: C.Context
freeTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Error", [t|Error|])
    , (C.TypeName "FT_Face", [t|Ptr FaceRec|])
    , (C.TypeName "FT_FaceRec_", [t|FaceRec|])
    , (C.TypeName "FT_Int", [t|CInt|])
    , (C.TypeName "FT_Library", [t|Ptr LibraryRec|])
    , (C.TypeName "FT_LibraryRec_", [t|LibraryRec|])
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
