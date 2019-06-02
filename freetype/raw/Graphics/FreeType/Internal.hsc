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
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-}

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
, Face(..), FaceRec
, Generic(..)
, Library(..), LibraryRec
, Memory(..), MemoryRec(..), AllocFunc, FreeFunc, ReallocFunc
, Fixed
, Pos
, SizeRec(..), SizeMetrics(..), SizeInternalRec
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

, foreignLibrary
, foreignFace
-- * contexts
, freeTypeCtx
) where

import Control.Exception
import Data.Int
import qualified Data.Map as Map
import Data.Word
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

#ifndef HLINT
pattern FREETYPE_MAJOR = (#const FREETYPE_MAJOR) :: Int
pattern FREETYPE_MINOR = (#const FREETYPE_MINOR) :: Int
pattern FREETYPE_PATCH = (#const FREETYPE_PATCH) :: Int
#endif

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

data Generic = Generic
  { generic_data      :: {-# unpack #-} !(Ptr ())
  , generic_finalizer :: {-# unpack #-} !(FinalizerPtr ())
  } deriving (Eq,Show)

instance Storable Generic where
  sizeOf _ = #size FT_Generic
  alignment _ = #alignment FT_Generic
  peek p = Generic
    <$> (#peek FT_Generic, data) p
    <*> (#peek FT_Generic, finalizer) p
  poke p Generic{..} = do
    (#poke FT_Generic, data) p generic_data
    (#poke FT_Generic, finalizer) p generic_finalizer

data SizeMetrics = SizeMetrics
  { size_metrics_x_ppem
  , size_metrics_y_ppem :: {-# unpack #-} !Word16
  , size_metrics_x_scale
  , size_metrics_y_scale :: {-# unpack #-} !Fixed
  , size_metrics_ascender
  , size_metrics_descender
  , size_metrics_height
  , size_metrics_max_advance :: {-# unpack #-} !Pos
  } deriving (Eq,Show)

instance Storable SizeMetrics where
  sizeOf _ = #size FT_Size_Metrics
  alignment _ = #alignment FT_Size_Metrics
  peek p = SizeMetrics
    <$> (#peek FT_Size_Metrics, x_ppem) p
    <*> (#peek FT_Size_Metrics, y_ppem) p
    <*> (#peek FT_Size_Metrics, x_scale) p
    <*> (#peek FT_Size_Metrics, y_scale) p
    <*> (#peek FT_Size_Metrics, ascender) p
    <*> (#peek FT_Size_Metrics, descender) p
    <*> (#peek FT_Size_Metrics, height) p
    <*> (#peek FT_Size_Metrics, max_advance) p
  poke p SizeMetrics{..} = do
    (#poke FT_Size_Metrics, x_ppem) p size_metrics_x_ppem
    (#poke FT_Size_Metrics, y_ppem) p size_metrics_y_ppem
    (#poke FT_Size_Metrics, x_scale) p size_metrics_x_scale
    (#poke FT_Size_Metrics, y_scale) p size_metrics_y_scale
    (#poke FT_Size_Metrics, ascender) p size_metrics_ascender
    (#poke FT_Size_Metrics, descender) p size_metrics_descender
    (#poke FT_Size_Metrics, height) p size_metrics_height
    (#poke FT_Size_Metrics, max_advance) p size_metrics_max_advance

data SizeInternalRec

type Fixed = Word32
type Pos = Int32

data SizeRec = SizeRec
  { size_face :: {-# unpack #-} !(Ptr FaceRec)
  , size_generic :: {-# unpack #-} !Generic
  , size_metrics :: {-# unpack #-} !SizeMetrics
  , size_internal :: {-# unpack #-} !(Ptr SizeInternalRec)
  } deriving (Eq,Show)

instance Storable SizeRec where
  sizeOf _ = #size FT_SizeRec
  alignment _ = #alignment FT_SizeRec
  peek p = SizeRec
    <$> (#peek FT_SizeRec, face) p
    <*> (#peek FT_SizeRec, generic) p
    <*> (#peek FT_SizeRec, metrics) p
    <*> (#peek FT_SizeRec, internal) p
  poke p SizeRec{..} = do
    (#poke FT_SizeRec, face) p size_face
    (#poke FT_SizeRec, generic) p size_generic
    (#poke FT_SizeRec, metrics) p size_metrics
    (#poke FT_SizeRec, internal) p size_internal

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
    [ (C.TypeName "FT_Error",             [t|Error|])
    , (C.TypeName "FT_Face",              [t|Ptr FaceRec|])
    , (C.TypeName "FT_FaceRec_",          [t|FaceRec|])
    , (C.TypeName "FT_Generic",           [t|Generic|])
    , (C.TypeName "FT_Int",               [t|Int32|])
    , (C.TypeName "FT_Int32",             [t|Int32|])
    , (C.TypeName "FT_Library",           [t|Ptr LibraryRec|])
    , (C.TypeName "FT_LibraryRec_",       [t|LibraryRec|])
    , (C.TypeName "FT_Long",              [t|Int32|])
    , (C.TypeName "FT_Memory",            [t|Ptr MemoryRec|])
    , (C.TypeName "FT_MemoryRec_",        [t|MemoryRec|])
    , (C.TypeName "FT_Size_Internal",     [t|Ptr SizeInternalRec|])
    , (C.Struct   "FT_Size_InternalRec_", [t|SizeInternalRec|])
    , (C.TypeName "FT_Size_Metrics",      [t|SizeMetrics|])
    , (C.Struct   "FT_Size_Metrics_",     [t|SizeMetrics|])
    , (C.TypeName "FT_Size_Rec",          [t|SizeRec|])
    , (C.Struct   "FT_Size_Rec_",         [t|SizeRec|])
    , (C.TypeName "FT_UInt",              [t|Word32|])
    , (C.TypeName "FT_UInt32",            [t|Word32|])
    , (C.TypeName "FT_ULong",             [t|Word32|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char (Just C.Unsigned))) [t|Ptr CUChar|] [|withCUString|])
    , ("str", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char Nothing)) [t|Ptr CChar|] [|withCString|])
    , ("face", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Face") [t|Ptr FaceRec|] [|withForeignPtr|])
    , ("generic", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Generic") [t|Ptr Generic|] [|with|])
    , ("library", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Library") [t|Ptr LibraryRec|] [|withForeignPtr|])
    , ("sizemetrics", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Size_Metrics") [t|Ptr SizeMetrics|] [|with|])
    , ("sizerec", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Size_Rec") [t|Ptr SizeRec|] [|with|])
    ]
  }
