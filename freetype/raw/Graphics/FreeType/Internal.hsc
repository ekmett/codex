{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language OverloadedStrings #-}
{-# language ForeignFunctionInterface #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
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
#include FT_TRIGONOMETRY_H
#include "hsc-err.h"
#let err_exports = err_exports()
#let err_patterns = err_patterns()
#endif

module Graphics.FreeType.Internal
( Angle
, pattern ANGLE_PI
, pattern ANGLE_2PI
, pattern ANGLE_PI2
, pattern ANGLE_PI4
, angleDiff

, Error
  ( Error
#ifndef HLINT
#err_exports
#endif
  )
, ok

, Face(..)
, FaceRec
, foreignFace

, Generic(..)

, Library(..)
, LibraryRec
, foreignLibrary
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

, Matrix(..)
, matrixInvert, matrixMultiply

, Memory(..)
, MemoryRec(..)
, AllocFunc, FreeFunc, ReallocFunc

, Pos

, SizeRec(..), SizeMetrics(..), SizeInternalRec

, SizeRequest
, SizeRequestRec(..)
, SizeRequestType
  ( SizeRequestType
  , SIZE_REQUEST_TYPE_NOMINAL
  , SIZE_REQUEST_TYPE_REAL_DIM
  , SIZE_REQUEST_TYPE_BBOX
  , SIZE_REQUEST_TYPE_CELL
  , SIZE_REQUEST_TYPE_SCALES
  )

, Vector(..)
, vectorTransform

-- * contexts
, freeTypeCtx
) where

import Control.Exception
import Data.Default
import Data.Int
import qualified Data.Map as Map
import Data.Primitive.Types
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import qualified Foreign.Concurrent as Concurrent
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Numeric.Fixed
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import Graphics.FreeType.Private

type Angle = Fixed
pattern ANGLE_PI  = Fixed (#const FT_ANGLE_PI)
pattern ANGLE_2PI = Fixed (#const FT_ANGLE_2PI)
pattern ANGLE_PI2 = Fixed (#const FT_ANGLE_PI2)
pattern ANGLE_PI4 = Fixed (#const FT_ANGLE_PI4)

-- | By convention the library will throw any non-0 FT_Error encountered.
newtype Error = Error CInt deriving newtype (Eq,Ord,Show,Num,Enum,Real,Integral,Storable)

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

data Matrix = Matrix
  { matrix_xx, matrix_xy
  , matrix_yx, matrix_yy :: {-# unpack #-} !Fixed
  } deriving (Eq,Show)

instance Storable Matrix where
  sizeOf _    = #size FT_Matrix
  alignment _ = #alignment FT_Matrix
  peek ptr = Matrix
    <$> (#peek FT_Matrix, xx) ptr
    <*> (#peek FT_Matrix, xy) ptr
    <*> (#peek FT_Matrix, yx) ptr
    <*> (#peek FT_Matrix, yy) ptr
  poke ptr Matrix{..} = do
    (#poke FT_Matrix, xx) ptr matrix_xx
    (#poke FT_Matrix, xy) ptr matrix_xy
    (#poke FT_Matrix, yx) ptr matrix_yx
    (#poke FT_Matrix, yy) ptr matrix_yy

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

newtype SizeRequestType = SizeRequestType Int32 deriving newtype (Eq,Show,Storable,Prim)

pattern SIZE_REQUEST_TYPE_NOMINAL = SizeRequestType (#const FT_SIZE_REQUEST_TYPE_NOMINAL)
pattern SIZE_REQUEST_TYPE_REAL_DIM = SizeRequestType (#const FT_SIZE_REQUEST_TYPE_REAL_DIM)
pattern SIZE_REQUEST_TYPE_BBOX = SizeRequestType (#const FT_SIZE_REQUEST_TYPE_BBOX)
pattern SIZE_REQUEST_TYPE_CELL = SizeRequestType (#const FT_SIZE_REQUEST_TYPE_CELL)
pattern SIZE_REQUEST_TYPE_SCALES = SizeRequestType (#const FT_SIZE_REQUEST_TYPE_SCALES)

type SizeRequest = Ptr SizeRequestRec

data SizeRequestRec = SizeRequestRec
  { size_request_type  :: {-# unpack #-} !SizeRequestType
  , size_request_width
  , size_request_height :: {-# unpack #-} !Int32
  , size_request_horiResolution
  , size_request_vertResolution :: {-# unpack #-} !Word32
  } deriving (Eq,Show)

instance Storable SizeRequestRec where
  sizeOf    _ = #size FT_Size_RequestRec
  alignment _ = #alignment FT_Size_RequestRec
  peek ptr = SizeRequestRec
    <$> (#peek FT_Size_RequestRec, type) ptr
    <*> (#peek FT_Size_RequestRec, width) ptr
    <*> (#peek FT_Size_RequestRec, height) ptr
    <*> (#peek FT_Size_RequestRec, horiResolution) ptr
    <*> (#peek FT_Size_RequestRec, vertResolution) ptr
  poke ptr SizeRequestRec{..} = do
    (#poke FT_Size_RequestRec, type) ptr size_request_type
    (#poke FT_Size_RequestRec, width) ptr size_request_width
    (#poke FT_Size_RequestRec, height) ptr size_request_height
    (#poke FT_Size_RequestRec, horiResolution) ptr size_request_horiResolution
    (#poke FT_Size_RequestRec, vertResolution) ptr size_request_vertResolution

data Vector = Vector
  { vector_x, vector_y :: {-# unpack #-} !Pos
  } deriving (Eq, Show)

instance Storable Vector where
  sizeOf _    = #size FT_Vector
  alignment _ = #alignment FT_Vector
  peek ptr = Vector
    <$> (#peek FT_Vector, x) ptr
    <*> (#peek FT_Vector, y) ptr
  poke ptr Vector{..} = do
    (#poke FT_Vector, x) ptr vector_x
    (#poke FT_Vector, y) ptr vector_y

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Angle", [t|Fixed|])
    , (C.TypeName "FT_Error", [t|Error|])
    , (C.TypeName "FT_Face", [t|Ptr FaceRec|])
    , (C.TypeName "FT_Fixed", [t|Fixed|])
    , (C.TypeName "FT_Library", [t|Ptr LibraryRec|])
    , (C.TypeName "FT_Matrix", [t|Matrix|])
    , (C.TypeName "FT_Vector", [t|Vector|])
    ]
  }

C.include "<ft2build.h>"
C.verbatim "#include FT_FREETYPE_H"
C.verbatim "#include FT_GLYPH_H"
C.verbatim "#include FT_MODULE_H"
C.verbatim "#include FT_TYPES_H"
C.verbatim "#include FT_TRIGONOMETRY_H"

angleDiff :: Angle -> Angle -> Angle
angleDiff angle1 angle2 = [C.pure|FT_Angle { FT_Angle_Diff($(FT_Angle angle1),$(FT_Angle angle2)) }|]

matrixInvert:: Matrix -> Maybe Matrix
matrixInvert m = unsafeLocalState $
  with m $ \mm -> do
    e <- [C.exp|FT_Error { FT_Matrix_Invert($(FT_Matrix * mm))}|]
    if e == Err_Ok then Just <$> peek mm else pure Nothing

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply m n = unsafeLocalState $
   with m $ \mm ->
   with n $ \nm ->
    [C.block|void {
      FT_Matrix_Multiply($(FT_Matrix * mm),$(FT_Matrix * nm));
    }|] *> peek nm

instance Semigroup Matrix where
  (<>) = matrixMultiply

instance Monoid Matrix where
  mempty = Matrix 1 0 0 1

instance Default Matrix where
  def = Matrix 1 0 0 1

vectorTransform :: Vector -> Matrix -> Vector
vectorTransform v m = unsafeLocalState $
  with v $ \vp ->
    with m $ \mp ->
      [C.block|void { FT_Vector_Transform($(FT_Vector * vp),$(FT_Matrix * mp)); }|] *> peek vp

instance Default Vector where
  def = Vector 0 0

foreignFace :: Ptr FaceRec -> IO Face
foreignFace p = Face <$> Concurrent.newForeignPtr p ([C.exp|FT_Error { FT_Done_Face($(FT_Face p)) }|] >>= ok)

-- | Assumes we are using @FT_New_Library@ management rather than @FT_Init_FreeType@ management
foreignLibrary :: Ptr LibraryRec -> IO Library
foreignLibrary p = Library <$> Concurrent.newForeignPtr p ([C.exp|FT_Error { FT_Done_Library($(FT_Library p)) }|] >>= ok)

freeTypeCtx :: C.Context
freeTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Error",              [t|Error|])
    , (C.TypeName "FT_Face",               [t|Ptr FaceRec|])
    , (C.TypeName "FT_FaceRec_",           [t|FaceRec|])
    , (C.TypeName "FT_Generic",            [t|Generic|])
    , (C.TypeName "FT_Int",                [t|Int32|])
    , (C.TypeName "FT_Int32",              [t|Int32|])
    , (C.TypeName "FT_Library",            [t|Ptr LibraryRec|])
    , (C.TypeName "FT_LibraryRec_",        [t|LibraryRec|])
    , (C.TypeName "FT_Long",               [t|Int32|])
    , (C.TypeName "FT_Matrix",             [t|Matrix|])
    , (C.Struct   "FT_Matrix_",            [t|Matrix|])
    , (C.TypeName "FT_Memory",             [t|Ptr MemoryRec|])
    , (C.TypeName "FT_MemoryRec_",         [t|MemoryRec|])
    , (C.TypeName "FT_Size_Internal",      [t|Ptr SizeInternalRec|])
    , (C.Struct   "FT_Size_InternalRec_",  [t|SizeInternalRec|])
    , (C.TypeName "FT_Size_Metrics",       [t|SizeMetrics|])
    , (C.Struct   "FT_Size_Metrics_",      [t|SizeMetrics|])
    , (C.TypeName "FT_Size_Rec",           [t|SizeRec|])
    , (C.Struct   "FT_Size_Rec_",          [t|SizeRec|])
    , (C.Struct   "FT_Size_Request",       [t|Ptr SizeRequestRec|])
    , (C.Struct   "FT_Size_RequestRec_",   [t|SizeRequestRec|])
    , (C.TypeName "FT_Size_RequestRec",    [t|SizeRequestRec|])
    , (C.TypeName "FT_Size_Request_Type",  [t|SizeRequestType|])
    , (C.Enum     "FT_Size_Request_Type_", [t|SizeRequestType|])
    , (C.Struct   "FT_Size_Rec_",          [t|SizeRec|])
    , (C.TypeName "FT_UInt",               [t|Word32|])
    , (C.TypeName "FT_UInt32",             [t|Word32|])
    , (C.TypeName "FT_ULong",              [t|Word32|])
    , (C.TypeName "FT_Vector",             [t|Vector|])
    , (C.Struct   "FT_Vector_",            [t|Vector|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char (Just C.Unsigned))) [t|Ptr CUChar|] [|withCUString|])
    , ("str", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char Nothing)) [t|Ptr CChar|] [|withCString|])
    , ("face", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Face") [t|Ptr FaceRec|] [|withForeignPtr|])
    , ("library", anti (C.TypeSpecifier mempty $ C.TypeName "FT_Library") [t|Ptr LibraryRec|] [|withForeignPtr|])
    , ("generic", anti (ptr $ C.TypeName "FT_Generic") [t|Ptr Generic|] [|with|])
    , ("matrix",  anti (ptr $ C.TypeName "FT_Matrix") [t|Ptr Matrix|] [|with|])
    , ("vector",  anti (ptr $ C.TypeName "FT_Vector") [t|Ptr Vector|] [|with|])
    ] 
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
