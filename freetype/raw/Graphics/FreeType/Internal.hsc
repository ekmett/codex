{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language TypeFamilies #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language StrictData #-}
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
{-# options_ghc -funbox-strict-fields #-}

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
#let pattern n,t = "pattern %s = %s (%d)",#n,#t,(int)(FT_ ## n)
#endif

module Graphics.FreeType.Internal
(

  Angle
, pattern ANGLE_PI
, pattern ANGLE_2PI
, pattern ANGLE_PI2
, pattern ANGLE_PI4
, angleDiff

, BitmapSize(..)

, Error
  ( Error
#ifndef HLINT
#err_exports
#endif
  )
, ok

, Face
, FaceRec
, foreignFace

, Generic(..)

, GlyphSlot
, GlyphSlotRec

, KerningMode
  ( KerningMode
  , KERNING_DEFAULT
  , KERNING_UNFITTED
  , KERNING_UNSCALED
  )
, Library
, LibraryRec
, foreignLibrary
, pattern FREETYPE_MAJOR
, pattern FREETYPE_MINOR
, pattern FREETYPE_PATCH

, Matrix(..)
, matrixInvert, matrixMultiply

, Memory
, MemoryRec(..)
, AllocFunc, FreeFunc, ReallocFunc

, Pos

, RenderMode
  ( RenderMode
  , RENDER_MODE_NORMAL
  , RENDER_MODE_LIGHT
  , RENDER_MODE_MONO
  , RENDER_MODE_LCD
  , RENDER_MODE_LCD_V
  )

, Size
, SizeRec(..)
, SizeMetrics(..)
, SizeInternalRec

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
, childPtr
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
import Foreign.ForeignPtr.Unsafe
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

data BitmapSize = BitmapSize
  { bitmapsize_height
  , bitmapsize_width :: Int16
  , bitmapsize_size
  , bitmapsize_x_ppem
  , bitmapsize_y_ppem :: Pos
  } deriving (Eq,Show)

instance Storable BitmapSize where
  sizeOf _ = #size FT_Bitmap_Size
  alignment _ = #alignment FT_Bitmap_Size
  peek p = BitmapSize
    <$> (#peek FT_Bitmap_Size, height) p
    <*> (#peek FT_Bitmap_Size, width) p
    <*> (#peek FT_Bitmap_Size, size) p
    <*> (#peek FT_Bitmap_Size, x_ppem) p
    <*> (#peek FT_Bitmap_Size, y_ppem) p
  poke p BitmapSize{..} = do
    (#poke FT_Bitmap_Size, height) p bitmapsize_height
    (#poke FT_Bitmap_Size, width) p bitmapsize_width
    (#poke FT_Bitmap_Size, size) p bitmapsize_size
    (#poke FT_Bitmap_Size, x_ppem) p bitmapsize_x_ppem
    (#poke FT_Bitmap_Size, y_ppem) p bitmapsize_y_ppem

-- | Given a foreign ptr and a ptr, produces a foreign ptr that has the same finalizers as the first, but now
-- pointing at the target value. Holding this foreign ptr will keep the original alive and vice versa.
--
-- This can be used when accessing, say, a part of a whole that has a shared lifetime.
childPtr :: ForeignPtr a -> Ptr b -> ForeignPtr b
childPtr fp p = fp `plusForeignPtr` minusPtr p (unsafeForeignPtrToPtr fp)

-- | By convention the library will throw any non-0 FT_Error encountered.
newtype Error = Error CInt deriving newtype (Eq,Ord,Show,Storable)

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

type Face = ForeignPtr FaceRec
data FaceRec

#ifndef HLINT
pattern FREETYPE_MAJOR = (#const FREETYPE_MAJOR) :: Int
pattern FREETYPE_MINOR = (#const FREETYPE_MINOR) :: Int
pattern FREETYPE_PATCH = (#const FREETYPE_PATCH) :: Int
#endif

data Generic = Generic
  { generic_data      :: Ptr ()
  , generic_finalizer :: FinalizerPtr ()
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
  , matrix_yx, matrix_yy :: Fixed
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

type GlyphSlot = ForeignPtr GlyphSlotRec
data GlyphSlotRec

-- Note: the library inconsistently passes these as an FT_UInt to FT_Get_Kerning
-- but the type itself is an int
newtype KerningMode = KerningMode Word32 deriving (Eq,Ord,Show)

#pattern KERNING_DEFAULT, KerningMode
#pattern KERNING_UNFITTED, KerningMode
#pattern KERNING_UNSCALED, KerningMode

type Library = ForeignPtr LibraryRec
data LibraryRec

type AllocFunc = Memory -> CLong -> IO (Ptr ())
type FreeFunc = Memory -> Ptr () -> IO ()
type ReallocFunc = Memory -> CLong -> CLong -> Ptr () -> IO (Ptr ())

type Memory = ForeignPtr MemoryRec
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

data SizeMetrics = SizeMetrics
  { sizemetrics_x_ppem
  , sizemetrics_y_ppem :: Word16
  , sizemetrics_x_scale
  , sizemetrics_y_scale :: Fixed
  , sizemetrics_ascender
  , sizemetrics_descender
  , sizemetrics_height
  , sizemetrics_max_advance :: Pos
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
    (#poke FT_Size_Metrics, x_ppem) p sizemetrics_x_ppem
    (#poke FT_Size_Metrics, y_ppem) p sizemetrics_y_ppem
    (#poke FT_Size_Metrics, x_scale) p sizemetrics_x_scale
    (#poke FT_Size_Metrics, y_scale) p sizemetrics_y_scale
    (#poke FT_Size_Metrics, ascender) p sizemetrics_ascender
    (#poke FT_Size_Metrics, descender) p sizemetrics_descender
    (#poke FT_Size_Metrics, height) p sizemetrics_height
    (#poke FT_Size_Metrics, max_advance) p sizemetrics_max_advance

data SizeInternalRec

type Pos = Int32

type Size = ForeignPtr SizeRec
data SizeRec = SizeRec
  { size_face     :: Ptr FaceRec
  , size_generic  :: Generic
  , size_metrics  :: SizeMetrics
  , size_internal :: Ptr SizeInternalRec
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

#pattern SIZE_REQUEST_TYPE_NOMINAL, SizeRequestType
#pattern SIZE_REQUEST_TYPE_REAL_DIM, SizeRequestType
#pattern SIZE_REQUEST_TYPE_BBOX, SizeRequestType
#pattern SIZE_REQUEST_TYPE_CELL, SizeRequestType
#pattern SIZE_REQUEST_TYPE_SCALES, SizeRequestType

newtype RenderMode = RenderMode Int32 deriving newtype (Eq,Show,Storable,Prim)
#pattern RENDER_MODE_NORMAL, RenderMode
#pattern RENDER_MODE_LIGHT, RenderMode
#pattern RENDER_MODE_MONO, RenderMode
#pattern RENDER_MODE_LCD, RenderMode
#pattern RENDER_MODE_LCD_V, RenderMode

type SizeRequest = Ptr SizeRequestRec

data SizeRequestRec = SizeRequestRec
  { sizerequest_type  :: SizeRequestType
  , sizerequest_width
  , sizerequest_height :: Int32
  , sizerequest_horiResolution
  , sizerequest_vertResolution :: Word32
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
    (#poke FT_Size_RequestRec, type) ptr sizerequest_type
    (#poke FT_Size_RequestRec, width) ptr sizerequest_width
    (#poke FT_Size_RequestRec, height) ptr sizerequest_height
    (#poke FT_Size_RequestRec, horiResolution) ptr sizerequest_horiResolution
    (#poke FT_Size_RequestRec, vertResolution) ptr sizerequest_vertResolution

data Vector = Vector
  { vector_x, vector_y :: Pos
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
foreignFace = newForeignPtr [C.funPtr|void free_face(FT_Face f) { FT_Done_Face(f); }|]

-- | Assumes we are using @FT_New_Library@ management rather than @FT_Init_FreeType@ management
foreignLibrary :: Ptr LibraryRec -> IO Library
foreignLibrary = newForeignPtr [C.funPtr|void free_library(FT_Library l) { FT_Done_Library(l); }|]

freeTypeCtx :: C.Context
freeTypeCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "FT_Error",              [t|Error|])
    , (C.TypeName "FT_Face",               [t|Ptr FaceRec|])
    , (C.TypeName "FT_FaceRec_",           [t|FaceRec|])
    , (C.TypeName "FT_Generic",            [t|Generic|])
    , (C.TypeName "FT_GlyphSlot",          [t|Ptr GlyphSlotRec|])
    , (C.TypeName "FT_GlyphSlotRec",       [t|GlyphSlotRec|])
    , (C.Struct   "FT_GlyphSlotRec_",      [t|GlyphSlotRec|])
    , (C.TypeName "FT_Int",                [t|Int32|])
    , (C.TypeName "FT_Int32",              [t|Int32|])
    , (C.TypeName "FT_KerningMode",        [t|KerningMode|])
    , (C.Enum     "FT_KerningMode_",       [t|KerningMode|])
    , (C.TypeName "FT_Library",            [t|Ptr LibraryRec|])
    , (C.TypeName "FT_LibraryRec_",        [t|LibraryRec|])
    , (C.TypeName "FT_Long",               [t|Int32|])
    , (C.TypeName "FT_Matrix",             [t|Matrix|])
    , (C.Struct   "FT_Matrix_",            [t|Matrix|])
    , (C.TypeName "FT_Memory",             [t|Ptr MemoryRec|])
    , (C.TypeName "FT_MemoryRec_",         [t|MemoryRec|])
    , (C.TypeName "FT_Render_Mode",        [t|RenderMode|])
    , (C.Enum     "FT_Render_Mode_",       [t|RenderMode|])
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
    , ("glyph-slot", anti (C.TypeSpecifier mempty $ C.TypeName "FT_GlyphSlot") [t|Ptr GlyphSlotRec|] [|withForeignPtr|])
    , ("generic", anti (ptr $ C.TypeName "FT_Generic") [t|Ptr Generic|] [|with|])
    , ("matrix",  anti (ptr $ C.TypeName "FT_Matrix") [t|Ptr Matrix|] [|with|])
    , ("vector",  anti (ptr $ C.TypeName "FT_Vector") [t|Ptr Vector|] [|with|])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
