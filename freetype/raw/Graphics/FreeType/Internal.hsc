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
#include "hsc-struct.h"
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

, PixelMode
  ( PixelMode
  , PIXEL_MODE_NONE
  , PIXEL_MODE_MONO
  , PIXEL_MODE_GRAY
  , PIXEL_MODE_GRAY2
  , PIXEL_MODE_GRAY4
  , PIXEL_MODE_LCD
  , PIXEL_MODE_LCD_V
  , PIXEL_MODE_BGRA
  )

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

{-
data Bitmap = Bitmap
  { bitmap_rows
  , bitmap_width
  , bitmap_pitch
  , bitmap_buffer
  , bitmap_num_grays
  , bitmap_pixel_mode
  , bitmap_palette_mode
  , bitmap_palette
  } deriving (Eq,Show)
-}

#struct bitmapsize,BitmapSize,FT_Bitmap_Size,height,Int16,width,Int16,size,Pos,x_ppem,Pos,y_ppem,Pos
#struct generic,Generic,FT_Generic,data,Ptr (),finalizer,FinalizerPtr ()
#struct matrix,Matrix,FT_Matrix,xx,Fixed,xy,Fixed,yx,Fixed,yy,Fixed
#struct memory,MemoryRec,struct FT_MemoryRec_,user,Ptr(),alloc,FunPtr AllocFunc,free,FunPtr FreeFunc,realloc,FunPtr ReallocFunc
#struct sizemetrics,SizeMetrics,FT_Size_Metrics,x_ppem,Word16,y_ppem,Word16,x_scale,Fixed,y_scale,Fixed,ascender,Pos,descender,Pos,height,Pos,max_advance,Pos
#struct size,SizeRec,FT_SizeRec,face,Ptr FaceRec,generic,Generic,metrics,SizeMetrics,internal,Ptr SizeInternalRec
#struct sizerequest,SizeRequestRec,FT_Size_RequestRec,type,SizeRequestType,width,Int32,height,Int32,horiResolution,Word32,vertResolution,Word32
#struct vector,Vector,FT_Vector,x,Pos,y,Pos

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


data SizeInternalRec
type Size = ForeignPtr SizeRec

type Pos = Int32

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

newtype PixelMode = PixelMode Int32 deriving newtype (Eq,Show,Storable,Prim)
#pattern PIXEL_MODE_NONE, PixelMode
#pattern PIXEL_MODE_MONO, PixelMode
#pattern PIXEL_MODE_GRAY, PixelMode
#pattern PIXEL_MODE_GRAY2, PixelMode
#pattern PIXEL_MODE_GRAY4, PixelMode
#pattern PIXEL_MODE_LCD, PixelMode
#pattern PIXEL_MODE_LCD_V, PixelMode
#pattern PIXEL_MODE_BGRA, PixelMode

type SizeRequest = Ptr SizeRequestRec


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
