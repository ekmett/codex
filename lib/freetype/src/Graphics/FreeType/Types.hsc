{-# language GeneralizedNewtypeDeriving #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language DuplicateRecordFields #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}
{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}

module Graphics.FreeType.Types where

import Foreign hiding (free, realloc, shift)
import Foreign.C.Types
import Foreign.C.String

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_IMAGE_H
#include FT_GLYPH_H
#include FT_OUTLINE_H
#include FT_SYSTEM_H

type Fixed = CLong
type Pos = CLong
type F26Dot6 = CLong
type Offset = #type size_t
type Error = CInt
type Byte = CUChar
type Bytes = Ptr Byte

type FT_Char = CChar
type FT_Bool = CUChar

data DriverRec_
type Driver = Ptr DriverRec_

--data FaceRec_
--type Face = Ptr FaceRec_
newtype Face = Face { getFace :: ForeignPtr Face }

-- data LibraryRec_
-- type Library = Ptr LibraryRec_

newtype Library = Library { getLibrary :: ForeignPtr Library }

data ModuleRec_
type Module = Ptr ModuleRec_

data RendererRec_
type Renderer = Ptr RendererRec_

data SubGlyphRec_
type SubGlyph = Ptr SubGlyphRec_

pattern LOAD_DEFAULT :: Int32
pattern LOAD_DEFAULT = #const FT_LOAD_DEFAULT

pattern LOAD_NO_SCALE :: Int32
pattern LOAD_NO_SCALE = #const FT_LOAD_NO_SCALE

pattern LOAD_NO_HINTING :: Int32
pattern LOAD_NO_HINTING = #const FT_LOAD_NO_HINTING

pattern LOAD_RENDER :: Int32
pattern LOAD_RENDER = #const FT_LOAD_RENDER

pattern LOAD_NO_BITMAP :: Int32
pattern LOAD_NO_BITMAP = #const FT_LOAD_NO_BITMAP

pattern LOAD_VERTICAL_LAYOUT :: Int32
pattern LOAD_VERTICAL_LAYOUT = #const FT_LOAD_VERTICAL_LAYOUT

pattern LOAD_FORCE_AUTOHINT :: Int32
pattern LOAD_FORCE_AUTOHINT = #const FT_LOAD_FORCE_AUTOHINT

pattern LOAD_CROP_BITMAP :: Int32
pattern LOAD_CROP_BITMAP = #const FT_LOAD_CROP_BITMAP

pattern LOAD_PEDANTIC :: Int32
pattern LOAD_PEDANTIC = #const FT_LOAD_PEDANTIC

pattern LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH :: Int32
pattern LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = #const FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH

pattern LOAD_NO_RECURSE :: Int32
pattern LOAD_NO_RECURSE = #const FT_LOAD_NO_RECURSE

pattern LOAD_IGNORE_TRANSFORM :: Int32
pattern LOAD_IGNORE_TRANSFORM = #const FT_LOAD_IGNORE_TRANSFORM

pattern LOAD_MONOCHROME :: Int32
pattern LOAD_MONOCHROME = #const FT_LOAD_MONOCHROME

pattern LOAD_LINEAR_DESIGN :: Int32
pattern LOAD_LINEAR_DESIGN = #const FT_LOAD_LINEAR_DESIGN

pattern LOAD_NO_AUTOHINT :: Int32
pattern LOAD_NO_AUTOHINT = #const FT_LOAD_NO_AUTOHINT

newtype RenderMode = RenderMode CUInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern RENDER_MODE_NORMAL :: RenderMode
pattern RENDER_MODE_LIGHT :: RenderMode
pattern RENDER_MODE_MONO :: RenderMode
pattern RENDER_MODE_LCD :: RenderMode
pattern RENDER_MODE_LCD_V :: RenderMode

pattern RENDER_MODE_NORMAL = #const FT_RENDER_MODE_NORMAL
pattern RENDER_MODE_LIGHT = #const FT_RENDER_MODE_LIGHT
pattern RENDER_MODE_MONO = #const FT_RENDER_MODE_MONO
pattern RENDER_MODE_LCD = #const FT_RENDER_MODE_LCD
pattern RENDER_MODE_LCD_V = #const FT_RENDER_MODE_LCD_V

pattern FREETYPE_MAJOR :: CInt
pattern FREETYPE_MAJOR = #const FREETYPE_MAJOR

pattern FREETYPE_MINOR :: CInt
pattern FREETYPE_MINOR = #const FREETYPE_MINOR

pattern FREETYPE_PATCH :: CInt
pattern FREETYPE_PATCH = #const FREETYPE_PATCH

newtype Encoding = Encoding Word32 deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern ENCODING_NONE           :: Encoding
pattern ENCODING_MS_SYMBOL      :: Encoding
pattern ENCODING_UNICODE        :: Encoding
pattern ENCODING_SJIS           :: Encoding
pattern ENCODING_GB2312         :: Encoding
pattern ENCODING_BIG5           :: Encoding
pattern ENCODING_WANSUNG        :: Encoding
pattern ENCODING_JOHAB          :: Encoding
pattern ENCODING_MS_SJIS        :: Encoding
pattern ENCODING_MS_GB2312      :: Encoding
pattern ENCODING_MS_BIG5        :: Encoding
pattern ENCODING_MS_WANSUNG     :: Encoding
pattern ENCODING_MS_JOHAB       :: Encoding
pattern ENCODING_ADOBE_STANDARD :: Encoding
pattern ENCODING_ADOBE_EXPERT   :: Encoding
pattern ENCODING_ADOBE_CUSTOM   :: Encoding
pattern ENCODING_ADOBE_LATIN_1  :: Encoding
pattern ENCODING_OLD_LATIN_2    :: Encoding
pattern ENCODING_APPLE_ROMAN    :: Encoding

pattern ENCODING_NONE           = #const FT_ENCODING_NONE
pattern ENCODING_MS_SYMBOL      = #const FT_ENCODING_MS_SYMBOL
pattern ENCODING_UNICODE        = #const FT_ENCODING_UNICODE
pattern ENCODING_SJIS           = #const FT_ENCODING_SJIS
pattern ENCODING_GB2312         = #const FT_ENCODING_GB2312
pattern ENCODING_BIG5           = #const FT_ENCODING_BIG5
pattern ENCODING_WANSUNG        = #const FT_ENCODING_WANSUNG
pattern ENCODING_JOHAB          = #const FT_ENCODING_JOHAB
pattern ENCODING_MS_SJIS        = #const FT_ENCODING_MS_SJIS
pattern ENCODING_MS_GB2312      = #const FT_ENCODING_MS_GB2312
pattern ENCODING_MS_BIG5        = #const FT_ENCODING_MS_BIG5
pattern ENCODING_MS_WANSUNG     = #const FT_ENCODING_MS_WANSUNG
pattern ENCODING_MS_JOHAB       = #const FT_ENCODING_MS_JOHAB
pattern ENCODING_ADOBE_STANDARD = #const FT_ENCODING_ADOBE_STANDARD
pattern ENCODING_ADOBE_EXPERT   = #const FT_ENCODING_ADOBE_EXPERT
pattern ENCODING_ADOBE_CUSTOM   = #const FT_ENCODING_ADOBE_CUSTOM
pattern ENCODING_ADOBE_LATIN_1  = #const FT_ENCODING_ADOBE_LATIN_1
pattern ENCODING_OLD_LATIN_2    = #const FT_ENCODING_OLD_LATIN_2
pattern ENCODING_APPLE_ROMAN    = #const FT_ENCODING_APPLE_ROMAN

newtype FaceFlag = FaceFlag CInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern FACE_FLAG_SCALABLE :: FaceFlag
pattern FACE_FLAG_SCALABLE = #const FT_FACE_FLAG_SCALABLE
pattern FACE_FLAG_FIXED_SIZES :: FaceFlag
pattern FACE_FLAG_FIXED_SIZES = #const FT_FACE_FLAG_FIXED_SIZES
pattern FACE_FLAG_FIXED_WIDTH :: FaceFlag
pattern FACE_FLAG_FIXED_WIDTH = #const FT_FACE_FLAG_FIXED_WIDTH
pattern FACE_FLAG_SFNT :: FaceFlag
pattern FACE_FLAG_SFNT = #const FT_FACE_FLAG_SFNT
pattern FACE_FLAG_HORIZONTAL :: FaceFlag
pattern FACE_FLAG_HORIZONTAL = #const FT_FACE_FLAG_HORIZONTAL
pattern FACE_FLAG_VERTICAL :: FaceFlag
pattern FACE_FLAG_VERTICAL = #const FT_FACE_FLAG_VERTICAL
pattern FACE_FLAG_KERNING :: FaceFlag
pattern FACE_FLAG_KERNING = #const FT_FACE_FLAG_KERNING
pattern FACE_FLAG_FAST_GLYPHS :: FaceFlag
pattern FACE_FLAG_FAST_GLYPHS = #const FT_FACE_FLAG_FAST_GLYPHS
pattern FACE_FLAG_MULTIPLE_MASTERS :: FaceFlag
pattern FACE_FLAG_MULTIPLE_MASTERS = #const FT_FACE_FLAG_MULTIPLE_MASTERS
pattern FACE_FLAG_GLYPH_NAMES :: FaceFlag
pattern FACE_FLAG_GLYPH_NAMES = #const FT_FACE_FLAG_GLYPH_NAMES
pattern FACE_FLAG_EXTERNAL_STREAM :: FaceFlag
pattern FACE_FLAG_EXTERNAL_STREAM = #const FT_FACE_FLAG_EXTERNAL_STREAM
pattern FACE_FLAG_HINTER :: FaceFlag
pattern FACE_FLAG_HINTER = #const FT_FACE_FLAG_HINTER
pattern FACE_FLAG_CID_KEYED :: FaceFlag
pattern FACE_FLAG_CID_KEYED = #const FT_FACE_FLAG_CID_KEYED
pattern FACE_FLAG_TRICKY :: FaceFlag
pattern FACE_FLAG_TRICKY = #const FT_FACE_FLAG_TRICKY

newtype Open = Open CUInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern OPEN_MEMORY :: Open
pattern OPEN_MEMORY = #const FT_OPEN_MEMORY
pattern OPEN_STREAM :: Open
pattern OPEN_STREAM = #const FT_OPEN_STREAM
pattern OPEN_PATHNAME :: Open
pattern OPEN_PATHNAME = #const FT_OPEN_PATHNAME
pattern OPEN_DRIVER :: Open
pattern OPEN_DRIVER = #const FT_OPEN_DRIVER
pattern OPEN_PARAMS :: Open
pattern OPEN_PARAMS = #const FT_OPEN_PARAMS

newtype SizeRequestType = SizeRequestType CUInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern SIZE_REQUEST_TYPE_NOMINAL :: SizeRequestType
pattern SIZE_REQUEST_TYPE_NOMINAL = #const FT_SIZE_REQUEST_TYPE_NOMINAL
pattern SIZE_REQUEST_TYPE_REAL_DIM :: SizeRequestType
pattern SIZE_REQUEST_TYPE_REAL_DIM = #const FT_SIZE_REQUEST_TYPE_REAL_DIM
pattern SIZE_REQUEST_TYPE_BBOX :: SizeRequestType
pattern SIZE_REQUEST_TYPE_BBOX = #const FT_SIZE_REQUEST_TYPE_BBOX
pattern SIZE_REQUEST_TYPE_CELL :: SizeRequestType
pattern SIZE_REQUEST_TYPE_CELL = #const FT_SIZE_REQUEST_TYPE_CELL
pattern SIZE_REQUEST_TYPE_SCALES :: SizeRequestType
pattern SIZE_REQUEST_TYPE_SCALES = #const FT_SIZE_REQUEST_TYPE_SCALES

pattern LOAD_TARGET_NORMAL :: Word32
pattern LOAD_TARGET_NORMAL = #const FT_LOAD_TARGET_NORMAL  
pattern LOAD_TARGET_LIGHT :: Word32
pattern LOAD_TARGET_LIGHT = #const FT_LOAD_TARGET_LIGHT
pattern LOAD_TARGET_MONO :: Word32
pattern LOAD_TARGET_MONO = #const FT_LOAD_TARGET_MONO
pattern LOAD_TARGET_LCD :: Word32
pattern LOAD_TARGET_LCD = #const FT_LOAD_TARGET_LCD
pattern LOAD_TARGET_LCD_V :: Word32
pattern LOAD_TARGET_LCD_V = #const FT_LOAD_TARGET_LCD_V

newtype KerningMode = KerningMode Word32 deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern KERNING_DEFAULT :: KerningMode
pattern KERNING_DEFAULT  = #const FT_KERNING_DEFAULT
pattern KERNING_UNFITTED :: KerningMode
pattern KERNING_UNFITTED = #const FT_KERNING_UNFITTED
pattern KERNING_UNSCALED :: KerningMode
pattern KERNING_UNSCALED = #const FT_KERNING_UNSCALED

newtype SubglyphFlag = SubglyphFlag CUInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern SUBGLYPH_FLAG_ARGS_ARE_WORDS :: SubglyphFlag
pattern SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES :: SubglyphFlag
pattern SUBGLYPH_FLAG_ROUND_XY_TO_GRID :: SubglyphFlag
pattern SUBGLYPH_FLAG_SCALE :: SubglyphFlag
pattern SUBGLYPH_FLAG_XY_SCALE :: SubglyphFlag
pattern SUBGLYPH_FLAG_2X2 :: SubglyphFlag
pattern SUBGLYPH_FLAG_USE_MY_METRICS :: SubglyphFlag

pattern SUBGLYPH_FLAG_ARGS_ARE_WORDS = #const FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS
pattern SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES = #const FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES
pattern SUBGLYPH_FLAG_ROUND_XY_TO_GRID = #const FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID
pattern SUBGLYPH_FLAG_SCALE = #const FT_SUBGLYPH_FLAG_SCALE
pattern SUBGLYPH_FLAG_XY_SCALE = #const FT_SUBGLYPH_FLAG_XY_SCALE
pattern SUBGLYPH_FLAG_2X2 = #const FT_SUBGLYPH_FLAG_2X2
pattern SUBGLYPH_FLAG_USE_MY_METRICS = #const FT_SUBGLYPH_FLAG_USE_MY_METRICS

pattern FSTYPE_INSTALLABLE_EMBEDDING :: CUShort
pattern FSTYPE_INSTALLABLE_EMBEDDING = #const FT_FSTYPE_INSTALLABLE_EMBEDDING
pattern FSTYPE_RESTRICTED_LICENSE_EMBEDDING :: CUShort
pattern FSTYPE_RESTRICTED_LICENSE_EMBEDDING = #const FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING
pattern FSTYPE_PREVIEW_AND_PRINT_EMBEDDING :: CUShort
pattern FSTYPE_PREVIEW_AND_PRINT_EMBEDDING = #const FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING
pattern FSTYPE_EDITABLE_EMBEDDING :: CUShort
pattern FSTYPE_EDITABLE_EMBEDDING = #const FT_FSTYPE_EDITABLE_EMBEDDING
pattern FSTYPE_NO_SUBSETTING :: CUShort
pattern FSTYPE_NO_SUBSETTING = #const FT_FSTYPE_NO_SUBSETTING
pattern FSTYPE_BITMAP_EMBEDDING_ONLY :: CUShort
pattern FSTYPE_BITMAP_EMBEDDING_ONLY = #const FT_FSTYPE_BITMAP_EMBEDDING_ONLY

newtype GlyphFormat = GlyphFormat Word32 deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern GLYPH_FORMAT_NONE :: GlyphFormat
pattern GLYPH_FORMAT_COMPOSITE :: GlyphFormat
pattern GLYPH_FORMAT_BITMAP :: GlyphFormat
pattern GLYPH_FORMAT_OUTLINE :: GlyphFormat
pattern GLYPH_FORMAT_PLOTTER :: GlyphFormat

pattern GLYPH_FORMAT_NONE = #const FT_GLYPH_FORMAT_NONE
pattern GLYPH_FORMAT_COMPOSITE = #const FT_GLYPH_FORMAT_COMPOSITE
pattern GLYPH_FORMAT_BITMAP = #const FT_GLYPH_FORMAT_BITMAP
pattern GLYPH_FORMAT_OUTLINE = #const FT_GLYPH_FORMAT_OUTLINE
pattern GLYPH_FORMAT_PLOTTER = #const FT_GLYPH_FORMAT_PLOTTER

newtype OUTLINE_FLAGS = OUTLINE_FLAGS Word32 deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern OUTLINE_NONE :: OUTLINE_FLAGS
pattern OUTLINE_OWNER :: OUTLINE_FLAGS
pattern OUTLINE_EVEN_ODD_FILL :: OUTLINE_FLAGS
pattern OUTLINE_REVERSE_FILL :: OUTLINE_FLAGS
pattern OUTLINE_IGNORE_DROPOUTS :: OUTLINE_FLAGS
pattern OUTLINE_SMART_DROPOUTS :: OUTLINE_FLAGS
pattern OUTLINE_INCLUDE_STUBS :: OUTLINE_FLAGS
pattern OUTLINE_HIGH_PRECISION :: OUTLINE_FLAGS
pattern OUTLINE_SINGLE_PASS :: OUTLINE_FLAGS

pattern OUTLINE_NONE = #const FT_OUTLINE_NONE
pattern OUTLINE_OWNER = #const FT_OUTLINE_OWNER
pattern OUTLINE_EVEN_ODD_FILL = #const FT_OUTLINE_EVEN_ODD_FILL
pattern OUTLINE_REVERSE_FILL = #const FT_OUTLINE_REVERSE_FILL
pattern OUTLINE_IGNORE_DROPOUTS = #const FT_OUTLINE_IGNORE_DROPOUTS
pattern OUTLINE_SMART_DROPOUTS = #const FT_OUTLINE_SMART_DROPOUTS
pattern OUTLINE_INCLUDE_STUBS = #const FT_OUTLINE_INCLUDE_STUBS
pattern OUTLINE_HIGH_PRECISION = #const FT_OUTLINE_HIGH_PRECISION
pattern OUTLINE_SINGLE_PASS = #const FT_OUTLINE_SINGLE_PASS

newtype Orientation = Orientation CUInt deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern ORIENTATION_TRUETYPE :: Orientation
pattern ORIENTATION_POSTSCRIPT :: Orientation
pattern ORIENTATION_FILL_RIGHT :: Orientation
pattern ORIENTATION_FILL_LEFT :: Orientation
pattern ORIENTATION_NONE :: Orientation

pattern ORIENTATION_TRUETYPE = #const FT_ORIENTATION_TRUETYPE
pattern ORIENTATION_POSTSCRIPT = #const FT_ORIENTATION_POSTSCRIPT
pattern ORIENTATION_FILL_RIGHT = #const FT_ORIENTATION_FILL_RIGHT
pattern ORIENTATION_FILL_LEFT = #const FT_ORIENTATION_FILL_LEFT
pattern ORIENTATION_NONE = #const FT_ORIENTATION_NONE

data Vector = Vector
  { x, y :: {-# unpack #-} !Pos
  } deriving (Read, Show, Eq)

instance Storable Vector where
  sizeOf _    = #size FT_Vector
  alignment _ = alignment (undefined :: Pos)
  peek ptr = Vector
    <$> (#peek FT_Vector, x) ptr
    <*> (#peek FT_Vector, y) ptr
  poke ptr Vector{..} = do
    (#poke FT_Vector, x) ptr x
    (#poke FT_Vector, y) ptr y

instance Num Vector where
  Vector a b + Vector c d = Vector (a + c) (b + d)
  Vector a b - Vector c d = Vector (a - c) (b - d)
  negate (Vector a b) = Vector (negate a) (negate b)
  Vector a b * Vector c d = Vector (a * c) (b * d)
  abs (Vector a b) = Vector (abs a) (abs b)
  signum (Vector a b) = Vector (signum a) (signum b)
  fromInteger t = (Vector >>= id) (fromInteger t)

data BBox = BBox { xMin, yMin, xMax, yMax :: {-# unpack #-} !Pos } deriving (Read, Show, Eq, Ord)

instance Storable BBox where
  sizeOf    _ = #size FT_BBox
  alignment _ = #alignment FT_BBox
  peek ptr = BBox
    <$> (#peek FT_BBox, xMin) ptr
    <*> (#peek FT_BBox, yMin) ptr
    <*> (#peek FT_BBox, xMax) ptr
    <*> (#peek FT_BBox, yMax) ptr
  poke ptr BBox{..} = do
    (#poke FT_BBox, xMin) ptr xMin
    (#poke FT_BBox, yMin) ptr yMin
    (#poke FT_BBox, xMax) ptr xMax
    (#poke FT_BBox, yMax) ptr yMax

data Bitmap = Bitmap
  { rows         :: {-# unpack #-} !CInt
  , width        :: {-# unpack #-} !CInt
  , pitch        :: {-# unpack #-} !CInt
  , buffer       :: {-# unpack #-} !(Ptr CChar)
  , num_grays    :: {-# unpack #-} !CShort
  , pixel_mode   :: {-# unpack #-} !CChar
  , palette_mode :: {-# unpack #-} !CChar
  , palette      :: {-# unpack #-} !(Ptr ())
  }

instance Storable Bitmap where
  sizeOf _    = #size FT_Bitmap
  alignment _ = #alignment FT_Bitmap
  peek ptr = Bitmap
    <$> (#peek FT_Bitmap, rows) ptr
    <*> (#peek FT_Bitmap, width) ptr
    <*> (#peek FT_Bitmap, pitch) ptr
    <*> (#peek FT_Bitmap, buffer) ptr
    <*> (#peek FT_Bitmap, num_grays) ptr
    <*> (#peek FT_Bitmap, pixel_mode) ptr
    <*> (#peek FT_Bitmap, palette_mode) ptr
    <*> (#peek FT_Bitmap, palette) ptr
  poke ptr Bitmap{..} = do
    (#poke FT_Bitmap, rows) ptr rows
    (#poke FT_Bitmap, width) ptr width
    (#poke FT_Bitmap, pitch) ptr pitch
    (#poke FT_Bitmap, buffer) ptr buffer
    (#poke FT_Bitmap, num_grays) ptr num_grays
    (#poke FT_Bitmap, pixel_mode) ptr pixel_mode
    (#poke FT_Bitmap, palette_mode) ptr palette_mode
    (#poke FT_Bitmap, palette) ptr palette

newtype BitmapGlyph = BitmapGlyph Glyph

class HasRoot t where
  root :: t -> Glyph
  cast :: Glyph -> t

instance HasRoot BitmapGlyph where
  root (BitmapGlyph ptr) = ptr
  cast = BitmapGlyph

left :: BitmapGlyph -> Ptr CInt
left (BitmapGlyph ptr) = (#ptr struct FT_BitmapGlyphRec_, left) ptr

top :: BitmapGlyph -> Ptr CInt
top (BitmapGlyph ptr) = (#ptr struct FT_BitmapGlyphRec_, top) ptr

class HasBitmap t where
  bitmap :: t -> Ptr Bitmap

instance HasBitmap BitmapGlyph where
  bitmap (BitmapGlyph ptr) = (#ptr struct FT_BitmapGlyphRec_, bitmap) ptr


data BitmapSize = BitmapSize
  { bs_height :: {-# unpack #-} !CShort
  , bs_width  :: {-# unpack #-} !CShort
  , bs_size   :: {-# unpack #-} !Pos
  , bs_x_ppem :: {-# unpack #-} !Pos
  , bs_y_ppem :: {-# unpack #-} !Pos
  } deriving (Read, Show, Eq)

instance Storable BitmapSize where
  sizeOf   _  = #size FT_Bitmap_Size
  alignment _ = #alignment FT_Bitmap_Size
  peek ptr = BitmapSize
    <$> (#peek FT_Bitmap_Size, height) ptr
    <*> (#peek FT_Bitmap_Size, width) ptr
    <*> (#peek FT_Bitmap_Size, size) ptr
    <*> (#peek FT_Bitmap_Size, x_ppem) ptr
    <*> (#peek FT_Bitmap_Size, y_ppem) ptr
  poke p BitmapSize{..} = do
    (#poke FT_Bitmap_Size, height) p bs_height
    (#poke FT_Bitmap_Size, width) p bs_width
    (#poke FT_Bitmap_Size, size) p bs_size
    (#poke FT_Bitmap_Size, x_ppem) p bs_x_ppem
    (#poke FT_Bitmap_Size, y_ppem) p bs_y_ppem

data CharMapRec
type CharMap = Ptr CharMapRec

class HasFace t where
  face :: t -> Ptr Face

instance HasFace CharMap where
  face = #ptr FT_CharMapRec, face

encoding :: CharMap -> Ptr Encoding
encoding = #ptr FT_CharMapRec, encoding

platform_id :: CharMap -> Ptr CUShort
platform_id = #ptr FT_CharMapRec, platform_id

encoding_id :: CharMap -> Ptr CUShort
encoding_id = #ptr FT_CharMapRec, encoding_id

num_faces :: Ptr Face -> Ptr CLong
num_faces = #ptr struct FT_FaceRec_, num_faces

face_index :: Ptr Face -> Ptr CLong
face_index = #ptr struct FT_FaceRec_, face_index

face_flags :: Ptr Face -> Ptr CLong
face_flags = #ptr struct FT_FaceRec_, face_flags

style_flags :: Ptr Face -> Ptr CLong
style_flags = #ptr struct FT_FaceRec_, style_flags

num_glyphs :: Ptr Face -> Ptr CLong
num_glyphs = #ptr struct FT_FaceRec_, num_glyphs

family_name :: Ptr Face -> Ptr CString
family_name = #ptr struct FT_FaceRec_, family_name

style_name :: Ptr Face -> Ptr CString
style_name = #ptr struct FT_FaceRec_, style_name

num_fixed_sizes :: Ptr Face -> Ptr CInt
num_fixed_sizes = #ptr struct FT_FaceRec_, num_fixed_sizes

available_sizes :: Ptr Face -> Ptr (Ptr BitmapSize)
available_sizes = #ptr struct FT_FaceRec_, available_sizes

num_charmaps :: Ptr Face -> Ptr CInt
num_charmaps = #ptr struct FT_FaceRec_, num_charmaps

charmaps :: Ptr Face -> Ptr (Ptr CharMap)
charmaps = #ptr struct FT_FaceRec_, charmaps

class HasGeneric t where
  generic :: t -> Ptr Generic

instance HasGeneric (Ptr Face) where
  generic = #ptr struct FT_FaceRec_, generic

bbox :: Ptr Face -> Ptr BBox
bbox = #ptr struct FT_FaceRec_, bbox

units_per_EM :: Ptr Face -> Ptr CUShort
units_per_EM = #ptr struct FT_FaceRec_, units_per_EM

ascender :: Ptr Face -> Ptr CShort
ascender = #ptr struct FT_FaceRec_, ascender

descender :: Ptr Face -> Ptr CShort
descender = #ptr struct FT_FaceRec_, descender

height :: Ptr Face -> Ptr CShort
height = #ptr struct FT_FaceRec_, height

max_advance_width :: Ptr Face -> Ptr CShort
max_advance_width = #ptr struct FT_FaceRec_, max_advance_width

max_advance_height :: Ptr Face -> Ptr CShort
max_advance_height = #ptr struct FT_FaceRec_, max_advance_height

underline_position :: Ptr Face -> Ptr CShort
underline_position = #ptr struct FT_FaceRec_, underline_position

underline_thickness :: Ptr Face -> Ptr CShort
underline_thickness = #ptr struct FT_FaceRec_, underline_thickness

glyph :: Ptr Face -> Ptr GlyphSlot
glyph = #ptr struct FT_FaceRec_, glyph

class HasSize size t | t -> size where
  size :: t -> Ptr size

instance HasSize Size (Ptr Face) where
  size = #ptr struct FT_FaceRec_, size

charmap :: Ptr Face -> Ptr CharMap
charmap = #ptr struct FT_FaceRec_, charmap

has :: FaceFlag -> Ptr Face -> IO Bool
has flag fc = do
  f <- peek $ face_flags fc
  return $ 0 /= (f .&. fromIntegral flag)

_HAS_HORIZONTAL :: Ptr Face -> IO Bool
_HAS_HORIZONTAL = has FACE_FLAG_HORIZONTAL

_HAS_VERTICAL :: Ptr Face -> IO Bool
_HAS_VERTICAL = has FACE_FLAG_VERTICAL

_HAS_KERNING :: Ptr Face -> IO Bool
_HAS_KERNING = has FACE_FLAG_KERNING

_IS_SCALABLE :: Ptr Face -> IO Bool
_IS_SCALABLE = has FACE_FLAG_SCALABLE

_IS_SFNT :: Ptr Face -> IO Bool
_IS_SFNT = has FACE_FLAG_SFNT

_IS_FIXED_WIDTH :: Ptr Face -> IO Bool
_IS_FIXED_WIDTH = has FACE_FLAG_FIXED_WIDTH

_HAS_FIXED_SIZES :: Ptr Face -> IO Bool
_HAS_FIXED_SIZES = has FACE_FLAG_FIXED_SIZES

_HAS_FAST_GLYPHS :: Ptr Face -> IO Bool
_HAS_FAST_GLYPHS = has FACE_FLAG_FAST_GLYPHS

_HAS_GLYPH_NAMES :: Ptr Face -> IO Bool
_HAS_GLYPH_NAMES = has FACE_FLAG_GLYPH_NAMES

_HAS_MULTIPLE_MASTERS :: Ptr Face -> IO Bool
_HAS_MULTIPLE_MASTERS = has FACE_FLAG_MULTIPLE_MASTERS

_IS_CID_KEYED :: Ptr Face -> IO Bool
_IS_CID_KEYED = has FACE_FLAG_CID_KEYED

_IS_TRICKY :: Ptr Face -> IO Bool
_IS_TRICKY = has FACE_FLAG_TRICKY

pattern STYLE_FLAG_ITALIC :: CLong
pattern STYLE_FLAG_ITALIC = #const FT_STYLE_FLAG_ITALIC

pattern STYLE_FLAG_BOLD :: CLong
pattern STYLE_FLAG_BOLD = #const FT_STYLE_FLAG_BOLD 

type GenericFinalizer = FunPtr (Ptr () -> IO ())

data Generic = Generic
  { data_ :: Ptr ()
  , finalizer :: GenericFinalizer
  }

instance Storable Generic where
  sizeOf    _ = #size FT_Generic
  alignment _ = #alignment FT_Generic
  peek ptr = Generic
    <$> (#peek FT_Generic, data) ptr
    <*> (#peek FT_Generic, finalizer) ptr
  poke ptr Generic{..} = do
    (#poke FT_Generic, data) ptr data_
    (#poke FT_Generic, finalizer) ptr finalizer

data GlyphRec_
type Glyph = Ptr GlyphRec_

class HasLibrary t where
  library :: t -> Ptr Library

instance HasLibrary Glyph where
  library = #ptr struct FT_GlyphRec_, library

class HasFormat t where
  format :: t -> Ptr GlyphFormat

instance HasFormat Glyph where
  format = #ptr struct FT_GlyphRec_, format

class HasAdvance t where
  advance :: t -> Ptr Vector

instance HasAdvance Glyph where
  advance = #ptr struct FT_GlyphRec_, advance

data GlyphMetrics = GlyphMetrics
  { gm_width, gm_height, gm_horiBearingX, gm_horiBearingY, gm_horiAdvance
  , gm_vertBearingX, gm_vertBearingY, gm_vertAdvance :: {-# unpack #-} !Pos
  } deriving (Read, Show, Eq)

instance Storable GlyphMetrics where
  sizeOf    _ = #size FT_Glyph_Metrics
  alignment _ = #alignment FT_Glyph_Metrics
  peek ptr = GlyphMetrics
    <$> (#peek FT_Glyph_Metrics, width) ptr
    <*> (#peek FT_Glyph_Metrics, height) ptr
    <*> (#peek FT_Glyph_Metrics, horiBearingX) ptr
    <*> (#peek FT_Glyph_Metrics, horiBearingY) ptr
    <*> (#peek FT_Glyph_Metrics, horiAdvance) ptr
    <*> (#peek FT_Glyph_Metrics, vertBearingX) ptr
    <*> (#peek FT_Glyph_Metrics, vertBearingY) ptr
    <*> (#peek FT_Glyph_Metrics, vertAdvance) ptr
  poke ptr GlyphMetrics{..} = do
    (#poke FT_Glyph_Metrics, width) ptr gm_width
    (#poke FT_Glyph_Metrics, height) ptr gm_height
    (#poke FT_Glyph_Metrics, horiBearingX) ptr gm_horiBearingX
    (#poke FT_Glyph_Metrics, horiBearingY) ptr gm_horiBearingY
    (#poke FT_Glyph_Metrics, horiAdvance) ptr gm_horiAdvance
    (#poke FT_Glyph_Metrics, vertBearingX) ptr gm_vertBearingX
    (#poke FT_Glyph_Metrics, vertBearingY) ptr gm_vertBearingY
    (#poke FT_Glyph_Metrics, vertAdvance) ptr gm_vertAdvance

data GlyphSlotRec_
type GlyphSlot = Ptr GlyphSlotRec_

instance HasLibrary GlyphSlot where
  library = #ptr struct FT_GlyphSlotRec_, library

instance HasFace GlyphSlot where
  face = #ptr struct FT_GlyphSlotRec_, face

next :: GlyphSlot -> Ptr GlyphSlot
next = #ptr struct FT_GlyphSlotRec_, next

instance HasGeneric GlyphSlot where
  generic = #ptr struct FT_GlyphSlotRec_, generic

class HasMetrics metrics t | t -> metrics where
  metrics :: t -> Ptr metrics

instance HasMetrics GlyphMetrics GlyphSlot where
  metrics = #ptr struct FT_GlyphSlotRec_, metrics

linearHoriAdvance :: GlyphSlot -> Ptr Fixed
linearHoriAdvance = #ptr struct FT_GlyphSlotRec_, linearHoriAdvance

linearVertAdvance :: GlyphSlot -> Ptr Fixed
linearVertAdvance = #ptr struct FT_GlyphSlotRec_, linearVertAdvance

instance HasAdvance GlyphSlot where
  advance = #ptr struct FT_GlyphSlotRec_, advance

instance HasFormat GlyphSlot where
  format = #ptr struct FT_GlyphSlotRec_, format

instance HasBitmap GlyphSlot where
  bitmap = #ptr struct FT_GlyphSlotRec_, bitmap

bitmap_left :: GlyphSlot -> Ptr CInt
bitmap_left = #ptr struct FT_GlyphSlotRec_, bitmap_left

bitmap_top :: GlyphSlot -> Ptr CInt
bitmap_top = #ptr struct FT_GlyphSlotRec_, bitmap_top

class HasOutline t where
  outline :: t -> Ptr Outline

instance HasOutline GlyphSlot where
  outline = #ptr struct FT_GlyphSlotRec_, outline

num_subglyphs :: GlyphSlot -> Ptr CUInt
num_subglyphs = #ptr struct FT_GlyphSlotRec_, num_subglyphs

subglyphs :: GlyphSlot -> Ptr SubGlyph
subglyphs = #ptr struct FT_GlyphSlotRec_, subglyphs

control_data :: GlyphSlot -> Ptr a
control_data = #ptr struct FT_GlyphSlotRec_, control_data

control_len :: GlyphSlot -> Ptr CLong
control_len = #ptr struct FT_GlyphSlotRec_, control_len

lsb_delta :: GlyphSlot -> Ptr Pos
lsb_delta = #ptr struct FT_GlyphSlotRec_, lsb_delta

rsb_delta :: GlyphSlot -> Ptr Pos
rsb_delta = #ptr struct FT_GlyphSlotRec_, rsb_delta

data Matrix = Matrix
  { xx, xy
  , yx, yy :: {-# unpack #-} !Fixed
  } deriving (Read, Show, Eq)

instance Storable Matrix where
  sizeOf _    = #size FT_Matrix
  alignment _ = alignment (undefined :: Fixed)
  peek ptr = Matrix
    <$> (#peek FT_Matrix, xx) ptr
    <*> (#peek FT_Matrix, xy) ptr
    <*> (#peek FT_Matrix, yx) ptr
    <*> (#peek FT_Matrix, yy) ptr
  poke ptr Matrix{..} = do
    (#poke FT_Matrix, xx) ptr xx
    (#poke FT_Matrix, xy) ptr xy
    (#poke FT_Matrix, yx) ptr yx
    (#poke FT_Matrix, yy) ptr yy

type AllocFunc   = FunPtr (Memory -> CLong -> IO (Ptr ()))
type FreeFunc    = FunPtr (Memory -> Ptr () -> IO ())
type ReallocFunc = FunPtr (Memory -> CLong -> CLong -> Ptr () -> IO (Ptr ()))
                     
type Memory = Ptr MemoryRec
data MemoryRec = MemoryRec
  { user    :: Ptr ()
  , alloc   :: AllocFunc
  , free    :: FreeFunc
  , realloc :: ReallocFunc
  }

instance Storable MemoryRec where
  sizeOf    _ = #size struct FT_MemoryRec_
  alignment _ = #alignment struct FT_MemoryRec_
  peek ptr = MemoryRec
    <$> (#peek struct FT_MemoryRec_, user) ptr
    <*> (#peek struct FT_MemoryRec_, alloc) ptr
    <*> (#peek struct FT_MemoryRec_, free) ptr
    <*> (#peek struct FT_MemoryRec_, realloc) ptr
  poke ptr MemoryRec{..} = do
    (#poke struct FT_MemoryRec_, user) ptr user
    (#poke struct FT_MemoryRec_, alloc) ptr alloc
    (#poke struct FT_MemoryRec_, free) ptr free
    (#poke struct FT_MemoryRec_, realloc) ptr realloc

data OpenArgs = OpenArgs
  { flags       :: {-# unpack #-} !CUInt
  , memory_base :: {-# unpack #-} !Bytes
  , memory_size :: {-# unpack #-} !CLong
  , pathname    :: {-# unpack #-} !CString
  , stream      :: {-# unpack #-} !Stream
  , driver      :: {-# unpack #-} !Module
  , num_params  :: {-# unpack #-} !Int
  , params      :: {-# unpack #-} !(Ptr Parameter)
  }

instance Storable OpenArgs where
  sizeOf    _ = #size FT_Open_Args
  alignment _ = #alignment FT_Open_Args
  peek ptr = OpenArgs
    <$> (#peek FT_Open_Args, flags) ptr
    <*> (#peek FT_Open_Args, memory_base) ptr
    <*> (#peek FT_Open_Args, memory_size) ptr
    <*> (#peek FT_Open_Args, pathname) ptr
    <*> (#peek FT_Open_Args, stream) ptr
    <*> (#peek FT_Open_Args, driver) ptr
    <*> (#peek FT_Open_Args, num_params) ptr
    <*> (#peek FT_Open_Args, params) ptr
  poke ptr OpenArgs{..} = do
    (#poke FT_Open_Args, flags) ptr flags
    (#poke FT_Open_Args, memory_base) ptr memory_base
    (#poke FT_Open_Args, memory_size) ptr memory_size
    (#poke FT_Open_Args, pathname) ptr pathname
    (#poke FT_Open_Args, stream) ptr stream
    (#poke FT_Open_Args, driver) ptr driver
    (#poke FT_Open_Args, num_params) ptr num_params
    (#poke FT_Open_Args, params) ptr params

data Outline = Outline
  { n_contours :: {-# unpack #-} !CShort
  , n_points   :: {-# unpack #-} !CShort
  , points     :: {-# unpack #-} !(Ptr Vector)
  , tags       :: {-# unpack #-} !CString
  , contours   :: {-# unpack #-} !(Ptr CShort)
  , flags      :: {-# unpack #-} !OUTLINE_FLAGS
  }

instance Storable Outline where
  sizeOf    _ = #size FT_Outline
  alignment _ = #alignment FT_Outline
  peek ptr = Outline
    <$> (#peek FT_Outline, n_contours) ptr
    <*> (#peek FT_Outline, n_points) ptr
    <*> (#peek FT_Outline, points) ptr
    <*> (#peek FT_Outline, tags) ptr
    <*> (#peek FT_Outline, contours) ptr
    <*> (#peek FT_Outline, flags) ptr
  poke ptr Outline{..} = do
    (#poke FT_Outline, n_contours) ptr n_contours
    (#poke FT_Outline, n_points)   ptr n_points
    (#poke FT_Outline, points)     ptr points
    (#poke FT_Outline, tags)       ptr tags
    (#poke FT_Outline, contours)   ptr contours
    (#poke FT_Outline, flags)      ptr flags

type OutlineMoveToFunc = FunPtr (Ptr Vector -> Ptr () -> IO CInt)
type OutlineLineToFunc = FunPtr (Ptr Vector -> Ptr () -> IO CInt)
type OutlineConicToFunc = FunPtr (Ptr Vector -> Ptr Vector -> Ptr () -> IO CInt)
type OutlineCubicToFunc = FunPtr (Ptr Vector -> Ptr Vector -> Ptr Vector -> Ptr () -> IO CInt)

data OutlineFuncs = OutlineFuncs
  { move_to  :: {-# unpack #-} !OutlineMoveToFunc
  , line_to  :: {-# unpack #-} !OutlineLineToFunc
  , conic_to :: {-# unpack #-} !OutlineConicToFunc
  , cubic_to :: {-# unpack #-} !OutlineCubicToFunc
  , shift    :: {-# unpack #-} !CInt
  , delta    :: {-# unpack #-} !Pos
  }

instance Storable OutlineFuncs where
  sizeOf    _ = #size FT_Outline_Funcs
  alignment _ = #alignment FT_Outline_Funcs
  peek ptr = OutlineFuncs
    <$> (#peek FT_Outline_Funcs, move_to) ptr
    <*> (#peek FT_Outline_Funcs, line_to) ptr
    <*> (#peek FT_Outline_Funcs, conic_to) ptr
    <*> (#peek FT_Outline_Funcs, cubic_to) ptr
    <*> (#peek FT_Outline_Funcs, shift) ptr
    <*> (#peek FT_Outline_Funcs, delta) ptr
  poke ptr OutlineFuncs{..} = do
    (#poke FT_Outline_Funcs, move_to) ptr move_to
    (#poke FT_Outline_Funcs, line_to) ptr line_to
    (#poke FT_Outline_Funcs, conic_to) ptr conic_to
    (#poke FT_Outline_Funcs, cubic_to) ptr cubic_to
    (#poke FT_Outline_Funcs, shift) ptr shift
    (#poke FT_Outline_Funcs, delta) ptr delta

newtype OutlineGlyph = OutlineGlyph Glyph

instance HasRoot OutlineGlyph where
  root (OutlineGlyph ptr) = ptr
  cast = OutlineGlyph

instance HasOutline OutlineGlyph where
  outline (OutlineGlyph ptr) = (#ptr struct FT_OutlineGlyphRec_, outline) ptr

data Parameter = Parameter
  { tag   :: {-# unpack #-} !CULong
  , data_ :: {-# unpack #-} !(Ptr ())
  }

instance Storable Parameter where
  sizeOf    _ = #size FT_Parameter
  alignment _ = #alignment FT_Parameter
  peek ptr = Parameter
    <$> (#peek FT_Parameter, tag) ptr
    <*> (#peek FT_Parameter, data) ptr
  poke ptr Parameter{..} = do
    (#poke FT_Parameter, tag) ptr tag
    (#poke FT_Parameter, data) ptr data_

data RasterParams = RasterParams
  { target      :: {-# unpack #-} !(Ptr Bitmap)
  , source      :: {-# unpack #-} !(Ptr ())
  , flags       :: {-# unpack #-} !CInt
  , gray_spans  :: {-# unpack #-} !SpanFunc
  , black_spans :: {-# unpack #-} !SpanFunc            -- doesn't work according to FT docs
  , bit_test    :: {-# unpack #-} !RasterBitTestFunc -- doesn't work according to FT docs
  , bit_set     :: {-# unpack #-} !RasterBitSetFunc  -- doesn't work according to FT docs
  , user        :: {-# unpack #-} !(Ptr ())
  , clip_box    :: !BBox
  }

instance Storable RasterParams where
  sizeOf    _ = #size FT_Raster_Params
  alignment _ = #alignment FT_Raster_Params
  peek ptr = RasterParams
    <$> (#peek FT_Raster_Params, target) ptr
    <*> (#peek FT_Raster_Params, source) ptr
    <*> (#peek FT_Raster_Params, flags) ptr
    <*> (#peek FT_Raster_Params, gray_spans) ptr
    <*> (#peek FT_Raster_Params, black_spans) ptr
    <*> (#peek FT_Raster_Params, bit_test) ptr
    <*> (#peek FT_Raster_Params, bit_set) ptr
    <*> (#peek FT_Raster_Params, user) ptr
    <*> (#peek FT_Raster_Params, clip_box) ptr
  poke ptr RasterParams{..} = do
    (#poke FT_Raster_Params, target) ptr target
    (#poke FT_Raster_Params, source) ptr source
    (#poke FT_Raster_Params, flags) ptr flags
    (#poke FT_Raster_Params, gray_spans) ptr gray_spans
    (#poke FT_Raster_Params, black_spans) ptr black_spans
    (#poke FT_Raster_Params, bit_test) ptr bit_test
    (#poke FT_Raster_Params, bit_set) ptr bit_set
    (#poke FT_Raster_Params, user) ptr user
    (#poke FT_Raster_Params, clip_box) ptr clip_box

type SpanFunc = FunPtr (CInt -> CInt -> Ptr Span -> Ptr () -> IO ())
type RasterBitTestFunc = FunPtr (CInt -> CInt -> Ptr () -> IO CInt)
type RasterBitSetFunc = FunPtr (CInt -> CInt -> Ptr () -> IO ())

data SizeRec_
type Size = Ptr SizeRec_

instance HasFace Size where
  face = #ptr struct FT_SizeRec_, face

instance HasGeneric Size where
  generic = #ptr struct FT_SizeRec_, generic

instance HasMetrics SizeMetrics Size where
  metrics = #ptr struct FT_SizeRec_, metrics

data SizeMetrics = SizeMetrics
  { sm_x_ppem      :: {-# unpack #-} !CUShort
  , sm_y_ppem      :: {-# unpack #-} !CUShort
  , sm_x_scale     :: {-# unpack #-} !Fixed
  , sm_y_scale     :: {-# unpack #-} !Fixed
  , sm_ascender    :: {-# unpack #-} !Pos
  , sm_descender   :: {-# unpack #-} !Pos
  , sm_height      :: {-# unpack #-} !Pos
  , sm_max_advance :: {-# unpack #-} !Pos
  } deriving (Read, Show, Eq)

instance Storable SizeMetrics where
  sizeOf    _ = #size FT_Size_Metrics
  alignment _ = #alignment FT_Size_Metrics
  peek ptr = SizeMetrics
    <$> (#peek FT_Size_Metrics, x_ppem) ptr
    <*> (#peek FT_Size_Metrics, y_ppem) ptr
    <*> (#peek FT_Size_Metrics, x_scale) ptr
    <*> (#peek FT_Size_Metrics, y_scale) ptr
    <*> (#peek FT_Size_Metrics, ascender) ptr
    <*> (#peek FT_Size_Metrics, descender) ptr
    <*> (#peek FT_Size_Metrics, height) ptr
    <*> (#peek FT_Size_Metrics, max_advance) ptr
  poke ptr SizeMetrics{..} = do
    (#poke FT_Size_Metrics, x_ppem) ptr sm_x_ppem
    (#poke FT_Size_Metrics, y_ppem) ptr sm_y_ppem
    (#poke FT_Size_Metrics, x_scale) ptr sm_x_scale
    (#poke FT_Size_Metrics, y_scale) ptr sm_y_scale
    (#poke FT_Size_Metrics, ascender) ptr sm_ascender
    (#poke FT_Size_Metrics, descender) ptr sm_descender
    (#poke FT_Size_Metrics, height) ptr sm_height
    (#poke FT_Size_Metrics, max_advance) ptr sm_max_advance

type SizeRequest = Ptr SizeRequestRec

data SizeRequestRec = SizeRequestRec
  { srr_type  :: {-# unpack #-} !SizeRequestType
  , srr_width  :: {-# unpack #-} !CLong
  , srr_height :: {-# unpack #-} !CLong
  , srr_horiResolution :: {-# unpack #-} !CUInt
  , srr_vertResolution :: {-# unpack #-} !CUInt
  }

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
    (#poke FT_Size_RequestRec, type) ptr srr_type
    (#poke FT_Size_RequestRec, height) ptr srr_height
    (#poke FT_Size_RequestRec, horiResolution) ptr srr_horiResolution
    (#poke FT_Size_RequestRec, vertResolution) ptr srr_vertResolution

data Span = Span
  { x        :: {-# unpack #-} !CShort
  , len      :: {-# unpack #-} !CUShort
  , coverage :: {-# unpack #-} !CUChar
  } deriving (Read, Show, Eq)

instance Storable Span where
  sizeOf    _ = #size FT_Span
  alignment _ = #alignment FT_Span
  peek ptr = Span
    <$> (#peek FT_Span, x) ptr
    <*> (#peek FT_Span, len) ptr
    <*> (#peek FT_Span, coverage) ptr
  poke ptr Span{..} = do
    (#poke FT_Span, x) ptr x
    (#poke FT_Span, len) ptr len
    (#poke FT_Span, coverage) ptr coverage

-- We don't have FILE* in Haskell, so we cheat and only
-- consider the long part of the FT_StreamDesc union...
type StreamDesc = CLong

type StreamIoFunc = FunPtr (Stream -> CULong -> Ptr CUChar -> CULong -> IO CULong)
type StreamCloseFunc = FunPtr (Stream -> IO ())

data StreamRec_
type Stream = Ptr StreamRec_

base :: Stream -> Ptr (Ptr CUChar)
base = #ptr struct FT_StreamRec_, base

instance HasSize CULong Stream where
  size = #ptr struct FT_StreamRec_, size

pos :: Stream -> Ptr CULong
pos = #ptr struct FT_StreamRec_, pos 

descriptor :: Stream -> Ptr StreamDesc
descriptor = #ptr struct FT_StreamRec_, descriptor

read :: Stream -> Ptr StreamIoFunc
read = #ptr struct FT_StreamRec_, read

close :: Stream -> Ptr StreamCloseFunc
close = #ptr struct FT_StreamRec_, close

memory :: Stream -> Ptr Memory
memory = #ptr struct FT_StreamRec_, memory

cursor :: Stream -> Ptr (Ptr CUChar)
cursor = #ptr struct FT_StreamRec_, cursor

limit :: Stream -> Ptr (Ptr CUChar)
limit = #ptr struct FT_StreamRec_, limit

