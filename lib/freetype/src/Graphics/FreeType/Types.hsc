{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.FreeType.Types where

import Foreign
import Foreign.C.Types

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_IMAGE_H
#include FT_OUTLINE_H

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

type Fixed = CLong
type Pos = CLong
type F26Dot6 = CLong
type Offset = #type size_t
type Error = CInt
type Byte = CUChar
type Bytes = Ptr Byte

type FT_Char = CChar
type FT_Bool = CUChar

newtype RenderMode = RenderMode CUInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern RENDER_MODE_NORMAL :: RenderMode
pattern RENDER_MODE_LIGHT  :: RenderMode
pattern RENDER_MODE_MONO   :: RenderMode
pattern RENDER_MODE_LCD    :: RenderMode
pattern RENDER_MODE_LCD_V  :: RenderMode

pattern RENDER_MODE_NORMAL = #const FT_RENDER_MODE_NORMAL
pattern RENDER_MODE_LIGHT  = #const FT_RENDER_MODE_LIGHT
pattern RENDER_MODE_MONO   = #const FT_RENDER_MODE_MONO
pattern RENDER_MODE_LCD    = #const FT_RENDER_MODE_LCD
pattern RENDER_MODE_LCD_V  = #const FT_RENDER_MODE_LCD_V

pattern FREETYPE_MAJOR :: CInt
pattern FREETYPE_MAJOR = #const FREETYPE_MAJOR

pattern FREETYPE_MINOR :: CInt
pattern FREETYPE_MINOR = #const FREETYPE_MINOR

pattern FREETYPE_PATCH :: CInt
pattern FREETYPE_PATCH = #const FREETYPE_PATCH

newtype Encoding = Encoding Word32
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype FaceFlag = FaceFlag CInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype Open = Open CUInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype SizeRequestType = SizeRequestType CUInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype KerningMode = KerningMode Word32
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

pattern KERNING_DEFAULT :: KerningMode
pattern KERNING_DEFAULT  = #const FT_KERNING_DEFAULT
pattern KERNING_UNFITTED :: KerningMode
pattern KERNING_UNFITTED = #const FT_KERNING_UNFITTED
pattern KERNING_UNSCALED :: KerningMode
pattern KERNING_UNSCALED = #const FT_KERNING_UNSCALED

newtype SubglyphFlag = SubglyphFlag CUInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype GlyphFormat = GlyphFormat Word32
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype OUTLINE_FLAGS = OUTLINE_FLAGS Word32
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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

newtype Orientation = Orientation CUInt
  deriving (Eq, Show, Real, Ord, Enum, Num, Integral, Storable)

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
