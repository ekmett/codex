{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language OverloadedStrings #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-} -- cuts half the length of this file

-- | ffi to the harfbuzz library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Harfbuzz.Internal
( Blob(..)
, Buffer(..)
, BufferClusterLevel
  ( BufferClusterLevel
  , BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES
  , BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS
  , BUFFER_CLUSTER_LEVEL_CHARACTERS
  , BUFFER_CLUSTER_LEVEL_DEFAULT
  )
, BufferContentType
  ( BufferContentType
  , BUFFER_CONTENT_TYPE_INVALID
  , BUFFER_CONTENT_TYPE_UNICODE
  , BUFFER_CONTENT_TYPE_GLYPHS
  )
, BufferDiffFlags
  ( BufferDiffFlags
  , BUFFER_DIFF_FLAG_EQUAL
  , BUFFER_DIFF_FLAG_CONTENT_TYPE_MISMATCH
  , BUFFER_DIFF_FLAG_LENGTH_MISMATCH
  , BUFFER_DIFF_FLAG_NOTDEF_PRESENT
  , BUFFER_DIFF_FLAG_DOTTED_CIRCLE_PRESENT
  , BUFFER_DIFF_FLAG_CODEPOINT_MISMATCH
  , BUFFER_DIFF_FLAG_CLUSTER_MISMATCH
  , BUFFER_DIFF_FLAG_GLYPH_FLAGS_MISMATCH
  , BUFFER_DIFF_FLAG_POSITION_MISMATCH
  )
, BufferFlags
  ( BufferFlags
  , BUFFER_FLAG_DEFAULT
  , BUFFER_FLAG_BOT
  , BUFFER_FLAG_EOT
  , BUFFER_FLAG_PRESERVE_DEFAULT_IGNORABLES
  , BUFFER_FLAG_REMOVE_DEFAULT_IGNORABLES
  , BUFFER_FLAG_DO_NOT_INSERT_DOTTED_CIRCLE
  )
, BufferMessageFunc
, pattern BUFFER_REPLACEMENT_CODEPOINT_DEFAULT
, BufferSerializeFlags
  ( BufferSerializeFlags
  , BUFFER_SERIALIZE_FLAG_DEFAULT
  , BUFFER_SERIALIZE_FLAG_NO_CLUSTERS
  , BUFFER_SERIALIZE_FLAG_NO_POSITIONS
  , BUFFER_SERIALIZE_FLAG_NO_GLYPH_NAMES
  , BUFFER_SERIALIZE_FLAG_GLYPH_EXTENTS
  , BUFFER_SERIALIZE_FLAG_GLYPH_FLAGS
  , BUFFER_SERIALIZE_FLAG_NO_ADVANCES
  )
, BufferSerializeFormat
  ( BufferSerializeFormat
  , BUFFER_SERIALIZE_FORMAT_TEXT
  , BUFFER_SERIALIZE_FORMAT_JSON
  , BUFFER_SERIALIZE_FORMAT_INVALID
  )
, buffer_serialize_format_to_string, buffer_serialize_format_from_string
, buffer_serialize_list_formats
, Codepoint
, Direction
  ( Direction
  , DIRECTION_INVALID, DIRECTION_LTR, DIRECTION_RTL, DIRECTION_BTT, DIRECTION_TTB
  )
, direction_to_string, direction_from_string
, Face(..)
, Feature(..)
, feature_to_string, feature_from_string
, Font(..)
, FontExtents(..)
, FontFuncs(..)
, GlyphExtents(..)
, GlyphFlags
  ( GlyphFlags
  , GLYPH_FLAG_UNSAFE_TO_BREAK
  , GLYPH_FLAG_DEFINED
  )
, GlyphInfo(..)
, GlyphPosition(..)
, Language
  ( Language
  , LANGUAGE_INVALID
  )
, language_to_string, language_from_string
, Map(..)
, pattern MAP_VALUE_INVALID
, MemoryMode
  ( MemoryMode
  , MEMORY_MODE_DUPLICATE, MEMORY_MODE_READONLY
  , MEMORY_MODE_WRITABLE, MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE
  )
, Position
, ReferenceTableFunc
, Script
  ( Script
  , SCRIPT_COMMON, SCRIPT_INHERITED, SCRIPT_UNKNOWN, SCRIPT_ARABIC, SCRIPT_ARMENIAN
  , SCRIPT_BENGALI, SCRIPT_CYRILLIC, SCRIPT_DEVANAGARI, SCRIPT_GEORGIAN, SCRIPT_GREEK
  , SCRIPT_GUJARATI, SCRIPT_GURMUKHI, SCRIPT_HANGUL, SCRIPT_HAN, SCRIPT_HEBREW
  , SCRIPT_HIRAGANA, SCRIPT_KANNADA, SCRIPT_KATAKANA, SCRIPT_LAO, SCRIPT_LATIN
  , SCRIPT_MALAYALAM, SCRIPT_ORIYA, SCRIPT_TAMIL, SCRIPT_TELUGU, SCRIPT_THAI
  , SCRIPT_TIBETAN, SCRIPT_BOPOMOFO, SCRIPT_BRAILLE, SCRIPT_CANADIAN_SYLLABICS
  , SCRIPT_CHEROKEE, SCRIPT_ETHIOPIC, SCRIPT_KHMER, SCRIPT_MONGOLIAN, SCRIPT_MYANMAR
  , SCRIPT_OGHAM, SCRIPT_RUNIC, SCRIPT_SINHALA, SCRIPT_SYRIAC, SCRIPT_THAANA
  , SCRIPT_YI, SCRIPT_DESERET, SCRIPT_GOTHIC, SCRIPT_OLD_ITALIC, SCRIPT_BUHID
  , SCRIPT_HANUNOO, SCRIPT_TAGALOG, SCRIPT_TAGBANWA, SCRIPT_CYPRIOT, SCRIPT_LIMBU
  , SCRIPT_LINEAR_B, SCRIPT_OSMANYA, SCRIPT_SHAVIAN, SCRIPT_TAI_LE, SCRIPT_UGARITIC
  , SCRIPT_BUGINESE, SCRIPT_COPTIC, SCRIPT_GLAGOLITIC, SCRIPT_KHAROSHTHI, SCRIPT_NEW_TAI_LUE
  , SCRIPT_OLD_PERSIAN, SCRIPT_SYLOTI_NAGRI, SCRIPT_TIFINAGH, SCRIPT_BALINESE, SCRIPT_CUNEIFORM
  , SCRIPT_NKO, SCRIPT_PHAGS_PA, SCRIPT_PHOENICIAN, SCRIPT_CARIAN, SCRIPT_CHAM
  , SCRIPT_KAYAH_LI, SCRIPT_LEPCHA, SCRIPT_LYCIAN, SCRIPT_LYDIAN, SCRIPT_OL_CHIKI
  , SCRIPT_REJANG, SCRIPT_SAURASHTRA, SCRIPT_SUNDANESE, SCRIPT_VAI, SCRIPT_AVESTAN
  , SCRIPT_BAMUM, SCRIPT_EGYPTIAN_HIEROGLYPHS, SCRIPT_IMPERIAL_ARAMAIC
  , SCRIPT_INSCRIPTIONAL_PAHLAVI, SCRIPT_INSCRIPTIONAL_PARTHIAN, SCRIPT_JAVANESE
  , SCRIPT_KAITHI, SCRIPT_LISU, SCRIPT_MEETEI_MAYEK, SCRIPT_OLD_SOUTH_ARABIAN
  , SCRIPT_OLD_TURKIC, SCRIPT_SAMARITAN, SCRIPT_TAI_THAM, SCRIPT_TAI_VIET, SCRIPT_BATAK
  , SCRIPT_BRAHMI, SCRIPT_MANDAIC, SCRIPT_CHAKMA, SCRIPT_MEROITIC_CURSIVE
  , SCRIPT_MEROITIC_HIEROGLYPHS, SCRIPT_MIAO, SCRIPT_SHARADA, SCRIPT_SORA_SOMPENG
  , SCRIPT_TAKRI, SCRIPT_BASSA_VAH, SCRIPT_CAUCASIAN_ALBANIAN, SCRIPT_DUPLOYAN
  , SCRIPT_ELBASAN, SCRIPT_GRANTHA, SCRIPT_KHOJKI, SCRIPT_KHUDAWADI, SCRIPT_LINEAR_A
  , SCRIPT_MAHAJANI, SCRIPT_MANICHAEAN, SCRIPT_MENDE_KIKAKUI, SCRIPT_MODI, SCRIPT_MRO
  , SCRIPT_NABATAEAN, SCRIPT_OLD_NORTH_ARABIAN, SCRIPT_OLD_PERMIC, SCRIPT_PAHAWH_HMONG
  , SCRIPT_PALMYRENE, SCRIPT_PAU_CIN_HAU, SCRIPT_PSALTER_PAHLAVI, SCRIPT_SIDDHAM
  , SCRIPT_TIRHUTA, SCRIPT_WARANG_CITI, SCRIPT_AHOM, SCRIPT_ANATOLIAN_HIEROGLYPHS
  , SCRIPT_HATRAN, SCRIPT_MULTANI, SCRIPT_OLD_HUNGARIAN, SCRIPT_SIGNWRITING, SCRIPT_ADLAM
  , SCRIPT_BHAIKSUKI, SCRIPT_MARCHEN, SCRIPT_OSAGE, SCRIPT_TANGUT, SCRIPT_NEWA
  , SCRIPT_MASARAM_GONDI, SCRIPT_NUSHU, SCRIPT_SOYOMBO, SCRIPT_ZANABAZAR_SQUARE
  , SCRIPT_DOGRA, SCRIPT_GUNJALA_GONDI, SCRIPT_HANIFI_ROHINGYA, SCRIPT_MAKASAR
  , SCRIPT_MEDEFAIDRIN, SCRIPT_OLD_SOGDIAN, SCRIPT_SOGDIAN, SCRIPT_ELYMAIC, SCRIPT_NANDINAGARI
  , SCRIPT_NYIAKENG_PUACHUE_HMONG, SCRIPT_WANCHO, SCRIPT_INVALID
  , SCRIPT__MAX_VALUE
  , SCRIPT__MAX_VALUE_SIGNED
  )
, SegmentProperties
  ( SegmentProperties
  , segment_properties_direction
  , segment_properties_script
  , segment_properties_language
  , SEGMENT_PROPERTIES_DEFAULT
  )
, Set(..)
, pattern SET_VALUE_INVALID
, ShapePlan(..)
, Shaper
  ( Shaper
  , SHAPER_INVALID
  )
, shape_list_shapers
, shaper_from_string, shaper_to_string
, Tag
  ( Tag, TAG
  , TAG_NONE, TAG_MAX, TAG_MAX_SIGNED
  )
, UnicodeCombiningClass
  ( UnicodeCombiningClass
  , UNICODE_COMBINING_CLASS_NOT_REORDERED
  , UNICODE_COMBINING_CLASS_OVERLAY
  , UNICODE_COMBINING_CLASS_NUKTA
  , UNICODE_COMBINING_CLASS_KANA_VOICING
  , UNICODE_COMBINING_CLASS_VIRAMA
  , UNICODE_COMBINING_CLASS_CCC10
  , UNICODE_COMBINING_CLASS_CCC11
  , UNICODE_COMBINING_CLASS_CCC12
  , UNICODE_COMBINING_CLASS_CCC13
  , UNICODE_COMBINING_CLASS_CCC14
  , UNICODE_COMBINING_CLASS_CCC15
  , UNICODE_COMBINING_CLASS_CCC16
  , UNICODE_COMBINING_CLASS_CCC17
  , UNICODE_COMBINING_CLASS_CCC18
  , UNICODE_COMBINING_CLASS_CCC19
  , UNICODE_COMBINING_CLASS_CCC20
  , UNICODE_COMBINING_CLASS_CCC21
  , UNICODE_COMBINING_CLASS_CCC22
  , UNICODE_COMBINING_CLASS_CCC23
  , UNICODE_COMBINING_CLASS_CCC24
  , UNICODE_COMBINING_CLASS_CCC25
  , UNICODE_COMBINING_CLASS_CCC26
  , UNICODE_COMBINING_CLASS_CCC27
  , UNICODE_COMBINING_CLASS_CCC28
  , UNICODE_COMBINING_CLASS_CCC29
  , UNICODE_COMBINING_CLASS_CCC30
  , UNICODE_COMBINING_CLASS_CCC31
  , UNICODE_COMBINING_CLASS_CCC32
  , UNICODE_COMBINING_CLASS_CCC33
  , UNICODE_COMBINING_CLASS_CCC34
  , UNICODE_COMBINING_CLASS_CCC35
  , UNICODE_COMBINING_CLASS_CCC36
  , UNICODE_COMBINING_CLASS_CCC84
  , UNICODE_COMBINING_CLASS_CCC91
  , UNICODE_COMBINING_CLASS_CCC103
  , UNICODE_COMBINING_CLASS_CCC107
  , UNICODE_COMBINING_CLASS_CCC118
  , UNICODE_COMBINING_CLASS_CCC122
  , UNICODE_COMBINING_CLASS_CCC129
  , UNICODE_COMBINING_CLASS_CCC130
  , UNICODE_COMBINING_CLASS_CCC133
  , UNICODE_COMBINING_CLASS_ATTACHED_BELOW_LEFT
  , UNICODE_COMBINING_CLASS_ATTACHED_BELOW
  , UNICODE_COMBINING_CLASS_ATTACHED_ABOVE
  , UNICODE_COMBINING_CLASS_ATTACHED_ABOVE_RIGHT
  , UNICODE_COMBINING_CLASS_BELOW_LEFT
  , UNICODE_COMBINING_CLASS_BELOW
  , UNICODE_COMBINING_CLASS_BELOW_RIGHT
  , UNICODE_COMBINING_CLASS_LEFT
  , UNICODE_COMBINING_CLASS_RIGHT
  , UNICODE_COMBINING_CLASS_ABOVE_LEFT
  , UNICODE_COMBINING_CLASS_ABOVE
  , UNICODE_COMBINING_CLASS_ABOVE_RIGHT
  , UNICODE_COMBINING_CLASS_DOUBLE_BELOW
  , UNICODE_COMBINING_CLASS_DOUBLE_ABOVE
  , UNICODE_COMBINING_CLASS_IOTA_SUBSCRIPT
  , UNICODE_COMBINING_CLASS_INVALID
  )
, UnicodeCombiningClassFunc
, UnicodeComposeFunc
, UnicodeDecomposeFunc
, UnicodeFuncs(..)
, UnicodeGeneralCategory
  ( UnicodeGeneralCategory
  , UNICODE_GENERAL_CATEGORY
  , UNICODE_GENERAL_CATEGORY_CONTROL
  , UNICODE_GENERAL_CATEGORY_FORMAT
  , UNICODE_GENERAL_CATEGORY_UNASSIGNED
  , UNICODE_GENERAL_CATEGORY_PRIVATE_USE
  , UNICODE_GENERAL_CATEGORY_SURROGATE
  , UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER
  , UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER
  , UNICODE_GENERAL_CATEGORY_OTHER_LETTER
  , UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER
  , UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER
  , UNICODE_GENERAL_CATEGORY_SPACING_MARK
  , UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK
  , UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK
  , UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER
  , UNICODE_GENERAL_CATEGORY_LETTER_NUMBER
  , UNICODE_GENERAL_CATEGORY_OTHER_NUMBER
  , UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION
  , UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL
  , UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL
  , UNICODE_GENERAL_CATEGORY_MATH_SYMBOL
  , UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL
  , UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR
  , UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR
  , UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR
  )
, UnicodeGeneralCategoryFunc
, UnicodeMirroringFunc
, UnicodeScriptFunc
, Key(..), OpaqueKey, withKey
, Variation(..)
, variation_to_string, variation_from_string
, pattern VERSION_MAJOR
, pattern VERSION_MINOR
, pattern VERSION_MICRO
-- * internals
, foreignBlob
, foreignBuffer
, foreignFace
, foreignFont
, foreignFontFuncs
, foreignMap
, foreignSet
, foreignShapePlan
, foreignUnicodeFuncs
, _hb_blob_destroy
, _hb_buffer_destroy
, _hb_face_destroy
, _hb_font_destroy
, _hb_font_funcs_destroy
, _hb_map_destroy
, _hb_set_destroy
, _hb_shape_plan_destroy
, _hb_unicode_funcs_destroy
-- * internals
, withSelf, withPtr
, cbool, boolc, w2c,c2w
, newByteStringCStringLen
, harfbuzzCtx
, hs_free_stable_ptr
) where

import Control.Applicative
import Data.Bits
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Internal as Strict
import Data.Char as Char
import Data.Coerce
import Data.Data (Data)
import Data.Default (Default(..))
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Hashable
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Traversable (for)
import Data.Tuple (swap)
import Foreign
import Foreign.C
import Foreign.Marshal.Unsafe
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.HaskellIdentifier as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import Text.Read

#ifndef HLINT
#include <hb.h>
#endif

-- | A 'Blob' wraps a chunk of binary data to handle lifecycle management of data while it is passed between client and HarfBuzz.
-- Blobs are primarily used to create font faces, but also to access font face tables, as well as pass around other binary data.
newtype Blob = Blob (ForeignPtr Blob) deriving (Eq,Ord,Show,Data)


-- | Buffers serve dual role in HarfBuzz; they hold the input characters that are passed to hb_shape(), and after shaping they
--
--hold the output glyphs.
newtype Buffer = Buffer (ForeignPtr Buffer) deriving (Eq,Ord,Show,Data)

newtype BufferClusterLevel = BufferClusterLevel CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

newtype BufferContentType = BufferContentType CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

newtype BufferDiffFlags = BufferDiffFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Bits)

newtype BufferFlags = BufferFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Bits)

newtype BufferSerializeFlags = BufferSerializeFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Bits)

newtype BufferSerializeFormat = BufferSerializeFormat CInt deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)

type Codepoint = Word32

newtype Direction = Direction Word32  deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)

-- | A font 'Face' represents a single face in a font family. More exactly a font 'Face' is a single
-- face in a binary font file. Faces are typically built from a binary blob and a face index. Faces
-- are used to create fonts.
newtype Face = Face (ForeignPtr Face) deriving (Eq,Ord,Show,Data)

data Feature = Feature
  { feature_tag :: {-# unpack #-} !Tag
  , feature_value :: {-# unpack #-} !Word32
  , feature_start, feature_end :: {-# unpack #-} !CUInt
  } deriving (Eq,Ord)

instance Storable Feature where
  sizeOf _ = #size hb_feature_t
  alignment _ = #alignment hb_feature_t
  peek p = Feature
    <$> (#peek hb_feature_t, tag) p
    <*> (#peek hb_feature_t, value) p
    <*> (#peek hb_feature_t, start) p
    <*> (#peek hb_feature_t, end) p
  poke p Feature{..} = do
    (#poke hb_feature_t, tag) p feature_tag
    (#poke hb_feature_t, value) p feature_value
    (#poke hb_feature_t, start) p feature_start
    (#poke hb_feature_t, end) p feature_end

newtype Font = Font (ForeignPtr Font) deriving (Eq,Ord,Show,Data)

data FontExtents = FontExtents
  { font_extents_ascender
  , font_extents_descender
  , font_extents_line_gap
  , font_extents_reserved9
  , font_extents_reserved8
  , font_extents_reserved7
  , font_extents_reserved6
  , font_extents_reserved5
  , font_extents_reserved4
  , font_extents_reserved3
  , font_extents_reserved2
  , font_extents_reserved1 :: {-# unpack #-} !Position
  } deriving (Show,Read)

instance Storable FontExtents where
  sizeOf _ = #size hb_font_extents_t
  alignment _ = #alignment hb_font_extents_t
  peek p = FontExtents
    <$> (#peek hb_font_extents_t, ascender)  p
    <*> (#peek hb_font_extents_t, descender) p
    <*> (#peek hb_font_extents_t, line_gap)  p
    <*> (#peek hb_font_extents_t, reserved9) p
    <*> (#peek hb_font_extents_t, reserved8) p
    <*> (#peek hb_font_extents_t, reserved7) p
    <*> (#peek hb_font_extents_t, reserved6) p
    <*> (#peek hb_font_extents_t, reserved5) p
    <*> (#peek hb_font_extents_t, reserved4) p
    <*> (#peek hb_font_extents_t, reserved3) p
    <*> (#peek hb_font_extents_t, reserved2) p
    <*> (#peek hb_font_extents_t, reserved1) p
  poke p FontExtents{..} = do
    (#poke hb_font_extents_t, ascender)  p font_extents_ascender
    (#poke hb_font_extents_t, descender) p font_extents_descender
    (#poke hb_font_extents_t, line_gap)  p font_extents_line_gap
    (#poke hb_font_extents_t, reserved9) p font_extents_reserved9
    (#poke hb_font_extents_t, reserved8) p font_extents_reserved8
    (#poke hb_font_extents_t, reserved7) p font_extents_reserved7
    (#poke hb_font_extents_t, reserved6) p font_extents_reserved6
    (#poke hb_font_extents_t, reserved5) p font_extents_reserved5
    (#poke hb_font_extents_t, reserved4) p font_extents_reserved4
    (#poke hb_font_extents_t, reserved3) p font_extents_reserved3
    (#poke hb_font_extents_t, reserved2) p font_extents_reserved2
    (#poke hb_font_extents_t, reserved1) p font_extents_reserved1

newtype FontFuncs = FontFuncs (ForeignPtr FontFuncs) deriving (Eq,Ord,Show,Data)

data GlyphExtents = GlyphExtents
  { glyph_extents_x_bearing
  , glyph_extents_y_bearing
  , glyph_extents_width
  , glyph_extents_height :: {-# unpack #-} !Position
  } deriving (Eq,Ord,Show,Read)

instance Storable GlyphExtents where
  sizeOf _ = #size hb_glyph_extents_t
  alignment _ = #alignment hb_glyph_extents_t
  peek p = GlyphExtents
    <$> (#peek hb_glyph_extents_t, x_bearing) p
    <*> (#peek hb_glyph_extents_t, y_bearing) p
    <*> (#peek hb_glyph_extents_t, width) p
    <*> (#peek hb_glyph_extents_t, height) p
  poke p GlyphExtents{..} = do
    (#poke hb_glyph_extents_t, x_bearing) p glyph_extents_x_bearing
    (#poke hb_glyph_extents_t, y_bearing) p glyph_extents_y_bearing
    (#poke hb_glyph_extents_t, width) p glyph_extents_width
    (#poke hb_glyph_extents_t, height) p glyph_extents_height

newtype GlyphFlags = GlyphFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Bits)

newtype GlyphInfo = GlyphInfo (Ptr GlyphInfo) deriving (Eq,Ord,Data,Storable) -- we never manage

data GlyphPosition = GlyphPosition
  { glyph_position_x_advance
  , glyph_position_y_advance
  , glyph_position_x_offset
  , glyph_position_y_offset :: {-# unpack #-} !Position
  } deriving (Eq,Ord,Show,Read)

instance Storable GlyphPosition where
  sizeOf _ = #size hb_glyph_position_t
  alignment _ = #alignment hb_glyph_position_t
  peek p = GlyphPosition
    <$> (#peek hb_glyph_position_t, x_advance) p
    <*> (#peek hb_glyph_position_t, y_advance) p
    <*> (#peek hb_glyph_position_t, x_offset) p
    <*> (#peek hb_glyph_position_t, y_offset) p
  poke p GlyphPosition{..} = do
    (#poke hb_glyph_position_t, x_advance) p glyph_position_x_advance
    (#poke hb_glyph_position_t, y_advance) p glyph_position_y_advance
    (#poke hb_glyph_position_t, x_offset) p glyph_position_x_offset
    (#poke hb_glyph_position_t, y_offset) p glyph_position_y_offset

data OpaqueKey deriving Data
newtype Key a = Key (ForeignPtr OpaqueKey) deriving (Eq,Ord,Show,Data)

newtype Language = Language (Ptr Language) deriving (Eq,Ord,Data,Storable) -- we never manage

newtype Map = Map (ForeignPtr Map) deriving (Eq,Ord,Show,Data)

newtype MemoryMode = MemoryMode CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

type Position = Word32

type BufferMessageFunc a = Ptr Buffer -> Ptr Font -> CString -> Ptr a -> IO ()

type ReferenceTableFunc a = Ptr Face -> Tag -> Ptr a -> IO (Ptr Blob)

newtype Script = Script Word32 deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)
instance Show Script where showsPrec e (Script w) = showsPrec e (Tag w)
instance Read Script where readPrec = fromString <$> step readPrec

data SegmentProperties = SegmentProperties
  { segment_properties_direction :: {-# unpack #-} !Direction
  , segment_properties_script    :: {-# unpack #-} !Script
  , segment_properties_language  :: {-# unpack #-} !Language
  , segment_properties_reserved1 :: {-# unpack #-} !(Ptr ())
  , segment_properties_reserved2 :: {-# unpack #-} !(Ptr ())
  }

instance Storable SegmentProperties where
  sizeOf _ = #size hb_segment_properties_t
  alignment _ = #alignment hb_segment_properties_t
  peek p = SegmentProperties
    <$> (#peek hb_segment_properties_t, direction) p
    <*> (#peek hb_segment_properties_t, script) p
    <*> (#peek hb_segment_properties_t, language) p
    <*> (#peek hb_segment_properties_t, reserved1) p
    <*> (#peek hb_segment_properties_t, reserved2) p
  poke p SegmentProperties{..} = do
    (#poke hb_segment_properties_t, direction) p segment_properties_direction
    (#poke hb_segment_properties_t, script) p segment_properties_script
    (#poke hb_segment_properties_t, language) p segment_properties_language
    (#poke hb_segment_properties_t, reserved1) p segment_properties_reserved1
    (#poke hb_segment_properties_t, reserved2) p segment_properties_reserved2

newtype Set = Set (ForeignPtr Set) deriving (Eq,Ord,Show,Data)

newtype ShapePlan = ShapePlan (ForeignPtr ShapePlan) deriving (Eq,Ord,Show,Data)

newtype Shaper = Shaper CString deriving (Eq,Ord,Storable) -- we never manage

instance Default Shaper where def = Shaper nullPtr

newtype Tag = Tag Word32 deriving (Eq,Ord,Num,Enum,Real,Integral,Storable)
{-# complete Tag #-}
{-# complete TAG #-}

w2c :: Word32 -> Char
w2c = toEnum . fromIntegral

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

untag :: Tag -> (Char, Char, Char, Char)
untag (Tag w) =
  ( w2c (unsafeShiftR w 24 .&. 0xff)
  , w2c (unsafeShiftR w 16 .&. 0xff)
  , w2c (unsafeShiftR w 8 .&. 0xff)
  , w2c (w .&. 0xff)
  )

pattern TAG :: Char -> Char -> Char -> Char -> Tag
pattern TAG a b c d <- (untag -> (a,b,c,d)) where
  TAG a b c d = Tag $
    unsafeShiftL (c2w a .&. 0xff) 24 .|.
    unsafeShiftL (c2w b .&. 0xff) 16 .|.
    unsafeShiftL (c2w c .&. 0xff) 8  .|.
    (c2w d .&. 0xff)

instance Show Tag where showsPrec e (TAG a b c d) = showsPrec e [a,b,c,d]
instance Read Tag where readPrec = fromString <$> step readPrec

instance IsString Tag where
  fromString [] = TAG ' ' ' ' ' ' ' '
  fromString [a] = TAG a ' ' ' ' ' '
  fromString [a,b] = TAG a b ' ' ' '
  fromString [a,b,c] = TAG a b c ' '
  fromString (a:b:c:d:_) = TAG a b c d

newtype UnicodeCombiningClass = UnicodeCombiningClass CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

type UnicodeCombiningClassFunc a = Ptr UnicodeFuncs -> Char -> Ptr a -> IO UnicodeCombiningClass

type UnicodeComposeFunc a = Ptr UnicodeFuncs -> Char -> Char -> Ptr Char -> Ptr a -> IO CInt -- hb_bool_tt

type UnicodeDecomposeFunc a = Ptr UnicodeFuncs -> Char -> Ptr Char -> Ptr Char -> Ptr a -> IO CInt -- hb_bool_t

newtype UnicodeFuncs = UnicodeFuncs (ForeignPtr UnicodeFuncs) deriving (Eq,Ord,Show,Data)

newtype UnicodeGeneralCategory = UnicodeGeneralCategory CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable)

type UnicodeGeneralCategoryFunc a = Ptr UnicodeFuncs -> Char -> Ptr a -> IO UnicodeGeneralCategory

type UnicodeMirroringFunc a = Ptr UnicodeFuncs -> Char -> Ptr a -> IO Char

type UnicodeScriptFunc a = Ptr UnicodeFuncs -> Char -> Ptr a -> IO Script

data Variation = Variation
  { variation_tag :: {-# unpack #-} !Tag
  , variation_value :: {-# unpack #-} !Float
  } deriving (Eq,Ord)

instance Storable Variation where
  sizeOf _ = #size hb_variation_t
  alignment _ = #alignment hb_variation_t
  peek p = Variation
    <$> (#peek hb_variation_t, tag) p
    <*> (#peek hb_variation_t, value) p
  poke p Variation{..} = do
    (#poke hb_variation_t, tag) p variation_tag
    (#poke hb_variation_t, value) p variation_value

-- * Startup a crippled inline-c context for use in non-orphan instances below

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t|Blob|])
    , (C.TypeName "hb_buffer_t", [t|Buffer|])
    , (C.TypeName "hb_buffer_serialize_format_t", [t|BufferSerializeFormat|])
    , (C.TypeName "hb_direction_t", [t|Direction|])
    , (C.TypeName "hb_face_t", [t|Face|])
    , (C.TypeName "hb_feature_t", [t|Feature|])
    , (C.TypeName "hb_font_t", [t|Font|])
    , (C.TypeName "hb_font_funcs_t", [t|FontFuncs|])
    , (C.TypeName "hb_language_t", [t|Ptr Language|])
    , (C.TypeName "hb_language_impl_t", [t|Language|])
    , (C.TypeName "hb_map_t", [t|Map|])
    , (C.TypeName "hb_script_t", [t|Script|])
    , (C.TypeName "hb_set_t", [t|Set|])
    , (C.TypeName "hb_segment_properties_t", [t|SegmentProperties|])
    , (C.TypeName "hb_shape_plan_t", [t|ShapePlan|])
    , (C.TypeName "hb_tag_t", [t|Tag|])
    , (C.TypeName "hb_unicode_funcs_t", [t|UnicodeFuncs|])
    , (C.TypeName "hb_user_data_key_t", [t|ForeignPtr OpaqueKey|])
    , (C.TypeName "hb_variation_t", [t|Variation|])
    , (C.TypeName "hb_bool_t", [t|CInt|])
    ]
  }

C.include "<hb.h>"

instance Default Blob where
  def = unsafeLocalState $ [C.exp|hb_blob_t * { hb_blob_get_empty() }|] >>= fmap Blob . newForeignPtr_
  {-# noinline def #-}

instance Default Buffer where
  def = unsafeLocalState $ [C.exp|hb_buffer_t * { hb_buffer_get_empty() }|] >>= fmap Buffer . newForeignPtr_
  {-# noinline def #-}

instance Default BufferFlags where
  def = BUFFER_FLAG_DEFAULT

instance Default SegmentProperties where
  def = SEGMENT_PROPERTIES_DEFAULT

buffer_serialize_list_strings :: [String]
buffer_serialize_list_strings = unsafeLocalState $ do
  pstrs <- [C.exp|const char ** { hb_buffer_serialize_list_formats() }|]
  cstrs <- peekArray0 nullPtr pstrs
  traverse peekCString cstrs
{-# noinline buffer_serialize_list_strings #-}

buffer_serialize_list_formats :: [(String,BufferSerializeFormat)]
buffer_serialize_list_formats = (\s -> (s, buffer_serialize_format_from_string s)) <$> buffer_serialize_list_strings
{-# noinline buffer_serialize_list_formats #-}

-- dangerous AF, as it doesn't check the format is valid!
buffer_serialize_format_from_string :: String -> BufferSerializeFormat
buffer_serialize_format_from_string s = unsafeLocalState $
  withCStringLen s $ \(cstr,fromIntegral -> l) ->
    [C.exp|hb_buffer_serialize_format_t { hb_buffer_serialize_format_from_string($(const char * cstr),$(int l)) }|]

buffer_serialize_format_to_string :: BufferSerializeFormat -> String
buffer_serialize_format_to_string t = unsafeLocalState $
  [C.exp|const char * { hb_buffer_serialize_format_to_string($(hb_buffer_serialize_format_t t)) }|] >>= peekCString

-- safer than the box of knives that is buffer_serialize_format_from_string!
instance IsString BufferSerializeFormat where
  fromString s = fromMaybe BUFFER_SERIALIZE_FORMAT_INVALID $ lookup s buffer_serialize_list_formats

instance Show BufferSerializeFormat where showsPrec d = showsPrec d . buffer_serialize_format_to_string
instance Read BufferSerializeFormat where readPrec = fromString <$> step readPrec

direction_from_string :: String -> Direction
direction_from_string s = unsafeLocalState $
  withCStringLen (take 1 s) $ \(cstr,fromIntegral -> l) ->
    [C.exp|hb_direction_t { hb_direction_from_string($(const char * cstr),$(int l)) }|]

direction_to_string :: Direction -> String
direction_to_string t = unsafeLocalState $
  [C.exp|const char * { hb_direction_to_string($(hb_direction_t t)) }|] >>= peekCString

instance IsString Direction where fromString = direction_from_string
instance Show Direction where showsPrec d = showsPrec d . direction_to_string
instance Read Direction where readPrec = direction_from_string <$> step readPrec

instance Default Face where
  def = unsafeLocalState $ [C.exp|hb_face_t * { hb_face_get_empty() }|] >>= fmap Face . newForeignPtr_
  {-# noinline def #-}

feature_from_string :: String -> Maybe Feature
feature_from_string s = unsafeLocalState $
  withCStringLen s $ \(cstr,fromIntegral -> len) ->
    alloca $ \feature -> do
      b <- [C.exp|hb_bool_t { hb_feature_from_string($(const char * cstr),$(int len),$(hb_feature_t * feature)) }|]
      if cbool b then Just <$> peek feature else pure Nothing

feature_to_string :: Feature -> String
feature_to_string feature = unsafeLocalState $
  allocaBytes 128 $ \buf ->
    with feature $ \f -> do
      [C.block|void { hb_feature_to_string($(hb_feature_t * f),$(char * buf),128); }|]
      peekCString buf

instance IsString Feature where fromString s = fromMaybe (error $ "invalid feature: " ++ s) $ feature_from_string s
instance Show Feature where showsPrec d = showsPrec d . feature_to_string
instance Read Feature where readPrec = step readPrec >>= maybe empty pure . feature_from_string

instance Default FontFuncs where
  def = unsafeLocalState $
    [C.exp|hb_font_funcs_t * { hb_font_funcs_get_empty() }|] >>= fmap FontFuncs . newForeignPtr_
  {-# noinline def #-}

instance IsString Language where
  fromString s = unsafeLocalState $
    withCStringLen s $ \(cstr,fromIntegral -> l) ->
      Language <$> [C.exp|hb_language_t { hb_language_from_string($(const char * cstr),$(int l)) }|]

instance Show Language where
  showsPrec d = showsPrec d . language_to_string

instance Read Language where
  readPrec = fromString <$> step readPrec

language_from_string :: String -> Language
language_from_string = fromString

language_to_string :: Language -> String
language_to_string (Language l) = unsafeLocalState (peekCString cstr) where
  cstr = [C.pure|const char * { hb_language_to_string($(hb_language_t l)) }|]

instance Default Map where
  def = unsafeLocalState $ [C.exp|hb_map_t * { hb_map_get_empty() }|] >>= fmap Map . newForeignPtr_
  {-# noinline def #-}

instance IsString Script where
  fromString (fromString -> tag) = [C.pure|hb_script_t { hb_script_from_iso15924_tag($(hb_tag_t tag)) }|]

deriving instance Show SegmentProperties

-- | @hb_segment_properties_equal@
deriving instance Eq SegmentProperties
deriving instance Ord SegmentProperties

-- | @hb_segment_properties_hash@
instance Hashable SegmentProperties where
  hashWithSalt i s = hashWithSalt i $ unsafeLocalState $ with s $ \sp -> [C.exp|unsigned int { hb_segment_properties_hash($(hb_segment_properties_t * sp)) }|] <&> (fromIntegral :: CUInt -> Int)

instance Default Set where
  def = unsafeLocalState $
    [C.exp|hb_set_t * { hb_set_get_empty() }|] >>= fmap Set . newForeignPtr_
  {-# noinline def #-}

instance Default ShapePlan where
  def = unsafeLocalState $
    [C.exp|hb_shape_plan_t * { hb_shape_plan_get_empty() }|] >>= fmap ShapePlan . newForeignPtr_
  {-# noinline def #-}

shapers :: [(String,Shaper)]
shapers = unsafeLocalState $ do
  ss <- peekArray0 nullPtr [C.pure|const char ** { hb_shape_list_shapers() }|]
  for ss $ \cstr -> peekCString cstr <&> \ str -> (str, Shaper cstr)
{-# noinline shape_list_shapers #-}

shape_list_shapers :: [Shaper]
shape_list_shapers = fmap snd shapers

instance IsString Shaper where
  fromString s = fromMaybe SHAPER_INVALID $ lookup s shapers

shaper_to_string :: Shaper -> Maybe String
shaper_to_string s = lookup s (swap <$> shapers)

shaper_from_string :: String -> Shaper
shaper_from_string = fromString

instance Show Shaper where
  showsPrec d s = case lookup s (swap <$> shapers) of
    Just t -> showsPrec d t
    Nothing -> showString "SHAPER_INVALID"

instance Read Shaper where
  readPrec = do str <- step readPrec
                maybe empty pure $ lookup str shapers
         <|> do Ident "SHAPER_INVALID" <- lexP
                pure def

instance Default Tag where def = TAG_NONE

-- | Note: @hb_unicode_funcs_get_empty@ not @hb_unicode_funcs_get_default@!
instance Default UnicodeFuncs where
  def = unsafeLocalState $ [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_get_empty() }|] >>= fmap UnicodeFuncs . newForeignPtr_
  {-# noinline def #-}

variation_from_string :: String -> Maybe Variation
variation_from_string s = unsafeLocalState $
  withCStringLen s $ \(cstr,fromIntegral -> len) ->
    alloca $ \variation -> do
      b <- [C.exp|hb_bool_t { hb_variation_from_string($(const char * cstr),$(int len),$(hb_variation_t * variation)) }|]
      if cbool b then Just <$> peek variation else pure Nothing

variation_to_string :: Variation -> String
variation_to_string variation = unsafeLocalState $
  allocaBytes 128 $ \buf ->
    with variation $ \f -> do
      [C.block|void { hb_variation_to_string($(hb_variation_t * f),$(char * buf),128); }|]
      peekCString buf

instance IsString Variation where fromString s = fromMaybe (error $ "invalid variation: " ++ s) $ variation_from_string s
instance Show Variation where showsPrec d = showsPrec d . variation_to_string
instance Read Variation where readPrec = step readPrec >>= maybe empty pure . variation_from_string

-- * helpers

withPtr :: forall a r. Coercible a (Ptr a) => a -> (Ptr a -> IO r) -> IO r
withPtr = (&) . coerce

withSelf :: forall a r. Coercible a (ForeignPtr a) => a -> (Ptr a -> IO r) -> IO r
withSelf = withForeignPtr . coerce

cbool :: CInt -> Bool
cbool = toEnum . fromIntegral

boolc :: Bool -> CInt
boolc = fromIntegral . fromEnum

-- | Copies 'ByteString' to newly allocated 'CString'. The result must be
-- | explicitly freed using 'free' or 'finalizerFree'.
newByteStringCStringLen :: Strict.ByteString -> IO CStringLen
newByteStringCStringLen (Strict.PS fp o l) = do
  buf <- mallocBytes (l + 1)
  withForeignPtr fp $ \p -> do
    Strict.memcpy buf (p `plusPtr` o) l
    pokeByteOff buf l (0::Word8)
    return (castPtr buf, l)

-- * constants

#ifndef HLINT
pattern TAG_NONE = (#const HB_TAG_NONE) :: Tag
pattern TAG_MAX = (#const HB_TAG_MAX) :: Tag
pattern TAG_MAX_SIGNED = (#const HB_TAG_MAX_SIGNED) :: Tag

pattern DIRECTION_INVALID = (#const HB_DIRECTION_INVALID) :: Direction
pattern DIRECTION_LTR = (#const HB_DIRECTION_LTR) :: Direction
pattern DIRECTION_RTL = (#const HB_DIRECTION_RTL) :: Direction
pattern DIRECTION_TTB = (#const HB_DIRECTION_TTB) :: Direction
pattern DIRECTION_BTT = (#const HB_DIRECTION_BTT) :: Direction

{-# complete DIRECTION_INVALID, DIRECTION_LTR, DIRECTION_RTL, DIRECTION_TTB, DIRECTION_BTT #-}

pattern MEMORY_MODE_DUPLICATE = (#const HB_MEMORY_MODE_DUPLICATE) :: MemoryMode
pattern MEMORY_MODE_READONLY = (#const HB_MEMORY_MODE_READONLY) :: MemoryMode
pattern MEMORY_MODE_WRITABLE = (#const HB_MEMORY_MODE_WRITABLE) :: MemoryMode
pattern MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE = (#const HB_MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE) :: MemoryMode

{-# complete
  MEMORY_MODE_DUPLICATE, MEMORY_MODE_READONLY, MEMORY_MODE_WRITABLE,
  MEMORY_MODE_READONLY_MAY_MAKE_WRITABLE #-}

pattern SCRIPT_COMMON = (#const HB_SCRIPT_COMMON) :: Script
pattern SCRIPT_INHERITED = (#const HB_SCRIPT_INHERITED) :: Script
pattern SCRIPT_UNKNOWN = (#const HB_SCRIPT_UNKNOWN) :: Script
pattern SCRIPT_ARABIC = (#const HB_SCRIPT_ARABIC) :: Script
pattern SCRIPT_ARMENIAN = (#const HB_SCRIPT_ARMENIAN) :: Script
pattern SCRIPT_BENGALI = (#const HB_SCRIPT_BENGALI) :: Script
pattern SCRIPT_CYRILLIC = (#const HB_SCRIPT_CYRILLIC) :: Script
pattern SCRIPT_DEVANAGARI = (#const HB_SCRIPT_DEVANAGARI) :: Script
pattern SCRIPT_GEORGIAN = (#const HB_SCRIPT_GEORGIAN) :: Script
pattern SCRIPT_GREEK = (#const HB_SCRIPT_GREEK) :: Script
pattern SCRIPT_GUJARATI = (#const HB_SCRIPT_GUJARATI) :: Script
pattern SCRIPT_GURMUKHI = (#const HB_SCRIPT_GURMUKHI) :: Script
pattern SCRIPT_HANGUL = (#const HB_SCRIPT_HANGUL) :: Script
pattern SCRIPT_HAN = (#const HB_SCRIPT_HAN) :: Script
pattern SCRIPT_HEBREW = (#const HB_SCRIPT_HEBREW) :: Script
pattern SCRIPT_HIRAGANA = (#const HB_SCRIPT_HIRAGANA) :: Script
pattern SCRIPT_KANNADA = (#const HB_SCRIPT_KANNADA) :: Script
pattern SCRIPT_KATAKANA = (#const HB_SCRIPT_KATAKANA) :: Script
pattern SCRIPT_LAO = (#const HB_SCRIPT_LAO) :: Script
pattern SCRIPT_LATIN = (#const HB_SCRIPT_LATIN) :: Script
pattern SCRIPT_MALAYALAM = (#const HB_SCRIPT_MALAYALAM) :: Script
pattern SCRIPT_ORIYA = (#const HB_SCRIPT_ORIYA) :: Script
pattern SCRIPT_TAMIL = (#const HB_SCRIPT_TAMIL) :: Script
pattern SCRIPT_TELUGU = (#const HB_SCRIPT_TELUGU) :: Script
pattern SCRIPT_THAI = (#const HB_SCRIPT_THAI) :: Script
pattern SCRIPT_TIBETAN = (#const HB_SCRIPT_TIBETAN) :: Script
pattern SCRIPT_BOPOMOFO = (#const HB_SCRIPT_BOPOMOFO) :: Script
pattern SCRIPT_BRAILLE = (#const HB_SCRIPT_BRAILLE) :: Script
pattern SCRIPT_CANADIAN_SYLLABICS = (#const HB_SCRIPT_CANADIAN_SYLLABICS) :: Script
pattern SCRIPT_CHEROKEE = (#const HB_SCRIPT_CHEROKEE) :: Script
pattern SCRIPT_ETHIOPIC = (#const HB_SCRIPT_ETHIOPIC) :: Script
pattern SCRIPT_KHMER = (#const HB_SCRIPT_KHMER) :: Script
pattern SCRIPT_MONGOLIAN = (#const HB_SCRIPT_MONGOLIAN) :: Script
pattern SCRIPT_MYANMAR = (#const HB_SCRIPT_MYANMAR) :: Script
pattern SCRIPT_OGHAM = (#const HB_SCRIPT_OGHAM) :: Script
pattern SCRIPT_RUNIC = (#const HB_SCRIPT_RUNIC) :: Script
pattern SCRIPT_SINHALA = (#const HB_SCRIPT_SINHALA) :: Script
pattern SCRIPT_SYRIAC = (#const HB_SCRIPT_SYRIAC) :: Script
pattern SCRIPT_THAANA = (#const HB_SCRIPT_THAANA) :: Script
pattern SCRIPT_YI = (#const HB_SCRIPT_YI) :: Script
pattern SCRIPT_DESERET = (#const HB_SCRIPT_DESERET) :: Script
pattern SCRIPT_GOTHIC = (#const HB_SCRIPT_GOTHIC) :: Script
pattern SCRIPT_OLD_ITALIC = (#const HB_SCRIPT_OLD_ITALIC) :: Script
pattern SCRIPT_BUHID = (#const HB_SCRIPT_BUHID) :: Script
pattern SCRIPT_HANUNOO = (#const HB_SCRIPT_HANUNOO) :: Script
pattern SCRIPT_TAGALOG = (#const HB_SCRIPT_TAGALOG) :: Script
pattern SCRIPT_TAGBANWA = (#const HB_SCRIPT_TAGBANWA) :: Script
pattern SCRIPT_CYPRIOT = (#const HB_SCRIPT_CYPRIOT) :: Script
pattern SCRIPT_LIMBU = (#const HB_SCRIPT_LIMBU) :: Script
pattern SCRIPT_LINEAR_B = (#const HB_SCRIPT_LINEAR_B) :: Script
pattern SCRIPT_OSMANYA = (#const HB_SCRIPT_OSMANYA) :: Script
pattern SCRIPT_SHAVIAN = (#const HB_SCRIPT_SHAVIAN) :: Script
pattern SCRIPT_TAI_LE = (#const HB_SCRIPT_TAI_LE) :: Script
pattern SCRIPT_UGARITIC = (#const HB_SCRIPT_UGARITIC) :: Script
pattern SCRIPT_BUGINESE = (#const HB_SCRIPT_BUGINESE) :: Script
pattern SCRIPT_COPTIC = (#const HB_SCRIPT_COPTIC) :: Script
pattern SCRIPT_GLAGOLITIC = (#const HB_SCRIPT_GLAGOLITIC) :: Script
pattern SCRIPT_KHAROSHTHI = (#const HB_SCRIPT_KHAROSHTHI) :: Script
pattern SCRIPT_NEW_TAI_LUE = (#const HB_SCRIPT_NEW_TAI_LUE) :: Script
pattern SCRIPT_OLD_PERSIAN = (#const HB_SCRIPT_OLD_PERSIAN) :: Script
pattern SCRIPT_SYLOTI_NAGRI = (#const HB_SCRIPT_SYLOTI_NAGRI) :: Script
pattern SCRIPT_TIFINAGH = (#const HB_SCRIPT_TIFINAGH) :: Script
pattern SCRIPT_BALINESE = (#const HB_SCRIPT_BALINESE) :: Script
pattern SCRIPT_CUNEIFORM = (#const HB_SCRIPT_CUNEIFORM) :: Script
pattern SCRIPT_NKO = (#const HB_SCRIPT_NKO) :: Script
pattern SCRIPT_PHAGS_PA = (#const HB_SCRIPT_PHAGS_PA) :: Script
pattern SCRIPT_PHOENICIAN = (#const HB_SCRIPT_PHOENICIAN) :: Script
pattern SCRIPT_CARIAN = (#const HB_SCRIPT_CARIAN) :: Script
pattern SCRIPT_CHAM = (#const HB_SCRIPT_CHAM) :: Script
pattern SCRIPT_KAYAH_LI = (#const HB_SCRIPT_KAYAH_LI) :: Script
pattern SCRIPT_LEPCHA = (#const HB_SCRIPT_LEPCHA) :: Script
pattern SCRIPT_LYCIAN = (#const HB_SCRIPT_LYCIAN) :: Script
pattern SCRIPT_LYDIAN = (#const HB_SCRIPT_LYDIAN) :: Script
pattern SCRIPT_OL_CHIKI = (#const HB_SCRIPT_OL_CHIKI) :: Script
pattern SCRIPT_REJANG = (#const HB_SCRIPT_REJANG) :: Script
pattern SCRIPT_SAURASHTRA = (#const HB_SCRIPT_SAURASHTRA) :: Script
pattern SCRIPT_SUNDANESE = (#const HB_SCRIPT_SUNDANESE) :: Script
pattern SCRIPT_VAI = (#const HB_SCRIPT_VAI) :: Script
pattern SCRIPT_AVESTAN = (#const HB_SCRIPT_AVESTAN) :: Script
pattern SCRIPT_BAMUM = (#const HB_SCRIPT_BAMUM) :: Script
pattern SCRIPT_EGYPTIAN_HIEROGLYPHS = (#const HB_SCRIPT_EGYPTIAN_HIEROGLYPHS) :: Script
pattern SCRIPT_IMPERIAL_ARAMAIC = (#const HB_SCRIPT_IMPERIAL_ARAMAIC) :: Script
pattern SCRIPT_INSCRIPTIONAL_PAHLAVI = (#const HB_SCRIPT_INSCRIPTIONAL_PAHLAVI) :: Script
pattern SCRIPT_INSCRIPTIONAL_PARTHIAN = (#const HB_SCRIPT_INSCRIPTIONAL_PARTHIAN) :: Script
pattern SCRIPT_JAVANESE = (#const HB_SCRIPT_JAVANESE) :: Script
pattern SCRIPT_KAITHI = (#const HB_SCRIPT_KAITHI) :: Script
pattern SCRIPT_LISU = (#const HB_SCRIPT_LISU) :: Script
pattern SCRIPT_MEETEI_MAYEK = (#const HB_SCRIPT_MEETEI_MAYEK) :: Script
pattern SCRIPT_OLD_SOUTH_ARABIAN = (#const HB_SCRIPT_OLD_SOUTH_ARABIAN) :: Script
pattern SCRIPT_OLD_TURKIC = (#const HB_SCRIPT_OLD_TURKIC) :: Script
pattern SCRIPT_SAMARITAN = (#const HB_SCRIPT_SAMARITAN) :: Script
pattern SCRIPT_TAI_THAM = (#const HB_SCRIPT_TAI_THAM) :: Script
pattern SCRIPT_TAI_VIET = (#const HB_SCRIPT_TAI_VIET) :: Script
pattern SCRIPT_BATAK = (#const HB_SCRIPT_BATAK) :: Script
pattern SCRIPT_BRAHMI = (#const HB_SCRIPT_BRAHMI) :: Script
pattern SCRIPT_MANDAIC = (#const HB_SCRIPT_MANDAIC) :: Script
pattern SCRIPT_CHAKMA = (#const HB_SCRIPT_CHAKMA) :: Script
pattern SCRIPT_MEROITIC_CURSIVE = (#const HB_SCRIPT_MEROITIC_CURSIVE) :: Script
pattern SCRIPT_MEROITIC_HIEROGLYPHS = (#const HB_SCRIPT_MEROITIC_HIEROGLYPHS) :: Script
pattern SCRIPT_MIAO = (#const HB_SCRIPT_MIAO) :: Script
pattern SCRIPT_SHARADA = (#const HB_SCRIPT_SHARADA) :: Script
pattern SCRIPT_SORA_SOMPENG = (#const HB_SCRIPT_SORA_SOMPENG) :: Script
pattern SCRIPT_TAKRI = (#const HB_SCRIPT_TAKRI) :: Script
pattern SCRIPT_BASSA_VAH = (#const HB_SCRIPT_BASSA_VAH) :: Script
pattern SCRIPT_CAUCASIAN_ALBANIAN = (#const HB_SCRIPT_CAUCASIAN_ALBANIAN) :: Script
pattern SCRIPT_DUPLOYAN = (#const HB_SCRIPT_DUPLOYAN) :: Script
pattern SCRIPT_ELBASAN = (#const HB_SCRIPT_ELBASAN) :: Script
pattern SCRIPT_GRANTHA = (#const HB_SCRIPT_GRANTHA) :: Script
pattern SCRIPT_KHOJKI = (#const HB_SCRIPT_KHOJKI) :: Script
pattern SCRIPT_KHUDAWADI = (#const HB_SCRIPT_KHUDAWADI) :: Script
pattern SCRIPT_LINEAR_A = (#const HB_SCRIPT_LINEAR_A) :: Script
pattern SCRIPT_MAHAJANI = (#const HB_SCRIPT_MAHAJANI) :: Script
pattern SCRIPT_MANICHAEAN = (#const HB_SCRIPT_MANICHAEAN) :: Script
pattern SCRIPT_MENDE_KIKAKUI = (#const HB_SCRIPT_MENDE_KIKAKUI) :: Script
pattern SCRIPT_MODI = (#const HB_SCRIPT_MODI) :: Script
pattern SCRIPT_MRO = (#const HB_SCRIPT_MRO) :: Script
pattern SCRIPT_NABATAEAN = (#const HB_SCRIPT_NABATAEAN) :: Script
pattern SCRIPT_OLD_NORTH_ARABIAN = (#const HB_SCRIPT_OLD_NORTH_ARABIAN) :: Script
pattern SCRIPT_OLD_PERMIC = (#const HB_SCRIPT_OLD_PERMIC) :: Script
pattern SCRIPT_PAHAWH_HMONG = (#const HB_SCRIPT_PAHAWH_HMONG) :: Script
pattern SCRIPT_PALMYRENE = (#const HB_SCRIPT_PALMYRENE) :: Script
pattern SCRIPT_PAU_CIN_HAU = (#const HB_SCRIPT_PAU_CIN_HAU) :: Script
pattern SCRIPT_PSALTER_PAHLAVI = (#const HB_SCRIPT_PSALTER_PAHLAVI) :: Script
pattern SCRIPT_SIDDHAM = (#const HB_SCRIPT_SIDDHAM) :: Script
pattern SCRIPT_TIRHUTA = (#const HB_SCRIPT_TIRHUTA) :: Script
pattern SCRIPT_WARANG_CITI = (#const HB_SCRIPT_WARANG_CITI) :: Script
pattern SCRIPT_AHOM = (#const HB_SCRIPT_AHOM) :: Script
pattern SCRIPT_ANATOLIAN_HIEROGLYPHS = (#const HB_SCRIPT_ANATOLIAN_HIEROGLYPHS) :: Script
pattern SCRIPT_HATRAN = (#const HB_SCRIPT_HATRAN) :: Script
pattern SCRIPT_MULTANI = (#const HB_SCRIPT_MULTANI) :: Script
pattern SCRIPT_OLD_HUNGARIAN = (#const HB_SCRIPT_OLD_HUNGARIAN) :: Script
pattern SCRIPT_SIGNWRITING = (#const HB_SCRIPT_SIGNWRITING) :: Script
pattern SCRIPT_ADLAM = (#const HB_SCRIPT_ADLAM) :: Script
pattern SCRIPT_BHAIKSUKI = (#const HB_SCRIPT_BHAIKSUKI) :: Script
pattern SCRIPT_MARCHEN = (#const HB_SCRIPT_MARCHEN) :: Script
pattern SCRIPT_OSAGE = (#const HB_SCRIPT_OSAGE) :: Script
pattern SCRIPT_TANGUT = (#const HB_SCRIPT_TANGUT) :: Script
pattern SCRIPT_NEWA = (#const HB_SCRIPT_NEWA) :: Script
pattern SCRIPT_MASARAM_GONDI = (#const HB_SCRIPT_MASARAM_GONDI) :: Script
pattern SCRIPT_NUSHU = (#const HB_SCRIPT_NUSHU) :: Script
pattern SCRIPT_SOYOMBO = (#const HB_SCRIPT_SOYOMBO) :: Script
pattern SCRIPT_ZANABAZAR_SQUARE = (#const HB_SCRIPT_ZANABAZAR_SQUARE) :: Script
pattern SCRIPT_DOGRA = (#const HB_SCRIPT_DOGRA) :: Script
pattern SCRIPT_GUNJALA_GONDI = (#const HB_SCRIPT_GUNJALA_GONDI) :: Script
pattern SCRIPT_HANIFI_ROHINGYA = (#const HB_SCRIPT_HANIFI_ROHINGYA) :: Script
pattern SCRIPT_MAKASAR = (#const HB_SCRIPT_MAKASAR) :: Script
pattern SCRIPT_MEDEFAIDRIN = (#const HB_SCRIPT_MEDEFAIDRIN) :: Script
pattern SCRIPT_OLD_SOGDIAN = (#const HB_SCRIPT_OLD_SOGDIAN) :: Script
pattern SCRIPT_SOGDIAN = (#const HB_SCRIPT_SOGDIAN) :: Script
pattern SCRIPT_ELYMAIC = (#const HB_SCRIPT_ELYMAIC) :: Script
pattern SCRIPT_NANDINAGARI = (#const HB_SCRIPT_NANDINAGARI) :: Script
pattern SCRIPT_NYIAKENG_PUACHUE_HMONG = (#const HB_SCRIPT_NYIAKENG_PUACHUE_HMONG) :: Script
pattern SCRIPT_WANCHO = (#const HB_SCRIPT_WANCHO) :: Script
pattern SCRIPT_INVALID = (#const HB_SCRIPT_INVALID) :: Script
pattern SCRIPT__MAX_VALUE = (#const _HB_SCRIPT_MAX_VALUE) :: Script
pattern SCRIPT__MAX_VALUE_SIGNED = (#const _HB_SCRIPT_MAX_VALUE_SIGNED) :: Script

pattern BUFFER_FLAG_DEFAULT = (#const HB_BUFFER_FLAG_DEFAULT) :: BufferFlags
pattern BUFFER_FLAG_BOT = (#const HB_BUFFER_FLAG_BOT) :: BufferFlags
pattern BUFFER_FLAG_EOT = (#const HB_BUFFER_FLAG_EOT) :: BufferFlags
pattern BUFFER_FLAG_PRESERVE_DEFAULT_IGNORABLES = (#const HB_BUFFER_FLAG_PRESERVE_DEFAULT_IGNORABLES) :: BufferFlags
pattern BUFFER_FLAG_REMOVE_DEFAULT_IGNORABLES = (#const HB_BUFFER_FLAG_REMOVE_DEFAULT_IGNORABLES) :: BufferFlags
pattern BUFFER_FLAG_DO_NOT_INSERT_DOTTED_CIRCLE = (#const HB_BUFFER_FLAG_DO_NOT_INSERT_DOTTED_CIRCLE) :: BufferFlags

-- | Return cluster values grouped by graphemes into monotone order.
pattern BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES  = (#const HB_BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES) :: BufferClusterLevel
-- | Return cluster values grouped into monotone order.
pattern BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS = (#const HB_BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS) :: BufferClusterLevel
-- | Don't group cluster values
pattern BUFFER_CLUSTER_LEVEL_CHARACTERS = (#const HB_BUFFER_CLUSTER_LEVEL_CHARACTERS) :: BufferClusterLevel
-- | Default cluster level, equal to 'BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES'
pattern BUFFER_CLUSTER_LEVEL_DEFAULT = (#const HB_BUFFER_CLUSTER_LEVEL_DEFAULT) :: BufferClusterLevel

{-# complete
  BUFFER_CLUSTER_LEVEL_MONOTONE_GRAPHEMES,
  BUFFER_CLUSTER_LEVEL_MONOTONE_CHARACTERS,
  BUFFER_CLUSTER_LEVEL_CHARACTERS #-}

instance Default BufferClusterLevel where
  def = BUFFER_CLUSTER_LEVEL_DEFAULT

pattern BUFFER_CONTENT_TYPE_INVALID = (#const HB_BUFFER_CONTENT_TYPE_INVALID) :: BufferContentType
pattern BUFFER_CONTENT_TYPE_UNICODE = (#const HB_BUFFER_CONTENT_TYPE_UNICODE) :: BufferContentType
pattern BUFFER_CONTENT_TYPE_GLYPHS = (#const HB_BUFFER_CONTENT_TYPE_GLYPHS) :: BufferContentType

{-# complete
  BUFFER_CONTENT_TYPE_INVALID,
  BUFFER_CONTENT_TYPE_UNICODE,
  BUFFER_CONTENT_TYPE_GLYPHS #-}

pattern BUFFER_SERIALIZE_FORMAT_TEXT = (#const HB_BUFFER_SERIALIZE_FORMAT_TEXT) :: BufferSerializeFormat
pattern BUFFER_SERIALIZE_FORMAT_JSON = (#const HB_BUFFER_SERIALIZE_FORMAT_JSON) :: BufferSerializeFormat
pattern BUFFER_SERIALIZE_FORMAT_INVALID = (#const HB_BUFFER_SERIALIZE_FORMAT_INVALID) :: BufferSerializeFormat

{-# complete
  BUFFER_SERIALIZE_FORMAT_TEXT,
  BUFFER_SERIALIZE_FORMAT_JSON,
  BUFFER_SERIALIZE_FORMAT_INVALID #-}

pattern NULL :: Ptr a
pattern NULL <- ((nullPtr ==) -> True) where
  NULL = nullPtr

pattern LANGUAGE_INVALID = Language NULL :: Language

pattern SEGMENT_PROPERTIES_DEFAULT :: SegmentProperties
pattern SEGMENT_PROPERTIES_DEFAULT = SegmentProperties DIRECTION_INVALID SCRIPT_INVALID LANGUAGE_INVALID NULL NULL :: SegmentProperties

pattern UNICODE_GENERAL_CATEGORY_CONTROL = (#const HB_UNICODE_GENERAL_CATEGORY_CONTROL) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_FORMAT = (#const HB_UNICODE_GENERAL_CATEGORY_FORMAT) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_UNASSIGNED = (#const HB_UNICODE_GENERAL_CATEGORY_UNASSIGNED) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_PRIVATE_USE = (#const HB_UNICODE_GENERAL_CATEGORY_PRIVATE_USE) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_SURROGATE = (#const HB_UNICODE_GENERAL_CATEGORY_SURROGATE) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER = (#const HB_UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER = (#const HB_UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_OTHER_LETTER = (#const HB_UNICODE_GENERAL_CATEGORY_OTHER_LETTER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER = (#const HB_UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER = (#const HB_UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_SPACING_MARK = (#const HB_UNICODE_GENERAL_CATEGORY_SPACING_MARK) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK = (#const HB_UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK = (#const HB_UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER = (#const HB_UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_LETTER_NUMBER = (#const HB_UNICODE_GENERAL_CATEGORY_LETTER_NUMBER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_OTHER_NUMBER = (#const HB_UNICODE_GENERAL_CATEGORY_OTHER_NUMBER) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION = (#const HB_UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL = (#const HB_UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL = (#const HB_UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_MATH_SYMBOL = (#const HB_UNICODE_GENERAL_CATEGORY_MATH_SYMBOL) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL = (#const HB_UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR = (#const HB_UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR = (#const HB_UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR) :: UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR = (#const HB_UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR) :: UnicodeGeneralCategory


{-# complete
  UNICODE_GENERAL_CATEGORY_CONTROL, UNICODE_GENERAL_CATEGORY_FORMAT, UNICODE_GENERAL_CATEGORY_UNASSIGNED,
  UNICODE_GENERAL_CATEGORY_PRIVATE_USE, UNICODE_GENERAL_CATEGORY_SURROGATE, UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER,
  UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER, UNICODE_GENERAL_CATEGORY_OTHER_LETTER, UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER,
  UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER, UNICODE_GENERAL_CATEGORY_SPACING_MARK, UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK,
  UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK, UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER, UNICODE_GENERAL_CATEGORY_LETTER_NUMBER,
  UNICODE_GENERAL_CATEGORY_OTHER_NUMBER, UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION, UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION,
  UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION, UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION, UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION,
  UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION, UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION, UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL,
  UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL, UNICODE_GENERAL_CATEGORY_MATH_SYMBOL, UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL,
  UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR, UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR, UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR
  #-}

charGeneralCategoryToUnicodeGeneralCategory :: Char.GeneralCategory -> UnicodeGeneralCategory
charGeneralCategoryToUnicodeGeneralCategory = \case
  Control -> UNICODE_GENERAL_CATEGORY_CONTROL
  Format -> UNICODE_GENERAL_CATEGORY_FORMAT
  NotAssigned -> UNICODE_GENERAL_CATEGORY_UNASSIGNED
  PrivateUse -> UNICODE_GENERAL_CATEGORY_PRIVATE_USE
  Surrogate -> UNICODE_GENERAL_CATEGORY_SURROGATE
  LowercaseLetter -> UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER
  ModifierLetter -> UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER
  OtherLetter -> UNICODE_GENERAL_CATEGORY_OTHER_LETTER
  TitlecaseLetter -> UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER
  UppercaseLetter -> UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER
  SpacingCombiningMark -> UNICODE_GENERAL_CATEGORY_SPACING_MARK
  EnclosingMark -> UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK
  NonSpacingMark -> UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK
  DecimalNumber -> UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER
  LetterNumber -> UNICODE_GENERAL_CATEGORY_LETTER_NUMBER
  OtherNumber -> UNICODE_GENERAL_CATEGORY_OTHER_NUMBER
  ConnectorPunctuation -> UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION
  DashPunctuation -> UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION
  ClosePunctuation -> UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION
  FinalQuote -> UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION
  InitialQuote -> UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION
  OtherPunctuation -> UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION
  OpenPunctuation -> UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION
  CurrencySymbol -> UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL
  ModifierSymbol -> UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL
  MathSymbol -> UNICODE_GENERAL_CATEGORY_MATH_SYMBOL
  OtherSymbol -> UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL
  LineSeparator -> UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR
  ParagraphSeparator -> UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR
  Space -> UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR

unicodeGeneralCategoryToCharGeneralCategory :: UnicodeGeneralCategory -> Char.GeneralCategory
unicodeGeneralCategoryToCharGeneralCategory = \case
 UNICODE_GENERAL_CATEGORY_CONTROL -> Control
 UNICODE_GENERAL_CATEGORY_FORMAT -> Format
 UNICODE_GENERAL_CATEGORY_UNASSIGNED -> NotAssigned
 UNICODE_GENERAL_CATEGORY_PRIVATE_USE -> PrivateUse
 UNICODE_GENERAL_CATEGORY_SURROGATE -> Surrogate
 UNICODE_GENERAL_CATEGORY_LOWERCASE_LETTER -> LowercaseLetter
 UNICODE_GENERAL_CATEGORY_MODIFIER_LETTER -> ModifierLetter
 UNICODE_GENERAL_CATEGORY_OTHER_LETTER -> OtherLetter
 UNICODE_GENERAL_CATEGORY_TITLECASE_LETTER -> TitlecaseLetter
 UNICODE_GENERAL_CATEGORY_UPPERCASE_LETTER -> UppercaseLetter
 UNICODE_GENERAL_CATEGORY_SPACING_MARK -> SpacingCombiningMark
 UNICODE_GENERAL_CATEGORY_ENCLOSING_MARK -> EnclosingMark
 UNICODE_GENERAL_CATEGORY_NON_SPACING_MARK -> NonSpacingMark
 UNICODE_GENERAL_CATEGORY_DECIMAL_NUMBER -> DecimalNumber
 UNICODE_GENERAL_CATEGORY_LETTER_NUMBER -> LetterNumber
 UNICODE_GENERAL_CATEGORY_OTHER_NUMBER -> OtherNumber
 UNICODE_GENERAL_CATEGORY_CONNECT_PUNCTUATION -> ConnectorPunctuation
 UNICODE_GENERAL_CATEGORY_DASH_PUNCTUATION -> DashPunctuation
 UNICODE_GENERAL_CATEGORY_CLOSE_PUNCTUATION -> ClosePunctuation
 UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION -> FinalQuote
 UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION -> InitialQuote
 UNICODE_GENERAL_CATEGORY_OTHER_PUNCTUATION -> OtherPunctuation
 UNICODE_GENERAL_CATEGORY_OPEN_PUNCTUATION -> OpenPunctuation
 UNICODE_GENERAL_CATEGORY_CURRENCY_SYMBOL -> CurrencySymbol
 UNICODE_GENERAL_CATEGORY_MODIFIER_SYMBOL -> ModifierSymbol
 UNICODE_GENERAL_CATEGORY_MATH_SYMBOL -> MathSymbol
 UNICODE_GENERAL_CATEGORY_OTHER_SYMBOL -> OtherSymbol
 UNICODE_GENERAL_CATEGORY_LINE_SEPARATOR -> LineSeparator
 UNICODE_GENERAL_CATEGORY_PARAGRAPH_SEPARATOR -> ParagraphSeparator
 UNICODE_GENERAL_CATEGORY_SPACE_SEPARATOR -> Space
 x -> error $  "Unknown UnicodeGeneralCategory " ++ show x

-- | 'Char.GeneralCategory' is missing 'UNICODE_GENERAL_CATEGORY_FINAL_PUNCTUATION' and 'UNICODE_GENERAL_CATEGORY_INITIAL_PUNCTUATION'
pattern UNICODE_GENERAL_CATEGORY :: Char.GeneralCategory -> UnicodeGeneralCategory
pattern UNICODE_GENERAL_CATEGORY x <- (unicodeGeneralCategoryToCharGeneralCategory -> x) where
  UNICODE_GENERAL_CATEGORY y = charGeneralCategoryToUnicodeGeneralCategory y

{-# complete UNICODE_GENERAL_CATEGORY #-}

pattern UNICODE_COMBINING_CLASS_NOT_REORDERED = (#const HB_UNICODE_COMBINING_CLASS_NOT_REORDERED) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_OVERLAY = (#const HB_UNICODE_COMBINING_CLASS_OVERLAY) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_NUKTA = (#const HB_UNICODE_COMBINING_CLASS_NUKTA) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_KANA_VOICING = (#const HB_UNICODE_COMBINING_CLASS_KANA_VOICING) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_VIRAMA = (#const HB_UNICODE_COMBINING_CLASS_VIRAMA) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC10 = (#const HB_UNICODE_COMBINING_CLASS_CCC10) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC11 = (#const HB_UNICODE_COMBINING_CLASS_CCC11) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC12 = (#const HB_UNICODE_COMBINING_CLASS_CCC12) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC13 = (#const HB_UNICODE_COMBINING_CLASS_CCC13) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC14 = (#const HB_UNICODE_COMBINING_CLASS_CCC14) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC15 = (#const HB_UNICODE_COMBINING_CLASS_CCC15) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC16 = (#const HB_UNICODE_COMBINING_CLASS_CCC16) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC17 = (#const HB_UNICODE_COMBINING_CLASS_CCC17) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC18 = (#const HB_UNICODE_COMBINING_CLASS_CCC18) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC19 = (#const HB_UNICODE_COMBINING_CLASS_CCC19) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC20 = (#const HB_UNICODE_COMBINING_CLASS_CCC20) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC21 = (#const HB_UNICODE_COMBINING_CLASS_CCC21) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC22 = (#const HB_UNICODE_COMBINING_CLASS_CCC22) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC23 = (#const HB_UNICODE_COMBINING_CLASS_CCC23) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC24 = (#const HB_UNICODE_COMBINING_CLASS_CCC24) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC25 = (#const HB_UNICODE_COMBINING_CLASS_CCC25) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC26 = (#const HB_UNICODE_COMBINING_CLASS_CCC26) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC27 = (#const HB_UNICODE_COMBINING_CLASS_CCC27) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC28 = (#const HB_UNICODE_COMBINING_CLASS_CCC28) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC29 = (#const HB_UNICODE_COMBINING_CLASS_CCC29) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC30 = (#const HB_UNICODE_COMBINING_CLASS_CCC30) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC31 = (#const HB_UNICODE_COMBINING_CLASS_CCC31) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC32 = (#const HB_UNICODE_COMBINING_CLASS_CCC32) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC33 = (#const HB_UNICODE_COMBINING_CLASS_CCC33) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC34 = (#const HB_UNICODE_COMBINING_CLASS_CCC34) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC35 = (#const HB_UNICODE_COMBINING_CLASS_CCC35) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC36 = (#const HB_UNICODE_COMBINING_CLASS_CCC36) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC84 = (#const HB_UNICODE_COMBINING_CLASS_CCC84) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC91 = (#const HB_UNICODE_COMBINING_CLASS_CCC91) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC103 = (#const HB_UNICODE_COMBINING_CLASS_CCC103) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC107 = (#const HB_UNICODE_COMBINING_CLASS_CCC107) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC118 = (#const HB_UNICODE_COMBINING_CLASS_CCC118) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC122 = (#const HB_UNICODE_COMBINING_CLASS_CCC122) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC129 = (#const HB_UNICODE_COMBINING_CLASS_CCC129) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC130 = (#const HB_UNICODE_COMBINING_CLASS_CCC130) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_CCC133 = (#const HB_UNICODE_COMBINING_CLASS_CCC133) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ATTACHED_BELOW_LEFT = (#const HB_UNICODE_COMBINING_CLASS_ATTACHED_BELOW_LEFT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ATTACHED_BELOW = (#const HB_UNICODE_COMBINING_CLASS_ATTACHED_BELOW) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ATTACHED_ABOVE = (#const HB_UNICODE_COMBINING_CLASS_ATTACHED_ABOVE) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ATTACHED_ABOVE_RIGHT = (#const HB_UNICODE_COMBINING_CLASS_ATTACHED_ABOVE_RIGHT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_BELOW_LEFT = (#const HB_UNICODE_COMBINING_CLASS_BELOW_LEFT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_BELOW = (#const HB_UNICODE_COMBINING_CLASS_BELOW) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_BELOW_RIGHT = (#const HB_UNICODE_COMBINING_CLASS_BELOW_RIGHT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_LEFT = (#const HB_UNICODE_COMBINING_CLASS_LEFT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_RIGHT = (#const HB_UNICODE_COMBINING_CLASS_RIGHT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ABOVE_LEFT = (#const HB_UNICODE_COMBINING_CLASS_ABOVE_LEFT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ABOVE = (#const HB_UNICODE_COMBINING_CLASS_ABOVE) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_ABOVE_RIGHT = (#const HB_UNICODE_COMBINING_CLASS_ABOVE_RIGHT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_DOUBLE_BELOW = (#const HB_UNICODE_COMBINING_CLASS_DOUBLE_BELOW) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_DOUBLE_ABOVE = (#const HB_UNICODE_COMBINING_CLASS_DOUBLE_ABOVE) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_IOTA_SUBSCRIPT = (#const HB_UNICODE_COMBINING_CLASS_IOTA_SUBSCRIPT) :: UnicodeCombiningClass
pattern UNICODE_COMBINING_CLASS_INVALID = (#const HB_UNICODE_COMBINING_CLASS_INVALID) :: UnicodeCombiningClass

{-# complete
  UNICODE_COMBINING_CLASS_NOT_REORDERED, UNICODE_COMBINING_CLASS_OVERLAY,
  UNICODE_COMBINING_CLASS_NUKTA, UNICODE_COMBINING_CLASS_KANA_VOICING,
  UNICODE_COMBINING_CLASS_VIRAMA,
  UNICODE_COMBINING_CLASS_CCC10, UNICODE_COMBINING_CLASS_CCC11, UNICODE_COMBINING_CLASS_CCC12,
  UNICODE_COMBINING_CLASS_CCC13, UNICODE_COMBINING_CLASS_CCC14, UNICODE_COMBINING_CLASS_CCC15,
  UNICODE_COMBINING_CLASS_CCC16, UNICODE_COMBINING_CLASS_CCC17, UNICODE_COMBINING_CLASS_CCC18,
  UNICODE_COMBINING_CLASS_CCC19, UNICODE_COMBINING_CLASS_CCC20, UNICODE_COMBINING_CLASS_CCC21,
  UNICODE_COMBINING_CLASS_CCC22, UNICODE_COMBINING_CLASS_CCC23, UNICODE_COMBINING_CLASS_CCC24,
  UNICODE_COMBINING_CLASS_CCC25, UNICODE_COMBINING_CLASS_CCC26, UNICODE_COMBINING_CLASS_CCC27,
  UNICODE_COMBINING_CLASS_CCC28, UNICODE_COMBINING_CLASS_CCC29, UNICODE_COMBINING_CLASS_CCC30,
  UNICODE_COMBINING_CLASS_CCC31, UNICODE_COMBINING_CLASS_CCC32, UNICODE_COMBINING_CLASS_CCC33,
  UNICODE_COMBINING_CLASS_CCC34, UNICODE_COMBINING_CLASS_CCC35, UNICODE_COMBINING_CLASS_CCC36,
  UNICODE_COMBINING_CLASS_CCC84, UNICODE_COMBINING_CLASS_CCC91, UNICODE_COMBINING_CLASS_CCC103,
  UNICODE_COMBINING_CLASS_CCC107, UNICODE_COMBINING_CLASS_CCC118, UNICODE_COMBINING_CLASS_CCC122,
  UNICODE_COMBINING_CLASS_CCC129, UNICODE_COMBINING_CLASS_CCC130, UNICODE_COMBINING_CLASS_CCC133,
  UNICODE_COMBINING_CLASS_ATTACHED_BELOW_LEFT, UNICODE_COMBINING_CLASS_ATTACHED_BELOW,
  UNICODE_COMBINING_CLASS_ATTACHED_ABOVE, UNICODE_COMBINING_CLASS_ATTACHED_ABOVE_RIGHT,
  UNICODE_COMBINING_CLASS_BELOW_LEFT, UNICODE_COMBINING_CLASS_BELOW,
  UNICODE_COMBINING_CLASS_BELOW_RIGHT, UNICODE_COMBINING_CLASS_LEFT, UNICODE_COMBINING_CLASS_RIGHT,
  UNICODE_COMBINING_CLASS_ABOVE_LEFT, UNICODE_COMBINING_CLASS_ABOVE,
  UNICODE_COMBINING_CLASS_ABOVE_RIGHT, UNICODE_COMBINING_CLASS_DOUBLE_BELOW,
  UNICODE_COMBINING_CLASS_DOUBLE_ABOVE, UNICODE_COMBINING_CLASS_IOTA_SUBSCRIPT,
  UNICODE_COMBINING_CLASS_INVALID
 #-}

pattern GLYPH_FLAG_UNSAFE_TO_BREAK = (#const HB_GLYPH_FLAG_UNSAFE_TO_BREAK) :: GlyphFlags
pattern GLYPH_FLAG_DEFINED = (#const HB_GLYPH_FLAG_DEFINED) :: GlyphFlags

pattern BUFFER_REPLACEMENT_CODEPOINT_DEFAULT = (#const HB_BUFFER_REPLACEMENT_CODEPOINT_DEFAULT) :: Int

pattern SET_VALUE_INVALID = (#const HB_SET_VALUE_INVALID) :: Codepoint
pattern MAP_VALUE_INVALID = (#const HB_MAP_VALUE_INVALID) :: Codepoint

pattern VERSION_MAJOR = (#const HB_VERSION_MAJOR) :: Int
pattern VERSION_MINOR = (#const HB_VERSION_MINOR) :: Int
pattern VERSION_MICRO = (#const HB_VERSION_MICRO) :: Int

pattern BUFFER_SERIALIZE_FLAG_DEFAULT = (#const HB_BUFFER_SERIALIZE_FLAG_DEFAULT) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_NO_CLUSTERS = (#const HB_BUFFER_SERIALIZE_FLAG_NO_CLUSTERS) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_NO_POSITIONS = (#const HB_BUFFER_SERIALIZE_FLAG_NO_POSITIONS) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_NO_GLYPH_NAMES = (#const HB_BUFFER_SERIALIZE_FLAG_NO_GLYPH_NAMES) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_GLYPH_EXTENTS = (#const HB_BUFFER_SERIALIZE_FLAG_GLYPH_EXTENTS) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_GLYPH_FLAGS = (#const HB_BUFFER_SERIALIZE_FLAG_GLYPH_FLAGS) :: BufferSerializeFlags
pattern BUFFER_SERIALIZE_FLAG_NO_ADVANCES = (#const HB_BUFFER_SERIALIZE_FLAG_NO_ADVANCES) :: BufferSerializeFlags

pattern BUFFER_DIFF_FLAG_EQUAL = (#const HB_BUFFER_DIFF_FLAG_EQUAL) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_CONTENT_TYPE_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_CONTENT_TYPE_MISMATCH) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_LENGTH_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_LENGTH_MISMATCH) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_NOTDEF_PRESENT = (#const HB_BUFFER_DIFF_FLAG_NOTDEF_PRESENT) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_DOTTED_CIRCLE_PRESENT = (#const HB_BUFFER_DIFF_FLAG_DOTTED_CIRCLE_PRESENT) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_CODEPOINT_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_CODEPOINT_MISMATCH) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_CLUSTER_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_CLUSTER_MISMATCH) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_GLYPH_FLAGS_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_GLYPH_FLAGS_MISMATCH) :: BufferDiffFlags
pattern BUFFER_DIFF_FLAG_POSITION_MISMATCH = (#const HB_BUFFER_DIFF_FLAG_POSITION_MISMATCH) :: BufferDiffFlags

pattern SHAPER_INVALID = Shaper NULL :: Shaper

#endif

-- * Finalization

foreignBlob :: Ptr Blob -> IO Blob
foreignBlob = fmap Blob . newForeignPtr _hb_blob_destroy

foreignBuffer :: Ptr Buffer -> IO Buffer
foreignBuffer = fmap Buffer . newForeignPtr _hb_buffer_destroy

foreignFace :: Ptr Face -> IO Face
foreignFace = fmap Face . newForeignPtr _hb_face_destroy

foreignFont :: Ptr Font -> IO Font
foreignFont = fmap Font . newForeignPtr _hb_font_destroy

foreignFontFuncs :: Ptr FontFuncs -> IO FontFuncs
foreignFontFuncs = fmap FontFuncs . newForeignPtr _hb_font_funcs_destroy

foreignMap :: Ptr Map -> IO Map
foreignMap = fmap Map . newForeignPtr _hb_map_destroy

foreignSet :: Ptr Set -> IO Set
foreignSet = fmap Set . newForeignPtr _hb_set_destroy

foreignShapePlan :: Ptr ShapePlan -> IO ShapePlan
foreignShapePlan = fmap ShapePlan . newForeignPtr _hb_shape_plan_destroy

foreignUnicodeFuncs :: Ptr UnicodeFuncs -> IO UnicodeFuncs
foreignUnicodeFuncs = fmap UnicodeFuncs . newForeignPtr _hb_unicode_funcs_destroy

foreign import ccall "hb.h &hb_blob_destroy"          _hb_blob_destroy          :: FinalizerPtr Blob
foreign import ccall "hb.h &hb_buffer_destroy"        _hb_buffer_destroy        :: FinalizerPtr Buffer
foreign import ccall "hb.h &hb_face_destroy"          _hb_face_destroy          :: FinalizerPtr Face
foreign import ccall "hb.h &hb_font_destroy"          _hb_font_destroy          :: FinalizerPtr Font
foreign import ccall "hb.h &hb_font_funcs_destroy"    _hb_font_funcs_destroy    :: FinalizerPtr FontFuncs
foreign import ccall "hb.h &hb_map_destroy"           _hb_map_destroy           :: FinalizerPtr Map
foreign import ccall "hb.h &hb_set_destroy"           _hb_set_destroy           :: FinalizerPtr Set
foreign import ccall "hb.h &hb_shape_plan_destroy"    _hb_shape_plan_destroy    :: FinalizerPtr ShapePlan
foreign import ccall "hb.h &hb_unicode_funcs_destroy" _hb_unicode_funcs_destroy :: FinalizerPtr UnicodeFuncs
foreign import ccall "&"                               hs_free_stable_ptr       :: FinalizerPtr ()

-- * Inline C context

getHsVariable :: String -> C.HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ C.unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ C.unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

anti :: C.Type C.CIdentifier -> TH.TypeQ -> TH.ExpQ -> C.SomeAntiQuoter
anti cTy hsTyQ w = C.SomeAntiQuoter C.AntiQuoter
  { C.aqParser = do
    hId <- C.parseIdentifier
    let cId = C.mangleHaskellIdentifier hId
    return (cId, cTy, hId)
  , C.aqMarshaller = \_purity _cTypes _cTy cId -> do
    hsTy <- hsTyQ
    hsExp <- getHsVariable "fontConfigCtx" cId
    hsExp' <- [|$w (coerce $(pure hsExp))|]
    return (hsTy, hsExp')
  }

withKey :: Key a -> (Ptr OpaqueKey -> IO r) -> IO r
withKey (Key k) = withForeignPtr k

harfbuzzCtx :: C.Context
harfbuzzCtx = mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_blob_t", [t|Blob|])
    , (C.TypeName "hb_buffer_t", [t|Buffer|])
    , (C.TypeName "hb_buffer_cluster_level_t", [t|BufferClusterLevel|])
    , (C.TypeName "hb_buffer_content_type_t", [t|BufferContentType|])
    , (C.TypeName "hb_buffer_diff_flags_t", [t|BufferDiffFlags|])
    , (C.TypeName "hb_buffer_flags_t", [t|BufferFlags|])
    , (C.TypeName "hb_buffer_message_func_t", [t|FunPtr (BufferMessageFunc ())|])
    , (C.TypeName "hb_buffer_serialize_flags_t", [t|BufferSerializeFlags|])
    , (C.TypeName "hb_buffer_serialize_format_t", [t|BufferSerializeFormat|])
    , (C.TypeName "hb_bool_t", [t|CInt|])
    , (C.TypeName "hb_codepoint_t", [t|Codepoint|])
    , (C.TypeName "hb_destroy_func_t", [t|FinalizerPtr ()|])
    , (C.TypeName "hb_direction_t", [t|Direction|])
    , (C.TypeName "hb_face_t", [t|Face|])
    , (C.TypeName "hb_feature_t", [t|Feature|])
    , (C.TypeName "hb_font_t", [t|Font|])
    , (C.TypeName "hb_font_extents_t", [t|FontExtents|])
    , (C.TypeName "hb_font_funcs_t", [t|FontFuncs|])
    , (C.TypeName "hb_glyph_extents_t", [t|GlyphExtents|])
    , (C.TypeName "hb_glyph_flags_t", [t|GlyphFlags|])
    , (C.TypeName "hb_glyph_info_t", [t|GlyphInfo|])
    , (C.TypeName "hb_glyph_position_t", [t|GlyphPosition|])
    , (C.TypeName "hb_language_t", [t|Ptr Language|])
    , (C.TypeName "hb_language_impl_t", [t|Language|])
    , (C.TypeName "hb_map_t", [t|Map|])
    , (C.TypeName "hb_memory_mode_t", [t|MemoryMode|])
    , (C.TypeName "hb_position_t", [t|Position|])
    , (C.TypeName "hb_reference_table_func_t", [t|FunPtr (ReferenceTableFunc ())|])
    , (C.TypeName "hb_script_t", [t|Script|])
    , (C.TypeName "hb_segment_properties_t", [t|SegmentProperties|])
    , (C.TypeName "hb_set_t", [t|Set|])
    , (C.TypeName "hb_shape_plan_t", [t|ShapePlan|])
    , (C.TypeName "hb_tag_t", [t|Tag|])
    , (C.TypeName "hb_unicode_combining_class_t", [t|UnicodeCombiningClass|])
    , (C.TypeName "hb_unicode_combining_class_func_t", [t|FunPtr (UnicodeCombiningClassFunc ())|])
    , (C.TypeName "hb_unicode_compose_func_t", [t|FunPtr (UnicodeComposeFunc ())|])
    , (C.TypeName "hb_unicode_decompose_func_t", [t|FunPtr (UnicodeDecomposeFunc ())|])
    , (C.TypeName "hb_unicode_funcs_t", [t|UnicodeFuncs|])
    , (C.TypeName "hb_unicode_general_category_t", [t|UnicodeGeneralCategory|])
    , (C.TypeName "hb_unicode_general_category_func_t", [t|FunPtr (UnicodeGeneralCategoryFunc ())|])
    , (C.TypeName "hb_unicode_mirroring_func_t", [t|FunPtr (UnicodeMirroringFunc ())|])
    , (C.TypeName "hb_unicode_script_func_t", [t|FunPtr (UnicodeScriptFunc ())|])
    , (C.TypeName "hb_user_data_key_t", [t|OpaqueKey|])
    , (C.TypeName "hb_variation_t", [t|Variation|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ustr", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char (Just C.Unsigned))) [t|Ptr CUChar|] [|withCUString|])
    , ("str", anti (C.Ptr [C.CONST] $ C.TypeSpecifier mempty (C.Char Nothing)) [t|Ptr CChar|] [|withCString|])
    , ("blob", anti (ptr $ C.TypeName "hb_blob_t") [t|Ptr Blob|] [|withSelf|])
    , ("buffer", anti (ptr $ C.TypeName "hb_buffer_t") [t|Ptr Buffer|] [|withSelf|])
    , ("face", anti (ptr $ C.TypeName "hb_face_t") [t|Ptr Face|] [|withSelf|])
    , ("feature", anti (ptr $ C.TypeName "hb_feature_t") [t|Ptr Feature|] [|with|])
    , ("font", anti (ptr $ C.TypeName "hb_font_t") [t|Ptr Font|] [|withSelf|])
    , ("font-extents", anti (ptr $ C.TypeName "hb_font_extents_t") [t|Ptr FontExtents|] [|with|])
    , ("font-funcs", anti (ptr $ C.TypeName "hb_font_funcs_t") [t|Ptr FontFuncs|] [|withSelf|])
    , ("glyph-extents", anti (ptr $ C.TypeName "hb_glyph_extents_t") [t|Ptr GlyphExtents|] [|with|])
    , ("glyph-info", anti (ptr $ C.TypeName "hb_glyph_info_t") [t|Ptr GlyphInfo|] [|withPtr|])
    , ("key", anti (ptr $ C.TypeName "hb_user_data_key_t") [t|Ptr OpaqueKey|] [|withKey|])
    , ("language", anti (C.TypeSpecifier mempty $ C.TypeName "hb_language_t") [t|Ptr Language|] [|withPtr|])
    , ("map", anti (ptr $ C.TypeName "hb_map_t") [t|Ptr Map|] [|withSelf|])
    , ("segment-properties", anti (ptr $ C.TypeName "hb_segment_properties_t") [t|Ptr SegmentProperties|] [|with|])
    , ("set", anti (ptr $ C.TypeName "hb_set_t") [t|Ptr Set|] [|withSelf|])
    , ("shape-plan", anti (ptr $ C.TypeName "hb_shape_plan_t") [t|Ptr ShapePlan|] [|withSelf|])
    , ("maybe-tags", anti (ptr $ C.TypeName "hb_tag_t") [t|Ptr Tag|] [|maybeWith (withArray0 TAG_NONE) |])
    , ("unicode-funcs", anti (ptr $ C.TypeName "hb_unicode_funcs_t") [t|Ptr UnicodeFuncs|] [|withSelf|])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
