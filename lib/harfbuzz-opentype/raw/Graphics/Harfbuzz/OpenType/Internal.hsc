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
{-# language PolyKinds #-}
{-# language DataKinds #-}
{-# language UnboxedTuples #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures #-} -- cuts half the length of this file

-- | ffi to the harfbuzz library
--
-- As an internal module, I don't consider this module as supported by the PVP. Be careful.
module Graphics.Harfbuzz.OpenType.Internal
( Color
  ( Color
  , COLOR_BGRA
  , COLOR_RGBA
  )
, OpenTypeColorLayer(..)
, OpenTypeColorPaletteFlags
  ( OpenTypeColorPaletteFlags
  , OT_COLOR_PALETTE_FLAG_DEFAULT
  , OT_COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND
  , OT_COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND
  )
, OpenTypeLayoutGlyphClass
  ( OpenTypeLayoutGlyphClass
  , OT_LAYOUT_GLYPH_CLASS_UNCLASSIFIED
  , OT_LAYOUT_GLYPH_CLASS_BASE_GLYPH
  , OT_LAYOUT_GLYPH_CLASS_LIGATURE
  , OT_LAYOUT_GLYPH_CLASS_MARK
  , OT_LAYOUT_GLYPH_CLASS_COMPONENT
  )
, OpenTypeMathConstant
  ( OpenTypeMathConstant
  , OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN
  , OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN
  , OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT
  , OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT
  , OT_MATH_CONSTANT_MATH_LEADING
  , OT_MATH_CONSTANT_AXIS_HEIGHT
  , OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT
  , OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT
  , OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN
  , OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX
  , OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN
  , OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP
  , OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED
  , OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN
  , OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX
  , OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN
  , OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT
  , OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT
  , OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN
  , OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN
  , OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN
  , OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN
  , OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP
  , OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP
  , OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN
  , OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN
  , OT_MATH_CONSTANT_STACK_GAP_MIN
  , OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN
  , OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP
  , OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN
  , OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN
  , OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN
  , OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP
  , OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP
  , OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN
  , OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN
  , OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN
  , OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN
  , OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS
  , OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN
  , OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN
  , OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP
  , OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP
  , OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP
  , OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS
  , OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER
  , OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP
  , OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS
  , OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER
  , OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP
  , OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP
  , OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS
  , OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER
  , OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE
  , OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE
  , OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT
  )
, OpenTypeMathGlyphPart(..)
, OpenTypeMathGlyphPartFlags
  ( OpenTypeMathGlyphPartFlags
  , OT_MATH_GLYPH_PART_FLAG_EXTENDER
  )
, OpenTypeMathGlyphVariant(..)
, OpenTypeMathKern
  ( OpenTypeMathKern
  , OT_MATH_KERN_TOP_RIGHT
  , OT_MATH_KERN_TOP_LEFT
  , OT_MATH_KERN_BOTTOM_RIGHT
  , OT_MATH_KERN_BOTTOM_LEFT
  )
, OpenTypeName
  ( OpenTypeName
  , OT_NAME_ID_COPYRIGHT
  , OT_NAME_ID_FONT_FAMILY
  , OT_NAME_ID_FONT_SUBFAMILY
  , OT_NAME_ID_UNIQUE_ID
  , OT_NAME_ID_FULL_NAME
  , OT_NAME_ID_POSTSCRIPT_NAME
  , OT_NAME_ID_TRADEMARK
  , OT_NAME_ID_MANUFACTURER
  , OT_NAME_ID_DESIGNER
  , OT_NAME_ID_DESCRIPTION
  , OT_NAME_ID_VENDOR_URL
  , OT_NAME_ID_DESIGNER_URL
  , OT_NAME_ID_LICENSE
  , OT_NAME_ID_LICENSE_URL
  , OT_NAME_ID_TYPOGRAPHIC_FAMILY
  , OT_NAME_ID_TYPOGRAPHIC_SUBFAMILY
  , OT_NAME_ID_MAC_FULL_NAME
  , OT_NAME_ID_SAMPLE_TEXT
  , OT_NAME_ID_CID_FINDFONT_NAME
  , OT_NAME_ID_WWS_FAMILY
  , OT_NAME_ID_WWS_SUBFAMILY
  , OT_NAME_ID_LIGHT_BACKGROUND
  , OT_NAME_ID_DARK_BACKGROUND
  , OT_NAME_ID_VARIATIONS_PS_PREFIX
  , OT_NAME_ID_INVALID
  )
, OpenTypeNameEntry
, OpenTypeVarAxisFlags
  ( OpenTypeVarAxisFlags
  , OT_VAR_AXIS_FLAG_HIDDEN
  , OT_VAR_AXIS_FLAG__MAX_VALUE
  )
, OpenTypeVarAxisInfo(..)
-- We don't want to export these from Graphics.Harfbuzz.Common
-- but if we put them in the Tag and hide them, and then try
-- to re-export without the Tag data type in Graphics.Harfbuzz.OpenType
-- haddock will crash. So I break style and put them as standalone patterns.
, pattern OT_MATH_SCRIPT
, pattern OT_TAG_BASE
, pattern OT_TAG_GDEF
, pattern OT_TAG_GPOS
, pattern OT_TAG_GSUB
, pattern OT_TAG_JSTF
, pattern OT_TAG_DEFAULT_LANGUAGE
, pattern OT_TAG_DEFAULT_SCRIPT
, pattern OT_TAG_MATH
, pattern OT_TAG_VAR_AXIS_ITALIC
, pattern OT_TAG_VAR_AXIS_OPTICAL_SIZE
, pattern OT_TAG_VAR_AXIS_SLANT
, pattern OT_TAG_VAR_AXIS_WIDTH
, pattern OT_TAG_VAR_AXIS_WEIGHT
-- * internals
, harfbuzzOpenTypeCtx
) where

import Data.Bits
import Data.Primitive.Types
import qualified Data.Map as Map
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

import Graphics.Harfbuzz.Internal

#ifndef HLINT
#include <hb.h>
#include <hb-ot.h>
#endif

-- | Internally stored in BGRA order
newtype Color = Color Word32 deriving (Eq,Ord,Enum,Storable,Prim)

unbgra:: Color -> (Word8, Word8, Word8, Word8)
unbgra (Color w) =
  ( fromIntegral (unsafeShiftR w 24 .&. 0xff)
  , fromIntegral (unsafeShiftR w 16 .&. 0xff)
  , fromIntegral (unsafeShiftR w 8 .&. 0xff)
  , fromIntegral (w .&. 0xff)
  )

pattern COLOR_BGRA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
pattern COLOR_BGRA b g r a <- (unbgra -> (b,g,r,a)) where
  COLOR_BGRA b g r a = Color $ 
    unsafeShiftL (fromIntegral b .&. 0xff) 24 .|.
    unsafeShiftL (fromIntegral g .&. 0xff) 16 .|.
    unsafeShiftL (fromIntegral r .&. 0xff) 8  .|.
    (fromIntegral a .&. 0xff)

pattern COLOR_RGBA :: Word8 -> Word8 -> Word8 -> Word8 -> Color
pattern COLOR_RGBA r g b a = COLOR_BGRA b g r a

data OpenTypeColorLayer = OpenTypeColorLayer
  { ot_color_layer_glyph :: {-# unpack #-} !Codepoint
  , ot_color_layer_color_index :: {-# unpack #-} !Int -- color index
  } deriving (Eq,Ord,Show)

instance Storable OpenTypeColorLayer where
  sizeOf _ = #size hb_ot_color_layer_t
  alignment _ = #alignment hb_ot_color_layer_t
  peek p = OpenTypeColorLayer
   <$> (#peek hb_ot_color_layer_t, glyph) p
   <*> (#peek hb_ot_color_layer_t, color_index) p
  poke p OpenTypeColorLayer{..} = do
   (#poke hb_ot_color_layer_t, glyph) p ot_color_layer_glyph
   (#poke hb_ot_color_layer_t, color_index) p ot_color_layer_color_index

newtype OpenTypeColorPaletteFlags = OpenTypeColorPaletteFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

newtype OpenTypeLayoutGlyphClass = OpenTypeLayoutGlyphClass CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

newtype OpenTypeMathConstant = OpenTypeMathConstant CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

data OpenTypeMathGlyphPart = OpenTypeMathGlyphPart
  { ot_math_glyph_part_glyph :: {-# unpack #-} !Codepoint
  , ot_math_glyph_part_start_connector_length :: {-# unpack #-} !Position
  , ot_math_glyph_part_end_connector_length :: {-# unpack #-} !Position
  , ot_math_glyph_part_full_advance :: {-# unpack #-} !Position
  , ot_math_glyph_part_flags :: {-# unpack #-} !OpenTypeMathGlyphPartFlags
  } deriving (Eq,Ord,Show)

instance Storable OpenTypeMathGlyphPart where
  sizeOf _ = #size hb_ot_math_glyph_part_t
  alignment _ = #alignment hb_ot_math_glyph_part_t
  peek p = OpenTypeMathGlyphPart
   <$> (#peek hb_ot_math_glyph_part_t, glyph) p
   <*> (#peek hb_ot_math_glyph_part_t, start_connector_length) p
   <*> (#peek hb_ot_math_glyph_part_t, end_connector_length) p
   <*> (#peek hb_ot_math_glyph_part_t, full_advance) p
   <*> (#peek hb_ot_math_glyph_part_t, flags) p
  poke p OpenTypeMathGlyphPart{..} = do
   (#poke hb_ot_math_glyph_part_t, glyph) p ot_math_glyph_part_glyph
   (#poke hb_ot_math_glyph_part_t, start_connector_length) p ot_math_glyph_part_start_connector_length
   (#poke hb_ot_math_glyph_part_t, end_connector_length) p ot_math_glyph_part_end_connector_length
   (#poke hb_ot_math_glyph_part_t, full_advance) p ot_math_glyph_part_full_advance
   (#poke hb_ot_math_glyph_part_t, flags) p ot_math_glyph_part_flags

newtype OpenTypeMathGlyphPartFlags = OpenTypeMathGlyphPartFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

data OpenTypeMathGlyphVariant = OpenTypeMathGlyphVariant
  { ot_math_glyph_variant_glyph :: {-# unpack #-} !Codepoint
  , ot_math_glyph_variant_advance :: {-# unpack #-} !Position
  } deriving (Eq,Ord)

instance Storable OpenTypeMathGlyphVariant where
  sizeOf _ = #size hb_ot_math_glyph_variant_t
  alignment _ = #alignment hb_ot_math_glyph_variant_t
  peek p = OpenTypeMathGlyphVariant
   <$> (#peek hb_ot_math_glyph_variant_t, glyph) p
   <*> (#peek hb_ot_math_glyph_variant_t, advance) p
  poke p OpenTypeMathGlyphVariant{..} = do
   (#poke hb_ot_math_glyph_variant_t, glyph) p ot_math_glyph_variant_glyph
   (#poke hb_ot_math_glyph_variant_t, advance) p ot_math_glyph_variant_advance

newtype OpenTypeMathKern = OpenTypeMathKern CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

-- | An OpenType 'name' table identifier. There are predefined names, as well as name IDs returned
-- from other APIs. These can be used to fetch name strings from a font face.
newtype OpenTypeName = OpenTypeName Word32 deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

data OpenTypeNameEntry = OpenTypeNameEntry
  { ot_name_entry_name_id  :: {-# unpack #-} !OpenTypeName
  , ot_name_entry_var      :: {-# unpack #-} !Word32 -- private
  , ot_name_entry_language :: {-# unpack #-} !Language
  } deriving (Eq,Ord)

instance Storable OpenTypeNameEntry where
  sizeOf _ = #size hb_ot_name_entry_t
  alignment _ = #alignment hb_ot_name_entry_t
  peek p = OpenTypeNameEntry
   <$> (#peek hb_ot_name_entry_t, name_id) p
   <*> (#peek hb_ot_name_entry_t, var ) p
   <*> (#peek hb_ot_name_entry_t, language) p
  poke p OpenTypeNameEntry{..} = do
   (#poke hb_ot_name_entry_t, name_id) p ot_name_entry_name_id
   (#poke hb_ot_name_entry_t, var ) p ot_name_entry_var
   (#poke hb_ot_name_entry_t, language) p ot_name_entry_language

newtype OpenTypeVarAxisFlags = OpenTypeVarAxisFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

data OpenTypeVarAxisInfo = OpenTypeVarAxisInfo
  { ot_var_axis_info_axis_index    :: {-# unpack #-} !Word32
  , ot_var_axis_info_tag           :: {-# unpack #-} !Tag
  , ot_var_axis_info_name_id       :: {-# unpack #-} !OpenTypeName
  , ot_var_axis_info_flags         :: {-# unpack #-} !OpenTypeVarAxisFlags
  , ot_var_axis_info_min_value     :: {-# unpack #-} !Float
  , ot_var_axis_info_default_value :: {-# unpack #-} !Float
  , ot_var_axis_info_max_value     :: {-# unpack #-} !Float
  , ot_var_axis_info_reserved      :: {-# unpack #-} !Word32
  } deriving (Eq,Ord,Show,Read)

instance Storable OpenTypeVarAxisInfo where
  sizeOf _ = #size hb_ot_var_axis_info_t
  alignment _ = #alignment hb_ot_var_axis_info_t
  peek p = OpenTypeVarAxisInfo
   <$> (#peek hb_ot_var_axis_info_t, axis_index) p
   <*> (#peek hb_ot_var_axis_info_t, tag) p
   <*> (#peek hb_ot_var_axis_info_t, name_id) p
   <*> (#peek hb_ot_var_axis_info_t, flags) p
   <*> (#peek hb_ot_var_axis_info_t, min_value) p
   <*> (#peek hb_ot_var_axis_info_t, default_value) p
   <*> (#peek hb_ot_var_axis_info_t, max_value) p
   <*> (#peek hb_ot_var_axis_info_t, reserved) p
  poke p OpenTypeVarAxisInfo{..} = do
   (#poke hb_ot_var_axis_info_t, axis_index) p ot_var_axis_info_axis_index
   (#poke hb_ot_var_axis_info_t, tag) p ot_var_axis_info_tag
   (#poke hb_ot_var_axis_info_t, name_id) p ot_var_axis_info_name_id
   (#poke hb_ot_var_axis_info_t, flags) p ot_var_axis_info_flags
   (#poke hb_ot_var_axis_info_t, min_value) p ot_var_axis_info_min_value
   (#poke hb_ot_var_axis_info_t, default_value) p ot_var_axis_info_default_value
   (#poke hb_ot_var_axis_info_t, max_value) p ot_var_axis_info_max_value
   (#poke hb_ot_var_axis_info_t, reserved) p ot_var_axis_info_reserved

deriving instance Show OpenTypeNameEntry

#ifndef HLINT
pattern OT_TAG_BASE = (#const HB_OT_TAG_BASE) :: Tag -- "BASE"
pattern OT_TAG_GDEF = (#const HB_OT_TAG_GDEF) :: Tag -- "GDEF"
pattern OT_TAG_GPOS = (#const HB_OT_TAG_GPOS) :: Tag -- "GPOS"
pattern OT_TAG_GSUB = (#const HB_OT_TAG_GSUB) :: Tag -- "GSUB"
pattern OT_TAG_JSTF = (#const HB_OT_TAG_JSTF) :: Tag -- "JSTF"
pattern OT_TAG_DEFAULT_LANGUAGE = (#const HB_OT_TAG_DEFAULT_LANGUAGE) :: Tag -- "dflt"
pattern OT_TAG_DEFAULT_SCRIPT = (#const HB_OT_TAG_DEFAULT_SCRIPT) :: Tag -- "DFLT"

pattern OT_TAG_MATH = (#const HB_OT_TAG_MATH) :: Tag -- "MATH"
pattern OT_MATH_SCRIPT = (#const HB_OT_MATH_SCRIPT) :: Tag -- "math"

pattern OT_TAG_VAR_AXIS_ITALIC = (#const HB_OT_TAG_VAR_AXIS_ITALIC) :: Tag
pattern OT_TAG_VAR_AXIS_OPTICAL_SIZE = (#const HB_OT_TAG_VAR_AXIS_OPTICAL_SIZE) :: Tag
pattern OT_TAG_VAR_AXIS_SLANT = (#const HB_OT_TAG_VAR_AXIS_SLANT) :: Tag
pattern OT_TAG_VAR_AXIS_WIDTH = (#const HB_OT_TAG_VAR_AXIS_WIDTH) :: Tag
pattern OT_TAG_VAR_AXIS_WEIGHT = (#const HB_OT_TAG_VAR_AXIS_WEIGHT) :: Tag

pattern OT_COLOR_PALETTE_FLAG_DEFAULT = (#const HB_OT_COLOR_PALETTE_FLAG_DEFAULT) :: OpenTypeColorPaletteFlags
pattern OT_COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND = (#const HB_OT_COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND) :: OpenTypeColorPaletteFlags
pattern OT_COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND = (#const HB_OT_COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND) :: OpenTypeColorPaletteFlags

pattern OT_LAYOUT_GLYPH_CLASS_UNCLASSIFIED = (#const HB_OT_LAYOUT_GLYPH_CLASS_UNCLASSIFIED) :: OpenTypeLayoutGlyphClass
pattern OT_LAYOUT_GLYPH_CLASS_BASE_GLYPH = (#const HB_OT_LAYOUT_GLYPH_CLASS_BASE_GLYPH) :: OpenTypeLayoutGlyphClass
pattern OT_LAYOUT_GLYPH_CLASS_LIGATURE = (#const HB_OT_LAYOUT_GLYPH_CLASS_LIGATURE) :: OpenTypeLayoutGlyphClass
pattern OT_LAYOUT_GLYPH_CLASS_MARK = (#const HB_OT_LAYOUT_GLYPH_CLASS_MARK) :: OpenTypeLayoutGlyphClass
pattern OT_LAYOUT_GLYPH_CLASS_COMPONENT = (#const HB_OT_LAYOUT_GLYPH_CLASS_COMPONENT) :: OpenTypeLayoutGlyphClass

pattern OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN = (#const HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN = (#const HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT = (#const HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT = (#const HB_OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_MATH_LEADING = (#const HB_OT_MATH_CONSTANT_MATH_LEADING) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_AXIS_HEIGHT = (#const HB_OT_MATH_CONSTANT_AXIS_HEIGHT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT = (#const HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT = (#const HB_OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT = (#const HB_OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN = (#const HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN = (#const HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_GAP_MIN = (#const HB_OT_MATH_CONSTANT_STACK_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP = (#const HB_OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER = (#const HB_OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER = (#const HB_OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER = (#const HB_OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE = (#const HB_OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE = (#const HB_OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE) :: OpenTypeMathConstant
pattern OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT = (#const HB_OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT) :: OpenTypeMathConstant

pattern OT_MATH_GLYPH_PART_FLAG_EXTENDER = (#const HB_MATH_GLYPH_PART_FLAG_EXTENDER) :: OpenTypeMathGlyphPartFlags -- Note: HB_MATH not HB_OT_MATH :(

pattern OT_MATH_KERN_TOP_RIGHT = (#const HB_OT_MATH_KERN_TOP_RIGHT) :: OpenTypeMathKern
pattern OT_MATH_KERN_TOP_LEFT = (#const HB_OT_MATH_KERN_TOP_LEFT) :: OpenTypeMathKern
pattern OT_MATH_KERN_BOTTOM_RIGHT = (#const HB_OT_MATH_KERN_BOTTOM_RIGHT) :: OpenTypeMathKern
pattern OT_MATH_KERN_BOTTOM_LEFT = (#const HB_OT_MATH_KERN_BOTTOM_LEFT) :: OpenTypeMathKern

pattern OT_NAME_ID_COPYRIGHT = (#const HB_OT_NAME_ID_COPYRIGHT) :: OpenTypeName
pattern OT_NAME_ID_FONT_FAMILY = (#const HB_OT_NAME_ID_FONT_FAMILY) :: OpenTypeName
pattern OT_NAME_ID_FONT_SUBFAMILY = (#const HB_OT_NAME_ID_FONT_SUBFAMILY) :: OpenTypeName
pattern OT_NAME_ID_UNIQUE_ID = (#const HB_OT_NAME_ID_UNIQUE_ID) :: OpenTypeName
pattern OT_NAME_ID_FULL_NAME = (#const HB_OT_NAME_ID_FULL_NAME) :: OpenTypeName
pattern OT_NAME_ID_POSTSCRIPT_NAME = (#const HB_OT_NAME_ID_POSTSCRIPT_NAME) :: OpenTypeName
pattern OT_NAME_ID_TRADEMARK = (#const HB_OT_NAME_ID_TRADEMARK) :: OpenTypeName
pattern OT_NAME_ID_MANUFACTURER = (#const HB_OT_NAME_ID_MANUFACTURER) :: OpenTypeName
pattern OT_NAME_ID_DESIGNER = (#const HB_OT_NAME_ID_DESIGNER) :: OpenTypeName
pattern OT_NAME_ID_DESCRIPTION = (#const HB_OT_NAME_ID_DESCRIPTION) :: OpenTypeName
pattern OT_NAME_ID_VENDOR_URL = (#const HB_OT_NAME_ID_VENDOR_URL) :: OpenTypeName
pattern OT_NAME_ID_DESIGNER_URL = (#const HB_OT_NAME_ID_DESIGNER_URL) :: OpenTypeName
pattern OT_NAME_ID_LICENSE = (#const HB_OT_NAME_ID_LICENSE) :: OpenTypeName
pattern OT_NAME_ID_LICENSE_URL = (#const HB_OT_NAME_ID_LICENSE_URL) :: OpenTypeName
pattern OT_NAME_ID_TYPOGRAPHIC_FAMILY = (#const HB_OT_NAME_ID_TYPOGRAPHIC_FAMILY) :: OpenTypeName
pattern OT_NAME_ID_TYPOGRAPHIC_SUBFAMILY = (#const HB_OT_NAME_ID_TYPOGRAPHIC_SUBFAMILY) :: OpenTypeName
pattern OT_NAME_ID_MAC_FULL_NAME = (#const HB_OT_NAME_ID_MAC_FULL_NAME) :: OpenTypeName
pattern OT_NAME_ID_SAMPLE_TEXT = (#const HB_OT_NAME_ID_SAMPLE_TEXT) :: OpenTypeName
pattern OT_NAME_ID_CID_FINDFONT_NAME = (#const HB_OT_NAME_ID_CID_FINDFONT_NAME) :: OpenTypeName
pattern OT_NAME_ID_WWS_FAMILY = (#const HB_OT_NAME_ID_WWS_FAMILY) :: OpenTypeName
pattern OT_NAME_ID_WWS_SUBFAMILY = (#const HB_OT_NAME_ID_WWS_SUBFAMILY) :: OpenTypeName
pattern OT_NAME_ID_LIGHT_BACKGROUND = (#const HB_OT_NAME_ID_LIGHT_BACKGROUND) :: OpenTypeName
pattern OT_NAME_ID_DARK_BACKGROUND = (#const HB_OT_NAME_ID_DARK_BACKGROUND) :: OpenTypeName
pattern OT_NAME_ID_VARIATIONS_PS_PREFIX = (#const HB_OT_NAME_ID_VARIATIONS_PS_PREFIX) :: OpenTypeName
pattern OT_NAME_ID_INVALID = (#const HB_OT_NAME_ID_INVALID) :: OpenTypeName

pattern OT_VAR_AXIS_FLAG_HIDDEN = (#const HB_OT_VAR_AXIS_FLAG_HIDDEN) :: OpenTypeVarAxisFlags
pattern OT_VAR_AXIS_FLAG__MAX_VALUE = (#const _HB_OT_VAR_AXIS_FLAG_MAX_VALUE) :: OpenTypeVarAxisFlags
#endif

harfbuzzOpenTypeCtx :: C.Context
harfbuzzOpenTypeCtx = harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_ot_layout_glyph_class_t", [t|OpenTypeLayoutGlyphClass|])
    , (C.TypeName "hb_color_t", [t|Color|])
    , (C.TypeName "hb_ot_color_layer_t", [t|OpenTypeColorLayer|])
    , (C.TypeName "hb_ot_color_palette_flags_t", [t|OpenTypeColorPaletteFlags|])
    , (C.TypeName "hb_ot_math_kern_t", [t|OpenTypeMathKern|])
    , (C.TypeName "hb_ot_math_constant_t", [t|OpenTypeMathConstant|])
    , (C.TypeName "hb_ot_math_glyph_variant_t", [t|OpenTypeMathGlyphVariant|])
    , (C.TypeName "hb_ot_math_glyph_part_t", [t|OpenTypeMathGlyphPart|])
    , (C.TypeName "hb_ot_math_glyph_part_flags_t", [t|OpenTypeMathGlyphPartFlags|])
    , (C.TypeName "hb_ot_name_id_t", [t|OpenTypeName|])
    , (C.TypeName "hb_ot_name_entry_t", [t|OpenTypeNameEntry|])
    , (C.TypeName "hb_ot_var_axis_flags_t", [t|OpenTypeVarAxisFlags|])
    , (C.TypeName "hb_ot_var_axis_info_t", [t|OpenTypeVarAxisInfo|])
    ]
  , C.ctxAntiQuoters = Map.fromList
    [ ("ot-name-entry", anti (ptr $ C.TypeName "hb_ot_name_entry_t") [t|Ptr OpenTypeNameEntry|] [|with|])
    , ("ot-math-glyph-part", anti (ptr $ C.TypeName "hb_ot_math_glyph_part_t") [t|Ptr OpenTypeMathGlyphPart|] [|with|])
    , ("ot-math-glyph-variant", anti (ptr $ C.TypeName "hb_ot_math_glyph_variant_t") [t|Ptr OpenTypeMathGlyphVariant|] [|with|])
    , ("ot-color-layer", anti (ptr $ C.TypeName "hb_ot_color_layer_t") [t|Ptr OpenTypeColorLayer|] [|with|])
    , ("ot-var-axis-info", anti (ptr $ C.TypeName "hb_ot_var_axis_info_t") [t|Ptr OpenTypeVarAxisInfo|] [|with|])
    ]
  } where ptr = C.Ptr [] . C.TypeSpecifier mempty
