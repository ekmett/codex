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
, ColorLayer(..)
, ColorPaletteFlags
  ( ColorPaletteFlags
  , COLOR_PALETTE_FLAG_DEFAULT
  , COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND
  , COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND
  )
, LayoutGlyphClass
  ( LayoutGlyphClass
  , LAYOUT_GLYPH_CLASS_UNCLASSIFIED
  , LAYOUT_GLYPH_CLASS_BASE_GLYPH
  , LAYOUT_GLYPH_CLASS_LIGATURE
  , LAYOUT_GLYPH_CLASS_MARK
  , LAYOUT_GLYPH_CLASS_COMPONENT
  )
, pattern LAYOUT_DEFAULT_LANGUAGE_INDEX
, pattern LAYOUT_NO_FEATURE_INDEX
, pattern LAYOUT_NO_SCRIPT_INDEX
, pattern LAYOUT_NO_VARIATIONS_INDEX
, MathConstant
  ( MathConstant
  , MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN
  , MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN
  , MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT
  , MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT
  , MATH_CONSTANT_MATH_LEADING
  , MATH_CONSTANT_AXIS_HEIGHT
  , MATH_CONSTANT_ACCENT_BASE_HEIGHT
  , MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT
  , MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN
  , MATH_CONSTANT_SUBSCRIPT_TOP_MAX
  , MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN
  , MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP
  , MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED
  , MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN
  , MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX
  , MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN
  , MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT
  , MATH_CONSTANT_SPACE_AFTER_SCRIPT
  , MATH_CONSTANT_UPPER_LIMIT_GAP_MIN
  , MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN
  , MATH_CONSTANT_LOWER_LIMIT_GAP_MIN
  , MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN
  , MATH_CONSTANT_STACK_TOP_SHIFT_UP
  , MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP
  , MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN
  , MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN
  , MATH_CONSTANT_STACK_GAP_MIN
  , MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN
  , MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP
  , MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN
  , MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN
  , MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN
  , MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP
  , MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP
  , MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN
  , MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN
  , MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN
  , MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN
  , MATH_CONSTANT_FRACTION_RULE_THICKNESS
  , MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN
  , MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN
  , MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP
  , MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP
  , MATH_CONSTANT_OVERBAR_VERTICAL_GAP
  , MATH_CONSTANT_OVERBAR_RULE_THICKNESS
  , MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER
  , MATH_CONSTANT_UNDERBAR_VERTICAL_GAP
  , MATH_CONSTANT_UNDERBAR_RULE_THICKNESS
  , MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER
  , MATH_CONSTANT_RADICAL_VERTICAL_GAP
  , MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP
  , MATH_CONSTANT_RADICAL_RULE_THICKNESS
  , MATH_CONSTANT_RADICAL_EXTRA_ASCENDER
  , MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE
  , MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE
  , MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT
  )
, MathGlyphPart(..)
, MathGlyphPartFlags
  ( MathGlyphPartFlags
  , MATH_GLYPH_PART_FLAG_EXTENDER
  )
, MathGlyphVariant(..)
, MathKern
  ( MathKern
  , MATH_KERN_TOP_RIGHT
  , MATH_KERN_TOP_LEFT
  , MATH_KERN_BOTTOM_RIGHT
  , MATH_KERN_BOTTOM_LEFT
  )
, pattern MAX_TAGS_PER_LANGUAGE
, pattern MAX_TAGS_PER_SCRIPT
, Name
  ( Name
  , NAME_ID_COPYRIGHT
  , NAME_ID_FONT_FAMILY
  , NAME_ID_FONT_SUBFAMILY
  , NAME_ID_UNIQUE_ID
  , NAME_ID_FULL_NAME
  , NAME_ID_POSTSCRIPT_NAME
  , NAME_ID_TRADEMARK
  , NAME_ID_MANUFACTURER
  , NAME_ID_DESIGNER
  , NAME_ID_DESCRIPTION
  , NAME_ID_VENDOR_URL
  , NAME_ID_DESIGNER_URL
  , NAME_ID_LICENSE
  , NAME_ID_LICENSE_URL
  , NAME_ID_TYPOGRAPHIC_FAMILY
  , NAME_ID_TYPOGRAPHIC_SUBFAMILY
  , NAME_ID_MAC_FULL_NAME
  , NAME_ID_SAMPLE_TEXT
  , NAME_ID_CID_FINDFONT_NAME
  , NAME_ID_WWS_FAMILY
  , NAME_ID_WWS_SUBFAMILY
  , NAME_ID_LIGHT_BACKGROUND
  , NAME_ID_DARK_BACKGROUND
  , NAME_ID_VARIATIONS_PS_PREFIX
  , NAME_ID_INVALID
  )
, NameEntry
, VarAxisFlags
  ( VarAxisFlags
  , VAR_AXIS_FLAG_HIDDEN
  , VAR_AXIS_FLAG__MAX_VALUE
  )
, VarAxisInfo(..)
-- We don't want to export these from Graphics.Harfbuzz.Common
-- but if we put them in the Tag and hide them, and then try
-- to re-export without the Tag data type in Graphics.Harfbuzz.
-- haddock will crash. So I break style and put them as standalone patterns.
, pattern MATH_SCRIPT
, pattern TAG_BASE
, pattern TAG_GDEF
, pattern TAG_GPOS
, pattern TAG_GSUB
, pattern TAG_JSTF
, pattern TAG_DEFAULT_LANGUAGE
, pattern TAG_DEFAULT_SCRIPT
, pattern TAG_MATH
, pattern TAG_VAR_AXIS_ITALIC
, pattern TAG_VAR_AXIS_OPTICAL_SIZE
, pattern TAG_VAR_AXIS_SLANT
, pattern TAG_VAR_AXIS_WIDTH
, pattern TAG_VAR_AXIS_WEIGHT
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

data ColorLayer = ColorLayer
  { color_layer_glyph :: {-# unpack #-} !Codepoint
  , color_layer_color_index :: {-# unpack #-} !Int -- color index
  } deriving (Eq,Ord,Show)

instance Storable ColorLayer where
  sizeOf _ = #size hb_ot_color_layer_t
  alignment _ = #alignment hb_ot_color_layer_t
  peek p = ColorLayer
   <$> (#peek hb_ot_color_layer_t, glyph) p
   <*> (#peek hb_ot_color_layer_t, color_index) p
  poke p ColorLayer{..} = do
   (#poke hb_ot_color_layer_t, glyph) p color_layer_glyph
   (#poke hb_ot_color_layer_t, color_index) p color_layer_color_index

newtype ColorPaletteFlags = ColorPaletteFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

pattern LAYOUT_DEFAULT_LANGUAGE_INDEX = #const HB_OT_LAYOUT_DEFAULT_LANGUAGE_INDEX
pattern LAYOUT_NO_FEATURE_INDEX = #const HB_OT_LAYOUT_NO_FEATURE_INDEX
pattern LAYOUT_NO_SCRIPT_INDEX = #const HB_OT_LAYOUT_NO_SCRIPT_INDEX
pattern LAYOUT_NO_VARIATIONS_INDEX = #const HB_OT_LAYOUT_NO_VARIATIONS_INDEX

newtype LayoutGlyphClass = LayoutGlyphClass CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

newtype MathConstant = MathConstant CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

data MathGlyphPart = MathGlyphPart
  { math_glyph_part_glyph :: {-# unpack #-} !Codepoint
  , math_glyph_part_start_connector_length :: {-# unpack #-} !Position
  , math_glyph_part_end_connector_length :: {-# unpack #-} !Position
  , math_glyph_part_full_advance :: {-# unpack #-} !Position
  , math_glyph_part_flags :: {-# unpack #-} !MathGlyphPartFlags
  } deriving (Eq,Ord,Show)

instance Storable MathGlyphPart where
  sizeOf _ = #size hb_ot_math_glyph_part_t
  alignment _ = #alignment hb_ot_math_glyph_part_t
  peek p = MathGlyphPart
   <$> (#peek hb_ot_math_glyph_part_t, glyph) p
   <*> (#peek hb_ot_math_glyph_part_t, start_connector_length) p
   <*> (#peek hb_ot_math_glyph_part_t, end_connector_length) p
   <*> (#peek hb_ot_math_glyph_part_t, full_advance) p
   <*> (#peek hb_ot_math_glyph_part_t, flags) p
  poke p MathGlyphPart{..} = do
   (#poke hb_ot_math_glyph_part_t, glyph) p math_glyph_part_glyph
   (#poke hb_ot_math_glyph_part_t, start_connector_length) p math_glyph_part_start_connector_length
   (#poke hb_ot_math_glyph_part_t, end_connector_length) p math_glyph_part_end_connector_length
   (#poke hb_ot_math_glyph_part_t, full_advance) p math_glyph_part_full_advance
   (#poke hb_ot_math_glyph_part_t, flags) p math_glyph_part_flags

newtype MathGlyphPartFlags = MathGlyphPartFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

data MathGlyphVariant = MathGlyphVariant
  { math_glyph_variant_glyph :: {-# unpack #-} !Codepoint
  , math_glyph_variant_advance :: {-# unpack #-} !Position
  } deriving (Eq,Ord)

instance Storable MathGlyphVariant where
  sizeOf _ = #size hb_ot_math_glyph_variant_t
  alignment _ = #alignment hb_ot_math_glyph_variant_t
  peek p = MathGlyphVariant
   <$> (#peek hb_ot_math_glyph_variant_t, glyph) p
   <*> (#peek hb_ot_math_glyph_variant_t, advance) p
  poke p MathGlyphVariant{..} = do
   (#poke hb_ot_math_glyph_variant_t, glyph) p math_glyph_variant_glyph
   (#poke hb_ot_math_glyph_variant_t, advance) p math_glyph_variant_advance

newtype MathKern = MathKern CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

-- | An  'name' table identifier. There are predefined names, as well as name IDs returned
-- from other APIs. These can be used to fetch name strings from a font face.
newtype Name = Name Word32 deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim)

data NameEntry = NameEntry
  { name_entry_name_id  :: {-# unpack #-} !Name
  , name_entry_var      :: {-# unpack #-} !Word32 -- private
  , name_entry_language :: {-# unpack #-} !Language
  } deriving (Eq,Ord)

instance Storable NameEntry where
  sizeOf _ = #size hb_ot_name_entry_t
  alignment _ = #alignment hb_ot_name_entry_t
  peek p = NameEntry
   <$> (#peek hb_ot_name_entry_t, name_id) p
   <*> (#peek hb_ot_name_entry_t, var ) p
   <*> (#peek hb_ot_name_entry_t, language) p
  poke p NameEntry{..} = do
   (#poke hb_ot_name_entry_t, name_id) p name_entry_name_id
   (#poke hb_ot_name_entry_t, var ) p name_entry_var
   (#poke hb_ot_name_entry_t, language) p name_entry_language

newtype VarAxisFlags = VarAxisFlags CInt deriving (Eq,Ord,Show,Read,Num,Enum,Real,Integral,Storable,Prim,Bits)

data VarAxisInfo = VarAxisInfo
  { var_axis_info_axis_index    :: {-# unpack #-} !Word32
  , var_axis_info_tag           :: {-# unpack #-} !Tag
  , var_axis_info_name_id       :: {-# unpack #-} !Name
  , var_axis_info_flags         :: {-# unpack #-} !VarAxisFlags
  , var_axis_info_min_value     :: {-# unpack #-} !Float
  , var_axis_info_default_value :: {-# unpack #-} !Float
  , var_axis_info_max_value     :: {-# unpack #-} !Float
  , var_axis_info_reserved      :: {-# unpack #-} !Word32
  } deriving (Eq,Ord,Show,Read)

instance Storable VarAxisInfo where
  sizeOf _ = #size hb_ot_var_axis_info_t
  alignment _ = #alignment hb_ot_var_axis_info_t
  peek p = VarAxisInfo
   <$> (#peek hb_ot_var_axis_info_t, axis_index) p
   <*> (#peek hb_ot_var_axis_info_t, tag) p
   <*> (#peek hb_ot_var_axis_info_t, name_id) p
   <*> (#peek hb_ot_var_axis_info_t, flags) p
   <*> (#peek hb_ot_var_axis_info_t, min_value) p
   <*> (#peek hb_ot_var_axis_info_t, default_value) p
   <*> (#peek hb_ot_var_axis_info_t, max_value) p
   <*> (#peek hb_ot_var_axis_info_t, reserved) p
  poke p VarAxisInfo{..} = do
   (#poke hb_ot_var_axis_info_t, axis_index) p var_axis_info_axis_index
   (#poke hb_ot_var_axis_info_t, tag) p var_axis_info_tag
   (#poke hb_ot_var_axis_info_t, name_id) p var_axis_info_name_id
   (#poke hb_ot_var_axis_info_t, flags) p var_axis_info_flags
   (#poke hb_ot_var_axis_info_t, min_value) p var_axis_info_min_value
   (#poke hb_ot_var_axis_info_t, default_value) p var_axis_info_default_value
   (#poke hb_ot_var_axis_info_t, max_value) p var_axis_info_max_value
   (#poke hb_ot_var_axis_info_t, reserved) p var_axis_info_reserved

deriving instance Show NameEntry

#ifndef HLINT
pattern TAG_BASE = (#const HB_OT_TAG_BASE) :: Tag -- "BASE"
pattern TAG_GDEF = (#const HB_OT_TAG_GDEF) :: Tag -- "GDEF"
pattern TAG_GPOS = (#const HB_OT_TAG_GPOS) :: Tag -- "GPOS"
pattern TAG_GSUB = (#const HB_OT_TAG_GSUB) :: Tag -- "GSUB"
pattern TAG_JSTF = (#const HB_OT_TAG_JSTF) :: Tag -- "JSTF"
pattern TAG_DEFAULT_LANGUAGE = (#const HB_OT_TAG_DEFAULT_LANGUAGE) :: Tag -- "dflt"
pattern TAG_DEFAULT_SCRIPT = (#const HB_OT_TAG_DEFAULT_SCRIPT) :: Tag -- "DFLT"

pattern TAG_MATH = (#const HB_OT_TAG_MATH) :: Tag -- "MATH"
pattern MATH_SCRIPT = (#const HB_OT_MATH_SCRIPT) :: Tag -- "math"

pattern TAG_VAR_AXIS_ITALIC = (#const HB_OT_TAG_VAR_AXIS_ITALIC) :: Tag
pattern TAG_VAR_AXIS_OPTICAL_SIZE = (#const HB_OT_TAG_VAR_AXIS_OPTICAL_SIZE) :: Tag
pattern TAG_VAR_AXIS_SLANT = (#const HB_OT_TAG_VAR_AXIS_SLANT) :: Tag
pattern TAG_VAR_AXIS_WIDTH = (#const HB_OT_TAG_VAR_AXIS_WIDTH) :: Tag
pattern TAG_VAR_AXIS_WEIGHT = (#const HB_OT_TAG_VAR_AXIS_WEIGHT) :: Tag

pattern COLOR_PALETTE_FLAG_DEFAULT = (#const HB_OT_COLOR_PALETTE_FLAG_DEFAULT) :: ColorPaletteFlags
pattern COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND = (#const HB_OT_COLOR_PALETTE_FLAG_USABLE_WITH_LIGHT_BACKGROUND) :: ColorPaletteFlags
pattern COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND = (#const HB_OT_COLOR_PALETTE_FLAG_USABLE_WITH_DARK_BACKGROUND) :: ColorPaletteFlags

pattern LAYOUT_GLYPH_CLASS_UNCLASSIFIED = (#const HB_OT_LAYOUT_GLYPH_CLASS_UNCLASSIFIED) :: LayoutGlyphClass
pattern LAYOUT_GLYPH_CLASS_BASE_GLYPH = (#const HB_OT_LAYOUT_GLYPH_CLASS_BASE_GLYPH) :: LayoutGlyphClass
pattern LAYOUT_GLYPH_CLASS_LIGATURE = (#const HB_OT_LAYOUT_GLYPH_CLASS_LIGATURE) :: LayoutGlyphClass
pattern LAYOUT_GLYPH_CLASS_MARK = (#const HB_OT_LAYOUT_GLYPH_CLASS_MARK) :: LayoutGlyphClass
pattern LAYOUT_GLYPH_CLASS_COMPONENT = (#const HB_OT_LAYOUT_GLYPH_CLASS_COMPONENT) :: LayoutGlyphClass

pattern MAX_TAGS_PER_LANGUAGE :: (Eq a, Num a) => a
pattern MAX_TAGS_PER_LANGUAGE = #const HB_OT_MAX_TAGS_PER_LANGUAGE

pattern MAX_TAGS_PER_SCRIPT :: (Eq a, Num a) => a
pattern MAX_TAGS_PER_SCRIPT = #const HB_OT_MAX_TAGS_PER_SCRIPT

pattern MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN = (#const HB_OT_MATH_CONSTANT_SCRIPT_PERCENT_SCALE_DOWN) :: MathConstant
pattern MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN = (#const HB_OT_MATH_CONSTANT_SCRIPT_SCRIPT_PERCENT_SCALE_DOWN) :: MathConstant
pattern MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT = (#const HB_OT_MATH_CONSTANT_DELIMITED_SUB_FORMULA_MIN_HEIGHT) :: MathConstant
pattern MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT = (#const HB_OT_MATH_CONSTANT_DISPLAY_OPERATOR_MIN_HEIGHT) :: MathConstant
pattern MATH_CONSTANT_MATH_LEADING = (#const HB_OT_MATH_CONSTANT_MATH_LEADING) :: MathConstant
pattern MATH_CONSTANT_AXIS_HEIGHT = (#const HB_OT_MATH_CONSTANT_AXIS_HEIGHT) :: MathConstant
pattern MATH_CONSTANT_ACCENT_BASE_HEIGHT = (#const HB_OT_MATH_CONSTANT_ACCENT_BASE_HEIGHT) :: MathConstant
pattern MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT = (#const HB_OT_MATH_CONSTANT_FLATTENED_ACCENT_BASE_HEIGHT) :: MathConstant
pattern MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_SUBSCRIPT_TOP_MAX = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_TOP_MAX) :: MathConstant
pattern MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN = (#const HB_OT_MATH_CONSTANT_SUBSCRIPT_BASELINE_DROP_MIN) :: MathConstant
pattern MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_SHIFT_UP_CRAMPED) :: MathConstant
pattern MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MIN) :: MathConstant
pattern MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BASELINE_DROP_MAX) :: MathConstant
pattern MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_SUB_SUPERSCRIPT_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT = (#const HB_OT_MATH_CONSTANT_SUPERSCRIPT_BOTTOM_MAX_WITH_SUBSCRIPT) :: MathConstant
pattern MATH_CONSTANT_SPACE_AFTER_SCRIPT = (#const HB_OT_MATH_CONSTANT_SPACE_AFTER_SCRIPT) :: MathConstant
pattern MATH_CONSTANT_UPPER_LIMIT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_UPPER_LIMIT_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN = (#const HB_OT_MATH_CONSTANT_UPPER_LIMIT_BASELINE_RISE_MIN) :: MathConstant
pattern MATH_CONSTANT_LOWER_LIMIT_GAP_MIN = (#const HB_OT_MATH_CONSTANT_LOWER_LIMIT_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN = (#const HB_OT_MATH_CONSTANT_LOWER_LIMIT_BASELINE_DROP_MIN) :: MathConstant
pattern MATH_CONSTANT_STACK_TOP_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STACK_TOP_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STACK_TOP_DISPLAY_STYLE_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STACK_BOTTOM_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STACK_BOTTOM_DISPLAY_STYLE_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_STACK_GAP_MIN = (#const HB_OT_MATH_CONSTANT_STACK_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_STACK_DISPLAY_STYLE_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_TOP_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_BOTTOM_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_ABOVE_MIN) :: MathConstant
pattern MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN = (#const HB_OT_MATH_CONSTANT_STRETCH_STACK_GAP_BELOW_MIN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_DISPLAY_STYLE_SHIFT_UP) :: MathConstant
pattern MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_DISPLAY_STYLE_SHIFT_DOWN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_NUMERATOR_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_NUM_DISPLAY_STYLE_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_FRACTION_RULE_THICKNESS) :: MathConstant
pattern MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOMINATOR_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN = (#const HB_OT_MATH_CONSTANT_FRACTION_DENOM_DISPLAY_STYLE_GAP_MIN) :: MathConstant
pattern MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP = (#const HB_OT_MATH_CONSTANT_SKEWED_FRACTION_HORIZONTAL_GAP) :: MathConstant
pattern MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_SKEWED_FRACTION_VERTICAL_GAP) :: MathConstant
pattern MATH_CONSTANT_OVERBAR_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_OVERBAR_VERTICAL_GAP) :: MathConstant
pattern MATH_CONSTANT_OVERBAR_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_OVERBAR_RULE_THICKNESS) :: MathConstant
pattern MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER = (#const HB_OT_MATH_CONSTANT_OVERBAR_EXTRA_ASCENDER) :: MathConstant
pattern MATH_CONSTANT_UNDERBAR_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_UNDERBAR_VERTICAL_GAP) :: MathConstant
pattern MATH_CONSTANT_UNDERBAR_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_UNDERBAR_RULE_THICKNESS) :: MathConstant
pattern MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER = (#const HB_OT_MATH_CONSTANT_UNDERBAR_EXTRA_DESCENDER) :: MathConstant
pattern MATH_CONSTANT_RADICAL_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_RADICAL_VERTICAL_GAP) :: MathConstant
pattern MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP = (#const HB_OT_MATH_CONSTANT_RADICAL_DISPLAY_STYLE_VERTICAL_GAP) :: MathConstant
pattern MATH_CONSTANT_RADICAL_RULE_THICKNESS = (#const HB_OT_MATH_CONSTANT_RADICAL_RULE_THICKNESS) :: MathConstant
pattern MATH_CONSTANT_RADICAL_EXTRA_ASCENDER = (#const HB_OT_MATH_CONSTANT_RADICAL_EXTRA_ASCENDER) :: MathConstant
pattern MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE = (#const HB_OT_MATH_CONSTANT_RADICAL_KERN_BEFORE_DEGREE) :: MathConstant
pattern MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE = (#const HB_OT_MATH_CONSTANT_RADICAL_KERN_AFTER_DEGREE) :: MathConstant
pattern MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT = (#const HB_OT_MATH_CONSTANT_RADICAL_DEGREE_BOTTOM_RAISE_PERCENT) :: MathConstant

pattern MATH_GLYPH_PART_FLAG_EXTENDER = (#const HB_MATH_GLYPH_PART_FLAG_EXTENDER) :: MathGlyphPartFlags -- Note: HB_MATH not HB_OT_MATH :(

pattern MATH_KERN_TOP_RIGHT = (#const HB_OT_MATH_KERN_TOP_RIGHT) :: MathKern
pattern MATH_KERN_TOP_LEFT = (#const HB_OT_MATH_KERN_TOP_LEFT) :: MathKern
pattern MATH_KERN_BOTTOM_RIGHT = (#const HB_OT_MATH_KERN_BOTTOM_RIGHT) :: MathKern
pattern MATH_KERN_BOTTOM_LEFT = (#const HB_OT_MATH_KERN_BOTTOM_LEFT) :: MathKern

pattern NAME_ID_COPYRIGHT = (#const HB_OT_NAME_ID_COPYRIGHT) :: Name
pattern NAME_ID_FONT_FAMILY = (#const HB_OT_NAME_ID_FONT_FAMILY) :: Name
pattern NAME_ID_FONT_SUBFAMILY = (#const HB_OT_NAME_ID_FONT_SUBFAMILY) :: Name
pattern NAME_ID_UNIQUE_ID = (#const HB_OT_NAME_ID_UNIQUE_ID) :: Name
pattern NAME_ID_FULL_NAME = (#const HB_OT_NAME_ID_FULL_NAME) :: Name
pattern NAME_ID_POSTSCRIPT_NAME = (#const HB_OT_NAME_ID_POSTSCRIPT_NAME) :: Name
pattern NAME_ID_TRADEMARK = (#const HB_OT_NAME_ID_TRADEMARK) :: Name
pattern NAME_ID_MANUFACTURER = (#const HB_OT_NAME_ID_MANUFACTURER) :: Name
pattern NAME_ID_DESIGNER = (#const HB_OT_NAME_ID_DESIGNER) :: Name
pattern NAME_ID_DESCRIPTION = (#const HB_OT_NAME_ID_DESCRIPTION) :: Name
pattern NAME_ID_VENDOR_URL = (#const HB_OT_NAME_ID_VENDOR_URL) :: Name
pattern NAME_ID_DESIGNER_URL = (#const HB_OT_NAME_ID_DESIGNER_URL) :: Name
pattern NAME_ID_LICENSE = (#const HB_OT_NAME_ID_LICENSE) :: Name
pattern NAME_ID_LICENSE_URL = (#const HB_OT_NAME_ID_LICENSE_URL) :: Name
pattern NAME_ID_TYPOGRAPHIC_FAMILY = (#const HB_OT_NAME_ID_TYPOGRAPHIC_FAMILY) :: Name
pattern NAME_ID_TYPOGRAPHIC_SUBFAMILY = (#const HB_OT_NAME_ID_TYPOGRAPHIC_SUBFAMILY) :: Name
pattern NAME_ID_MAC_FULL_NAME = (#const HB_OT_NAME_ID_MAC_FULL_NAME) :: Name
pattern NAME_ID_SAMPLE_TEXT = (#const HB_OT_NAME_ID_SAMPLE_TEXT) :: Name
pattern NAME_ID_CID_FINDFONT_NAME = (#const HB_OT_NAME_ID_CID_FINDFONT_NAME) :: Name
pattern NAME_ID_WWS_FAMILY = (#const HB_OT_NAME_ID_WWS_FAMILY) :: Name
pattern NAME_ID_WWS_SUBFAMILY = (#const HB_OT_NAME_ID_WWS_SUBFAMILY) :: Name
pattern NAME_ID_LIGHT_BACKGROUND = (#const HB_OT_NAME_ID_LIGHT_BACKGROUND) :: Name
pattern NAME_ID_DARK_BACKGROUND = (#const HB_OT_NAME_ID_DARK_BACKGROUND) :: Name
pattern NAME_ID_VARIATIONS_PS_PREFIX = (#const HB_OT_NAME_ID_VARIATIONS_PS_PREFIX) :: Name
pattern NAME_ID_INVALID = (#const HB_OT_NAME_ID_INVALID) :: Name

pattern VAR_AXIS_FLAG_HIDDEN = (#const HB_OT_VAR_AXIS_FLAG_HIDDEN) :: VarAxisFlags
pattern VAR_AXIS_FLAG__MAX_VALUE = (#const _HB_OT_VAR_AXIS_FLAG_MAX_VALUE) :: VarAxisFlags
#endif

harfbuzzOpenTypeCtx :: C.Context
harfbuzzOpenTypeCtx = harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "hb_ot_layout_glyph_class_t", [t|LayoutGlyphClass|])
    , (C.TypeName "hb_color_t", [t|Color|])
    , (C.TypeName "hb_ot_color_layer_t", [t|ColorLayer|])
    , (C.TypeName "hb_ot_color_palette_flags_t", [t|ColorPaletteFlags|])
    , (C.TypeName "hb_ot_math_kern_t", [t|MathKern|])
    , (C.TypeName "hb_ot_math_constant_t", [t|MathConstant|])
    , (C.TypeName "hb_ot_math_glyph_variant_t", [t|MathGlyphVariant|])
    , (C.TypeName "hb_ot_math_glyph_part_t", [t|MathGlyphPart|])
    , (C.TypeName "hb_ot_math_glyph_part_flags_t", [t|MathGlyphPartFlags|])
    , (C.TypeName "hb_ot_name_id_t", [t|Name|])
    , (C.TypeName "hb_ot_name_entry_t", [t|NameEntry|])
    , (C.TypeName "hb_ot_var_axis_flags_t", [t|VarAxisFlags|])
    , (C.TypeName "hb_ot_var_axis_info_t", [t|VarAxisInfo|])
    ]
  }

