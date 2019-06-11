{-# language LambdaCase #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ScopedTypeVariables #-}
{-# language BlockArguments #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.Common
( Codepoint

, Direction(..)
, direction_from_string, direction_to_string
, direction_reverse, direction_is_valid
, direction_is_backward, direction_is_forward
, direction_is_vertical, direction_is_horizontal

, Feature(..)
, feature_to_string, feature_from_string

, Language(..)
, language_from_string, language_to_string
, language_get_default

, Position

, Script(..)
, script_from_string, script_to_string
, script_from_iso15924_tag, script_to_iso15924_tag
, script_get_horizontal_direction

, Tag(Tag,TAG,TAG_NONE,TAG_MAX,TAG_MAX_SIGNED)
, tag_from_string, tag_to_string

, Variation(..)
, variation_from_string, variation_to_string
) where

import Control.Monad.IO.Class
import Data.String
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Unsafe
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<stdlib.h>"
C.include "<hb.h>"
C.include "HsFFI.h"

direction_reverse :: Direction -> Direction
direction_reverse d = [C.pure|hb_direction_t { HB_DIRECTION_REVERSE($(hb_direction_t d)) }|]

direction_is_backward :: Direction -> Bool
direction_is_backward d = cbool [C.pure|int { HB_DIRECTION_IS_BACKWARD($(hb_direction_t d)) }|]

direction_is_forward :: Direction -> Bool
direction_is_forward d = cbool [C.pure|int { HB_DIRECTION_IS_FORWARD($(hb_direction_t d)) }|]

direction_is_horizontal :: Direction -> Bool
direction_is_horizontal d = cbool [C.pure|int { HB_DIRECTION_IS_HORIZONTAL($(hb_direction_t d)) }|]

direction_is_vertical :: Direction -> Bool
direction_is_vertical d = cbool [C.pure|int { HB_DIRECTION_IS_VERTICAL($(hb_direction_t d)) }|]

direction_is_valid :: Direction -> Bool
direction_is_valid d = cbool [C.pure|int { HB_DIRECTION_IS_VALID($(hb_direction_t d)) }|]

-- | The first time this is called it calls setLocale, which isn't thread safe.
-- For multithreaded use, first call once in an isolated fashion
language_get_default :: MonadIO m => m Language
language_get_default = liftIO $
  Language <$> [C.exp|hb_language_t { hb_language_get_default() }|]

script_from_iso15924_tag :: Tag -> Script
script_from_iso15924_tag tag = [C.pure|hb_script_t { hb_script_from_iso15924_tag ($(hb_tag_t tag)) }|]

script_to_iso15924_tag :: Script -> Tag
script_to_iso15924_tag script = [C.pure|hb_tag_t { hb_script_to_iso15924_tag ($(hb_script_t script)) }|]

script_get_horizontal_direction :: Script -> Direction
script_get_horizontal_direction script = [C.pure|hb_direction_t { hb_script_get_horizontal_direction($(hb_script_t script)) }|]

script_from_string :: String -> Script
script_from_string = script_from_iso15924_tag . tag_from_string

script_to_string :: Script -> String
script_to_string = tag_to_string . script_to_iso15924_tag

tag_from_string :: String -> Tag
tag_from_string = fromString

tag_to_string :: Tag -> String
tag_to_string t = unsafeLocalState $ allocaBytes 4 \buf -> do
  [C.exp|void { hb_tag_to_string($(hb_tag_t t),$(char * buf)) }|]
  peekCStringLen (buf,4)
