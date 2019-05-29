{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
-- |
module Graphics.Harfbuzz.OpenType.Layout
( tag_to_language
, tag_to_script
, tags_from_script_and_language
, tags_to_script_and_language
, pattern MAX_TAGS_PER_SCRIPT
, pattern MAX_TAGS_PER_LANGUAGE
, LayoutGlyphClass(..)
, pattern TAG_BASE
, pattern TAG_GDEF
, pattern TAG_GPOS
, pattern TAG_GSUB
, pattern TAG_JSTF
, pattern TAG_DEFAULT_LANGUAGE
, pattern TAG_DEFAULT_SCRIPT
, layout_collect_features
, layout_collect_lookups
, layout_feature_get_characters
, layout_feature_get_lookups
-- ...
) where

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- * Languages and Scripts

tag_to_script :: Tag -> Script
tag_to_script tag =[C.pure|hb_script_t { hb_ot_tag_to_script($(hb_tag_t tag)) }|]

tag_to_language :: Tag -> Language
tag_to_language tag = Language [C.pure|hb_language_t { hb_ot_tag_to_language($(hb_tag_t tag)) }|]

tags_from_script_and_language :: Script -> Language -> ([Tag],[Tag])
tags_from_script_and_language script language = unsafeLocalState $
  allocaArray (MAX_TAGS_PER_SCRIPT + MAX_TAGS_PER_LANGUAGE) $ \pscripts ->
    withArray [MAX_TAGS_PER_SCRIPT,MAX_TAGS_PER_LANGUAGE] $ \pscript_count -> do
      let planguages = advancePtr pscripts MAX_TAGS_PER_SCRIPT
          planguage_count = advancePtr pscript_count 1
      [C.block|void { hb_ot_tags_from_script_and_language( $(hb_script_t script), $language:language, $(unsigned int * pscript_count), $(hb_tag_t * pscripts), $(unsigned int * planguage_count), $(hb_tag_t * planguages)); }|]
      nscripts <- fromIntegral <$> peek pscript_count
      nlanguages <- fromIntegral <$> peek planguage_count
      (,) <$> peekArray nscripts pscripts <*> peekArray nlanguages planguages

tags_to_script_and_language :: Tag -> Tag -> (Script,Language)
tags_to_script_and_language script_tag language_tag = unsafeLocalState $
  alloca $ \pscript -> alloca $ \ planguage -> do
    [C.block|void { hb_ot_tags_to_script_and_language( $(hb_tag_t script_tag),$(hb_tag_t language_tag),$(hb_script_t * pscript),$(hb_language_t * planguage)); }|]
    (,) <$> peek pscript <*> (Language <$> peek planguage)

-- * Layout

-- | Fetches a list of all feature-lookup indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided, all
-- languages will be queried. If no list of features is provided, all features will be queried.
layout_collect_lookups :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
layout_collect_lookups face table_tag scripts languages features lookup_indices = liftIO
  [C.block|void { hb_ot_layout_collect_lookups( $face:face, $(hb_tag_t table_tag), $maybe-tags:scripts, $maybe-tags:languages, $maybe-tags:features, $set:lookup_indices); }|]

-- | Fetches a list of all feature indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided,
-- all languages will be queried. If no list of features is provided, all features will be queried.
layout_collect_features :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
layout_collect_features face table_tag scripts languages features feature_indices = liftIO
  [C.block|void { hb_ot_layout_collect_features( $face:face, $(hb_tag_t table_tag), $maybe-tags:scripts, $maybe-tags:languages, $maybe-tags:features, $set:feature_indices); }|]

-- | Fetches a list of the characters defined as having a variant under the specified "Character Variant" ("cvXX") feature tag.
layout_feature_get_characters_ :: Face -> Tag -> Int -> Int -> Int -> IO (Int, Int, [Codepoint])
layout_feature_get_characters_ face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) char_count = liftIO $
  allocaArray char_count $ \ pcharacters ->
    with (fromIntegral char_count) $ \pchar_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_feature_get_characters( $face:face, $(hb_tag_t table_tag), $(unsigned int feature_index), $(unsigned int start_offset), $(unsigned int * pchar_count), $(hb_codepoint_t * pcharacters)) }|]
      actual_char_count <- fromIntegral <$> peek pchar_count
      cs <- peekArray actual_char_count pcharacters
      pure (fromIntegral n, actual_char_count, cs)

layout_feature_get_characters :: MonadIO m => Face -> Tag -> Int -> m [Codepoint]
layout_feature_get_characters face table_tag feature_index = liftIO $ do
  (tot,ret,cs) <- layout_feature_get_characters_ face table_tag feature_index 0 1024
  if tot == ret then pure cs
  else layout_feature_get_characters_ face table_tag feature_index 1024 (tot - 1024) <&> \(_,_,ds) -> cs ++ ds

layout_feature_get_lookups_ :: Face -> Tag -> Int -> Int -> Int -> IO (Int, Int, [Int])
layout_feature_get_lookups_ face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) lookup_count = liftIO $
  allocaArray lookup_count $ \plookup_indices ->
    with (fromIntegral lookup_count) $ \plookup_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_feature_get_lookups( $face:face, $(hb_tag_t table_tag), $(unsigned int feature_index), $(unsigned int start_offset), $(unsigned int * plookup_count), $(unsigned int * plookup_indices)) }|]
      actual_lookup_count <- fromIntegral <$> peek plookup_count
      is <- peekArray actual_lookup_count plookup_indices
      pure (fromIntegral n, actual_lookup_count, fromIntegral <$> is)

-- | Fetches a list of all lookups enumerated for the specified feature, in the specified face's GSUB table or GPOS table.
layout_feature_get_lookups :: MonadIO m => Face -> Tag -> Int -> m [Int]
layout_feature_get_lookups face table_tag feature_index = liftIO $ do
  (tot,ret,cs) <- layout_feature_get_lookups_ face table_tag feature_index 0 256
  if tot == ret then pure cs
  else layout_feature_get_lookups_ face table_tag feature_index 256 (tot - 256) <&> \(_,_,ds) -> cs ++ ds
