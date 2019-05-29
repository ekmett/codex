{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
-- | @hb-ot.h@
module Graphics.Harfbuzz.OpenType
( OpenTypeName(..)
, OpenTypeLayoutGlyphClass(..)
, pattern OT_TAG_BASE
, pattern OT_TAG_GDEF
, pattern OT_TAG_GPOS
, pattern OT_TAG_GSUB
, pattern OT_TAG_JSTF
, pattern OT_TAG_DEFAULT_LANGUAGE
, pattern OT_TAG_DEFAULT_SCRIPT
, ot_layout_collect_features
, ot_layout_collect_lookups
, ot_layout_feature_get_characters
, ot_layout_feature_get_lookups
, ot_tag_to_language
, ot_tag_to_script
, ot_tags_from_script_and_language
, ot_tags_to_script_and_language
) where

import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Unsafe
import Foreign.Marshal.Utils
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- | Fetches a list of all feature-lookup indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided, all
-- languages will be queried. If no list of features is provided, all features will be queried.
ot_layout_collect_lookups :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
ot_layout_collect_lookups face table_tag scripts languages features lookup_indices = liftIO $
  [C.block|void {
    hb_ot_layout_collect_lookups(
      $face:face,
      $(hb_tag_t table_tag),
      $maybe-tags:scripts,
      $maybe-tags:languages,
      $maybe-tags:features,
      $set:lookup_indices
    );
  }|]

-- | Fetches a list of all feature indexes in the specified face's GSUB table or GPOS table, underneath the specified scripts,
-- languages, and features. If no list of scripts is provided, all scripts will be queried. If no list of languages is provided,
-- all languages will be queried. If no list of features is provided, all features will be queried.
ot_layout_collect_features :: MonadIO m => Face -> Tag -> Maybe [Tag] -> Maybe [Tag] -> Maybe [Tag] -> Set -> m ()
ot_layout_collect_features face table_tag scripts languages features feature_indices = liftIO $
  [C.block|void {
    hb_ot_layout_collect_features(
      $face:face,
      $(hb_tag_t table_tag),
      $maybe-tags:scripts,
      $maybe-tags:languages,
      $maybe-tags:features,
      $set:feature_indices
    );
  }|]

-- | Fetches a list of the characters defined as having a variant under the specified "Character Variant" ("cvXX") feature tag.
--
-- Note: If the length of the list of codepoints is equal to the supplied char_count then there is a chance that there where
-- more characters defined under the feature tag than were returned. This function can be called with incrementally larger
-- start_offset until the char_count output value is lower than its input value, or the size of the characters array can be increased.
ot_layout_feature_get_characters :: MonadIO m => Face -> Tag -> Int -> Int -> Int -> m (Int, [Codepoint])
ot_layout_feature_get_characters face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) char_count = liftIO $
  allocaArray char_count $ \ pcharacters ->
    with (fromIntegral char_count) $ \pchar_count -> do
      n <- [C.exp|unsigned int {
        hb_ot_layout_feature_get_characters(
          $face:face,
          $(hb_tag_t table_tag),
          $(unsigned int feature_index),
          $(unsigned int start_offset),
          $(unsigned int * pchar_count),
          $(hb_codepoint_t * pcharacters)
        )
      }|]
      actual_char_count <- peek pchar_count
      cs <- peekArray (fromIntegral actual_char_count) pcharacters
      pure (fromIntegral n, cs)


-- | Fetches a list of all lookups enumerated for the specified feature, in the specified face's GSUB table or GPOS table.
-- The list returned will begin at the offset provided.
ot_layout_feature_get_lookups :: MonadIO m => Face -> Tag -> Int -> Int -> Int -> m (Int, [Int])
ot_layout_feature_get_lookups face table_tag (fromIntegral -> feature_index) (fromIntegral -> start_offset) lookup_count = liftIO $
  allocaArray lookup_count $ \plookup_indices ->
    with (fromIntegral lookup_count) $ \plookup_count -> do
      n <- [C.exp|unsigned int {
        hb_ot_layout_feature_get_lookups(
          $face:face,
          $(hb_tag_t table_tag),
          $(unsigned int feature_index),
          $(unsigned int start_offset),
          $(unsigned int * plookup_count),
          $(unsigned int * plookup_indices)
        )
      }|]
      actual_lookup_count <- peek plookup_count
      is <- peekArray (fromIntegral actual_lookup_count) plookup_indices
      pure (fromIntegral n, fromIntegral <$> is)

ot_tag_to_script :: Tag -> Script
ot_tag_to_script tag =[C.pure|hb_script_t { hb_ot_tag_to_script($(hb_tag_t tag)) }|]

ot_tag_to_language :: Tag -> Language
ot_tag_to_language tag = Language [C.pure|hb_language_t { hb_ot_tag_to_language($(hb_tag_t tag)) }|]

ot_tags_from_script_and_language :: Script -> Language -> ([Tag],[Tag])
ot_tags_from_script_and_language script language = unsafeLocalState $
  allocaArray 256 $ \pscripts ->
    withArray [128,128] $ \pscript_count -> do
      let planguages = advancePtr pscripts 128
          planguage_count = advancePtr pscript_count 1
      [C.block|void {
        hb_ot_tags_from_script_and_language(
          $(hb_script_t script),
          $language:language,
          $(unsigned int * pscript_count),
          $(hb_tag_t * pscripts),
          $(unsigned int * planguage_count),
          $(hb_tag_t * planguages)
        );
      }|]
      nscripts <- fromIntegral <$> peek pscript_count
      nlanguages <- fromIntegral <$> peek planguage_count
      (,) <$> peekArray nscripts pscripts <*> peekArray nlanguages planguages

ot_tags_to_script_and_language :: Tag -> Tag -> (Script,Language)
ot_tags_to_script_and_language script_tag language_tag = unsafeLocalState $ alloca $ \pscript -> alloca $ \ planguage -> do
  [C.block|void {
    hb_ot_tags_to_script_and_language(
      $(hb_tag_t script_tag),$(hb_tag_t language_tag),$(hb_script_t * pscript),$(hb_language_t * planguage)
    );
  }|]
  (,) <$> peek pscript <*> (Language <$> peek planguage)
