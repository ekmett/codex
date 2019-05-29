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
, layout_feature_get_name_ids
, layout_feature_with_variations_get_lookups
, layout_get_attach_points
, layout_get_glyph_class
, layout_get_glyphs_in_class
, layout_get_ligature_carets
, layout_get_size_params
, layout_has_glyph_classes
, layout_has_positioning
, layout_has_substitution
, layout_language_find_feature
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
  if tot == ret then pure cs else layout_feature_get_characters_ face table_tag feature_index 1024 (tot - 1024) <&> \(_,_,ds) -> cs ++ ds

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
layout_feature_get_lookups face table_tag feature_index = pump 256 $ layout_feature_get_lookups_ face table_tag feature_index

-- | Tag = TAG_GSUB or TAG_POS
layout_feature_get_name_ids :: MonadIO m => Face -> Tag -> Int -> m (Maybe (Name, Name, Name, Int, Name))
layout_feature_get_name_ids face table_tag (fromIntegral -> feature_index) = liftIO $
  allocaArray 4 $ \ pids ->
    alloca $ \pnum_named_parameters -> do
      b <- [C.block|hb_bool_t {
         hb_ot_name_id_t * ids = $(hb_ot_name_id_t * pids);
         return hb_ot_layout_feature_get_name_ids($face:face,$(hb_tag_t table_tag),$(unsigned int feature_index),ids,ids+1,ids+2,$(unsigned int * pnum_named_parameters),ids+3);
      }|]
      if cbool b then fmap Just $ (,,,,) <$> peek pids <*> peek (advancePtr pids 1) <*> peek (advancePtr pids 2) <*> (fromIntegral <$> peek pnum_named_parameters) <*> peek (advancePtr pids 3)
      else pure Nothing

layout_feature_with_variations_get_lookups_ :: Face -> Tag -> Int -> Int -> Int -> Int -> IO (Int,Int,[Int])
layout_feature_with_variations_get_lookups_ face table_tag (fromIntegral -> feature_index) (fromIntegral -> variations_index) (fromIntegral -> start_offset) lookup_count =
  allocaArray lookup_count $ \ plookup_indices ->
    with (fromIntegral lookup_count) $ \plookup_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_feature_with_variations_get_lookups($face:face,$(hb_tag_t table_tag),$(unsigned int feature_index),$(unsigned int variations_index),$(unsigned int start_offset),$(unsigned int * plookup_count), $(hb_codepoint_t * plookup_indices)) }|]
      actual_lookup_count <- fromIntegral <$> peek plookup_count
      cs <- peekArray actual_lookup_count plookup_indices
      pure (fromIntegral n, actual_lookup_count, fromIntegral <$> cs)

layout_feature_with_variations_get_lookups :: MonadIO m => Face -> Tag -> Int -> Int -> m [Int]
layout_feature_with_variations_get_lookups face table_tag feature_index variations_index = pump 256 $ layout_feature_with_variations_get_lookups_ face table_tag feature_index variations_index

layout_get_attach_points_ :: Face -> Codepoint -> Int -> Int -> IO (Int,Int,[Int])
layout_get_attach_points_ face glyph (fromIntegral -> start_offset) point_count = do
  allocaArray point_count $ \ ppoint_array ->
    with (fromIntegral point_count) $ \ppoint_count -> do
      n <- [C.exp|unsigned int { hb_ot_layout_get_attach_points($face:face,$(hb_codepoint_t glyph),$(unsigned int start_offset),$(unsigned int * ppoint_count), $(hb_codepoint_t * ppoint_array)) }|]
      actual_point_count <- fromIntegral <$> peek ppoint_count
      cs <- peekArray actual_point_count ppoint_array
      pure (fromIntegral n, actual_point_count, fromIntegral <$> cs)

layout_get_attach_points :: MonadIO m => Face -> Codepoint -> m [Int]
layout_get_attach_points face glyph = pump 8 $ layout_get_attach_points_ face glyph

layout_get_glyph_class :: MonadIO m => Face -> Codepoint -> m LayoutGlyphClass
layout_get_glyph_class face glyph = liftIO [C.exp|hb_ot_layout_glyph_class_t { hb_ot_layout_get_glyph_class($face:face,$(hb_codepoint_t glyph)) }|]

layout_get_glyphs_in_class :: MonadIO m => Face -> LayoutGlyphClass -> Set -> m ()
layout_get_glyphs_in_class face glyph_class set = liftIO [C.block|void { hb_ot_layout_get_glyphs_in_class($face:face,$(hb_ot_layout_glyph_class_t glyph_class),$set:set); }|]

layout_get_ligature_carets_ :: Font -> Direction -> Codepoint -> Int -> Int -> IO (Int,Int,[Position])
layout_get_ligature_carets_ font direction glyph (fromIntegral -> start_offset) count = do -- fairly generic
  allocaArray count $ \ parray ->
    with (fromIntegral count) $ \pcount -> do
      n <- [C.exp|unsigned int { hb_ot_layout_get_ligature_carets($font:font,$(hb_direction_t direction),$(hb_codepoint_t glyph),$(unsigned int start_offset),$(unsigned int * pcount),$(hb_position_t * parray)) }|]
      actual_count <- fromIntegral <$> peek pcount
      cs <- peekArray actual_count parray
      pure (fromIntegral n, actual_count, cs)

layout_get_ligature_carets :: Font -> Direction -> Codepoint -> IO [Position]
layout_get_ligature_carets font direction glyph = pump 4 $ layout_get_ligature_carets_ font direction glyph

data LayoutSizeParams = LayoutSizeParams
  { layout_size_params_design_size       :: {-# unpack #-} !Int
  , layout_size_params_subfamily_id      :: {-# unpack #-} !Int
  , layout_size_params_subfamily_name_id :: {-# unpack #-} !Name
  , layout_size_params_range_start       :: {-# unpack #-} !Int
  , layout_size_params_range_end         :: {-# unpack #-} !Int
  } deriving (Eq,Ord,Show)

layout_get_size_params :: MonadIO m => Face -> m (Maybe LayoutSizeParams) -- packaged for convenience
layout_get_size_params face = liftIO $
  allocaArray 4 $ \ ss ->
    alloca $ \ psubfamily_name_id -> do
      b <- [C.block|hb_bool_t {
        unsigned int * ss = $(unsigned int * ss);
        return hb_ot_layout_get_size_params($face:face,ss,ss+1,$(hb_ot_name_id_t * psubfamily_name_id),ss+2,ss+3);
      }|]
      if cbool b then do
        design_size <- fromIntegral <$> peek ss
        subfamily_id <- fromIntegral <$> peek (advancePtr ss 1)
        subfamily_name_id <- peek (psubfamily_name_id)
        range_start <- fromIntegral <$> peek (advancePtr ss 2)
        range_end <- fromIntegral <$> peek (advancePtr ss 3)
        pure $ Just $ LayoutSizeParams design_size subfamily_id subfamily_name_id range_start range_end
      else pure Nothing

layout_has_glyph_classes :: MonadIO m => Face -> m Bool
layout_has_glyph_classes face = liftIO $ [C.exp|hb_bool_t { hb_ot_layout_has_glyph_classes($face:face) }|] <&> cbool

layout_has_positioning :: MonadIO m => Face -> m Bool
layout_has_positioning face = liftIO $ [C.exp|hb_bool_t { hb_ot_layout_has_positioning($face:face) }|] <&> cbool

layout_has_substitution :: MonadIO m => Face -> m Bool
layout_has_substitution face = liftIO $ [C.exp|hb_bool_t { hb_ot_layout_has_substitution($face:face) }|] <&> cbool

layout_language_find_feature :: MonadIO m => Face -> Tag -> Int -> Int -> Tag -> m (Maybe Int)
layout_language_find_feature face table_tag (fromIntegral -> script_index) (fromIntegral -> language_index) feature = liftIO $
  alloca $ \pfeature_index -> do
    b <- [C.exp|hb_bool_t { hb_ot_layout_language_find_feature($face:face,$(hb_tag_t table_tag),$(unsigned int script_index),$(unsigned int language_index),$(hb_tag_t feature),$(unsigned int * pfeature_index)) }|]
    if cbool b then Just . fromIntegral <$> peek pfeature_index else pure Nothing


