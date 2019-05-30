{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
module Graphics.Harfbuzz.Face
( Face
, face_collect_unicodes
, face_collect_variation_selectors
, face_collect_variation_unicodes
, face_count
, face_create
, face_create_for_tables
, face_glyph_count -- statevar
, face_index -- statevar
, face_is_immutable
, face_make_immutable
, face_builder_create
, face_builder_add_table
, face_reference_blob
, face_reference_table
, face_upem -- statevar
) where

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Data.StateVar
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Object
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

face_builder_create :: MonadIO m => m Face
face_builder_create = liftIO $ [C.exp|hb_face_t * { hb_face_builder_create() }|] >>= foreignFace

face_builder_add_table :: MonadIO m => Face -> Tag -> Blob -> m Bool
face_builder_add_table f t b = liftIO $ [C.exp|hb_bool_t { hb_face_builder_add_table($face:f,$(hb_tag_t t),$blob:b) }|] <&> cbool

face_reference_blob :: MonadIO m => Face -> m Blob
face_reference_blob f = liftIO $ [C.exp|hb_blob_t * { hb_face_reference_blob($face:f) }|] >>= foreignBlob

face_reference_table :: MonadIO m => Face -> Tag -> m Blob
face_reference_table f t = liftIO $ [C.exp|hb_blob_t * { hb_face_reference_table($face:f,$(hb_tag_t t)) }|] >>= foreignBlob

-- add the unicode codepoints present in the face to the given set
face_collect_unicodes :: MonadIO m => Face -> Set -> m ()
face_collect_unicodes f s = liftIO [C.block|void { hb_face_collect_unicodes($face:f,$set:s); }|]

face_collect_variation_selectors :: MonadIO m => Face -> Set -> m ()
face_collect_variation_selectors f s = liftIO [C.block|void { hb_face_collect_variation_selectors($face:f,$set:s); }|]

face_collect_variation_unicodes :: MonadIO m => Face -> Codepoint -> Set -> m ()
face_collect_variation_unicodes f variation s = liftIO [C.block|void { hb_face_collect_variation_unicodes($face:f,$(hb_codepoint_t variation),$set:s); }|]

face_count :: MonadIO m => Blob -> m Int
face_count b = liftIO $ [C.exp|int { hb_face_count($blob:b) }|] <&> fromIntegral

face_create :: MonadIO m => Blob -> Int -> m Face
face_create b (fromIntegral -> i) = liftIO $
  [C.exp|hb_face_t * { hb_face_create($blob:b,$(int i)) }|] >>= foreignFace

face_create_for_tables :: MonadIO m => (Face -> Tag -> IO Blob) -> m Face
face_create_for_tables fun = liftIO $ do
  (castFunPtr -> f) <- mkReferenceTableFunc $ \ pface tag _ -> do
    face <- [C.exp|hb_face_t * { hb_face_reference($(hb_face_t * pface)) }|] >>= foreignFace
    b <- fun face tag
    object_reference b
  [C.block|hb_face_t * {
    hb_reference_table_func_t f = $(hb_reference_table_func_t f);
    return hb_face_create_for_tables(f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|] >>= foreignFace

-- | Subsumes @hb_face_get_glyph_count@ and @hb_face_set_glyph_count@
face_glyph_count :: Face -> StateVar Int
face_glyph_count f = StateVar g s where
  g = [C.exp|int { hb_face_get_glyph_count($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_glyph_count($face:f,$(unsigned int v)); }|]

-- | Subsumes @hb_face_get_index@ ancd @hb_face_set_index@
face_index :: Face -> StateVar Int
face_index f = StateVar g s where
  g = [C.exp|int { hb_face_get_index($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_index($face:f,$(unsigned int v)); }|]

face_upem :: Face -> StateVar Int
face_upem f = StateVar g s where
  g = [C.exp|int { hb_face_get_upem($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [C.block|void { hb_face_set_upem($face:f,$(unsigned int v)); }|]

face_is_immutable :: MonadIO m => Face -> m Bool
face_is_immutable b = liftIO $ [C.exp|hb_bool_t { hb_face_is_immutable($face:b) }|] <&> cbool

face_make_immutable :: MonadIO m => Face -> m ()
face_make_immutable b = liftIO [C.block|void { hb_face_make_immutable($face:b); }|]
