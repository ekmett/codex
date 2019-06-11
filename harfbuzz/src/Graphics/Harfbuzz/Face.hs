{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language BlockArguments #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
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

import Control.Monad.Primitive
import Data.Functor ((<&>))
import Data.Primitive.StateVar
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as U

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Object
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"
C.include "HsFFI.h"

face_builder_create :: PrimMonad m => m (Face (PrimState m))
face_builder_create = unsafeIOToPrim $ [U.exp|hb_face_t * { hb_face_builder_create() }|] >>= foreignFace

face_builder_add_table :: PrimMonad m => Face (PrimState m) -> Tag -> Blob (PrimState m) -> m Bool
face_builder_add_table f t b = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_face_builder_add_table($face:f,$(hb_tag_t t),$blob:b) }|] <&> cbool

face_reference_blob :: PrimMonad m => Face (PrimState m) -> m (Blob (PrimState m))
face_reference_blob f = unsafeIOToPrim $ [U.exp|hb_blob_t * { hb_face_reference_blob($face:f) }|] >>= foreignBlob

face_reference_table :: PrimMonad m => Face (PrimState m) -> Tag -> m (Blob (PrimState m))
face_reference_table f t = unsafeIOToPrim $ [U.exp|hb_blob_t * { hb_face_reference_table($face:f,$(hb_tag_t t)) }|] >>= foreignBlob

-- add the unicode codepoints present in the face to the given set
face_collect_unicodes :: PrimMonad m => Face (PrimState m) -> Set (PrimState m) -> m ()
face_collect_unicodes f s = unsafeIOToPrim [U.block|void { hb_face_collect_unicodes($face:f,$set:s); }|]

face_collect_variation_selectors :: PrimMonad m => Face (PrimState m) -> Set (PrimState m) -> m ()
face_collect_variation_selectors f s = unsafeIOToPrim [U.block|void { hb_face_collect_variation_selectors($face:f,$set:s); }|]

face_collect_variation_unicodes :: PrimMonad m => Face (PrimState m) -> Codepoint -> Set (PrimState m) -> m ()
face_collect_variation_unicodes f variation s = unsafeIOToPrim [U.block|void { hb_face_collect_variation_unicodes($face:f,$(hb_codepoint_t variation),$set:s); }|]

face_count :: PrimMonad m => Blob (PrimState m) -> m Int
face_count b = unsafeIOToPrim $ [U.exp|int { hb_face_count($blob:b) }|] <&> fromIntegral

face_create :: PrimMonad m => Blob (PrimState m) -> Int -> m (Face (PrimState m))
face_create b (fromIntegral -> i) = unsafeIOToPrim $
  [U.exp|hb_face_t * { hb_face_create($blob:b,$(int i)) }|] >>= foreignFace

face_create_for_tables :: PrimBase m => (Face (PrimState m) -> Tag -> m (Blob (PrimState m))) -> m (Face (PrimState m))
face_create_for_tables fun = unsafeIOToPrim do
  (castFunPtr -> f) <- mkReferenceTableFunc \ pface tag _ -> do
    face <- [U.exp|hb_face_t * { hb_face_reference($(hb_face_t * pface)) }|] >>= foreignFace
    unsafePrimToIO do
      b <- fun face tag
      object_reference b
  [U.block|hb_face_t * {
    hb_reference_table_func_t f = $(hb_reference_table_func_t f);
    return hb_face_create_for_tables(f,f,(hb_destroy_func_t)hs_free_fun_ptr);
  }|] >>= foreignFace

-- | Subsumes @hb_face_get_glyph_count@ and @hb_face_set_glyph_count@
face_glyph_count :: Face s -> StateVar s Int
face_glyph_count f = unsafeStateVar g s where
  g = [U.exp|int { hb_face_get_glyph_count($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [U.block|void { hb_face_set_glyph_count($face:f,$(unsigned int v)); }|]

-- | Subsumes @hb_face_get_index@ ancd @hb_face_set_index@
face_index :: Face s -> StateVar s Int
face_index f = unsafeStateVar g s where
  g = [U.exp|int { hb_face_get_index($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [U.block|void { hb_face_set_index($face:f,$(unsigned int v)); }|]

face_upem :: Face s -> StateVar s Int
face_upem f = unsafeStateVar g s where
  g = [U.exp|int { hb_face_get_upem($face:f) }|] <&> fromIntegral
  s (fromIntegral -> v) = [U.block|void { hb_face_set_upem($face:f,$(unsigned int v)); }|]

face_is_immutable :: PrimMonad m => Face (PrimState m) -> m Bool
face_is_immutable b = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_face_is_immutable($face:b) }|] <&> cbool

face_make_immutable :: PrimMonad m => Face (PrimState m) -> m ()
face_make_immutable b = unsafeIOToPrim [U.block|void { hb_face_make_immutable($face:b); }|]
