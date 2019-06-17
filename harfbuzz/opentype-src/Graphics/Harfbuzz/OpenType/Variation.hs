{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# options_ghc -Wno-incomplete-uni-patterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.OpenType.Variation
( pattern TAG_VAR_AXIS_ITALIC
, pattern TAG_VAR_AXIS_OPTICAL_SIZE
, pattern TAG_VAR_AXIS_SLANT
, pattern TAG_VAR_AXIS_WIDTH
, pattern TAG_VAR_AXIS_WEIGHT
, var_has_data
, var_get_axis_count
, var_get_axis_infos
, var_find_axis_info
, var_get_named_instance_count
, var_named_instance_get_subfamily_name_id
, var_named_instance_get_postscript_name_id
, var_normalize_variations
, var_normalize_coords
) where

import Control.Monad.Primitive
import Control.Monad.ST.Unsafe
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal
import Graphics.Harfbuzz.OpenType.Private

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

var_has_data :: PrimMonad m => Face (PrimState m) -> m Bool
var_has_data face = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_ot_var_has_data($face:face) }|] <&> cbool

var_get_axis_count :: PrimMonad m => Face (PrimState m) -> m Int
var_get_axis_count face = unsafeIOToPrim $ [C.exp|unsigned int { hb_ot_var_get_axis_count($face:face) }|] <&> fromIntegral

var_get_axis_infos :: PrimMonad m => Face (PrimState m) -> m [VarAxisInfo]
var_get_axis_infos face = pump 8 $ \start_offset requested_axes_count ->
  with requested_axes_count $ \paxes_count ->
    allocaArray (fromIntegral requested_axes_count) $ \paxes -> do
      total_axes <- [C.exp|unsigned int { hb_ot_var_get_axis_infos($face:face,$(unsigned int start_offset),$(unsigned int * paxes_count),$(hb_ot_var_axis_info_t * paxes)) }|]
      retrieved_axes <- peek paxes_count
      axes <- peekArray (fromIntegral retrieved_axes) paxes
      pure (total_axes, retrieved_axes, axes)

var_find_axis_info :: PrimMonad m => Face (PrimState m) -> Tag -> m (Maybe VarAxisInfo)
var_find_axis_info face axis_tag = unsafeIOToPrim $
  alloca $ \paxis_info -> do
    b <- [C.exp|hb_bool_t { hb_ot_var_find_axis_info($face:face,$(hb_tag_t axis_tag),$(hb_ot_var_axis_info_t * paxis_info)) }|]
    if cbool b then Just <$> peek paxis_info else pure Nothing

var_get_named_instance_count :: PrimMonad m => Face (PrimState m) -> m Int
var_get_named_instance_count face = unsafeIOToPrim $ [C.exp|unsigned int { hb_ot_var_get_named_instance_count($face:face) }|] <&> fromIntegral

var_named_instance_get_subfamily_name_id :: PrimMonad m => Face (PrimState m) -> Int -> m Name
var_named_instance_get_subfamily_name_id face (fromIntegral -> instance_index) = unsafeIOToPrim [C.exp|hb_ot_name_id_t { hb_ot_var_named_instance_get_subfamily_name_id($face:face,$(unsigned int instance_index)) }|]

var_named_instance_get_postscript_name_id :: PrimMonad m => Face (PrimState m) -> Int -> m Name
var_named_instance_get_postscript_name_id face (fromIntegral -> instance_index) = unsafeIOToPrim [C.exp|hb_ot_name_id_t { hb_ot_var_named_instance_get_postscript_name_id($face:face,$(unsigned int instance_index)) }|]

var_normalize_variations :: PrimMonad m => Face (PrimState m) -> [Variation] -> m [Int]
var_normalize_variations face variations = unsafeIOToPrim $ do
  n@(fromIntegral -> num_coords) <- unsafeSTToIO $ var_get_axis_count face
  withArrayLen variations $ \ (fromIntegral -> variations_length) pvariations ->
    allocaArray n $ \ pcoords -> do
      [C.block|void {
        hb_ot_var_normalize_variations($face:face,$(hb_variation_t * pvariations),$(unsigned int variations_length),$(int * pcoords),$(unsigned int num_coords));
      }|]
      fmap fromIntegral <$> peekArray n pcoords

var_normalize_coords :: (PrimMonad m, Traversable f) => Face (PrimState m) -> f Float -> m (f Int)
var_normalize_coords face design = unsafeIOToPrim $
  withArrayLen (coerce $ toList design) $ \len@(fromIntegral -> n) pdesign_coords ->
    allocaArray len $ \pnormalized_coords -> do
      [C.block|void { hb_ot_var_normalize_coords($face:face,$(unsigned int n),$(const float * pdesign_coords),$(int * pnormalized_coords)); }|]
      let step _ = state $ \ (x:xs) -> (fromIntegral x,xs)
      peekArray len pnormalized_coords <&> evalState (traverse step design)
