{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
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
module Graphics.Harfbuzz.Shape
( shape -- the point of all of this
, shape_full
, shape_list_shapers

, ShapePlan
, shape_plan_create
, shape_plan_create_cached
, shape_plan_create2
, shape_plan_create_cached2
, shape_plan_execute
, shape_plan_get_shaper

, Shaper(SHAPER_INVALID)
, shaper_from_string, shaper_to_string
) where

import Control.Monad.Primitive
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

shape :: PrimMonad m => Font (PrimState m) -> Buffer (PrimState m) -> [Feature] -> m ()
shape font buffer features = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.block|void{
      hb_shape($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len));
    }|]

shape_full :: PrimMonad m => Font (PrimState m) -> Buffer (PrimState m) -> [Feature] -> [Shaper] -> m ()
shape_full font buffer features shapers = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.block|void{
         hb_shape_full($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers));
      }|]

shape_plan_create :: PrimMonad m => Face (PrimState m) -> SegmentProperties -> [Feature] -> [Shaper] -> m (ShapePlan (PrimState m))
shape_plan_create face props features shapers = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create_cached :: PrimMonad m => Face (PrimState m) -> SegmentProperties -> [Feature] -> [Shaper] -> m (ShapePlan (PrimState m))
shape_plan_create_cached face props features shapers = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create_cached($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create2 :: PrimMonad m => Face (PrimState m) -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m (ShapePlan (PrimState m))
shape_plan_create2 face props features coords shapers = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArrayLen (fromIntegral <$> coords) $ \ (fromIntegral -> num_coords) pcoords ->
      withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
        [C.exp|hb_shape_plan_t * {
           hb_shape_plan_create2(
             $face:face,
             $segment-properties:props,
             $(const hb_feature_t * pfeatures),$(unsigned int len),
             $(const int * pcoords),$(unsigned int num_coords),
             $(const char * const * pshapers))
        }|] >>= foreignShapePlan

shape_plan_create_cached2 :: PrimMonad m => Face (PrimState m) -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m (ShapePlan (PrimState m))
shape_plan_create_cached2 face props features coords shapers = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArrayLen (fromIntegral <$> coords) $ \ (fromIntegral -> num_coords) pcoords ->
      withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
        [C.exp|hb_shape_plan_t * {
           hb_shape_plan_create_cached2(
             $face:face,
             $segment-properties:props,
             $(const hb_feature_t * pfeatures),$(unsigned int len),
             $(const int * pcoords),$(unsigned int num_coords),
             $(const char * const * pshapers))
        }|] >>= foreignShapePlan

shape_plan_execute :: PrimMonad m => ShapePlan (PrimState m) -> Font (PrimState m) -> Buffer (PrimState m) -> [Feature] -> m Bool
shape_plan_execute plan font buffer features = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.exp|hb_bool_t {
      hb_shape_plan_execute($shape-plan:plan,$font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len))
    }|] <&> cbool

shape_plan_get_shaper :: PrimMonad m => ShapePlan (PrimState m) -> m Shaper
shape_plan_get_shaper plan = unsafeIOToPrim $ [C.exp|const char * { hb_shape_plan_get_shaper($shape-plan:plan) }|] <&> Shaper
