{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
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

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

shape :: MonadIO m => Font -> Buffer -> [Feature] -> m ()
shape font buffer features = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.block|void{
      hb_shape($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len));
    }|]

shape_full :: MonadIO m => Font -> Buffer -> [Feature] -> [Shaper] -> m ()
shape_full font buffer features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.block|void{
         hb_shape_full($font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers));
      }|]

shape_plan_create :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Shaper] -> m ShapePlan
shape_plan_create face props features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create_cached :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Shaper] -> m ShapePlan
shape_plan_create_cached face props features shapers = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    withArray0 SHAPER_INVALID shapers $ \ (castPtr -> pshapers) ->
      [C.exp|hb_shape_plan_t * {
         hb_shape_plan_create_cached($face:face,$segment-properties:props,$(const hb_feature_t * pfeatures),$(unsigned int len),$(const char * const * pshapers))
      }|] >>= foreignShapePlan

shape_plan_create2 :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m ShapePlan
shape_plan_create2 face props features coords shapers = liftIO $
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

shape_plan_create_cached2 :: MonadIO m => Face -> SegmentProperties -> [Feature] -> [Int] -> [Shaper] -> m ShapePlan
shape_plan_create_cached2 face props features coords shapers = liftIO $
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

shape_plan_execute :: MonadIO m => ShapePlan -> Font -> Buffer -> [Feature] -> m Bool
shape_plan_execute plan font buffer features = liftIO $
  withArrayLen features $ \ (fromIntegral -> len) pfeatures ->
    [C.exp|hb_bool_t {
      hb_shape_plan_execute($shape-plan:plan,$font:font,$buffer:buffer,$(const hb_feature_t * pfeatures),$(unsigned int len))
    }|] <&> cbool

shape_plan_get_shaper :: MonadIO m => ShapePlan -> m Shaper
shape_plan_get_shaper plan = liftIO $ [C.exp|const char * { hb_shape_plan_get_shaper($shape-plan:plan) }|] <&> Shaper
