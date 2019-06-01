{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.OpenType.Shape
( shape_glyphs_closure
, shape_plan_collect_lookups
) where

import Control.Monad.Primitive
import Foreign.Marshal.Array
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

shape_glyphs_closure :: PrimMonad m => Font (PrimState m) -> Buffer (PrimState m) -> [Feature] -> Set (PrimState m) -> m ()
shape_glyphs_closure font buffer features glyphs = unsafeIOToPrim $
  withArrayLen features $ \ (fromIntegral -> num_features) pfeatures ->
    [C.block|void { hb_ot_shape_glyphs_closure( $font:font, $buffer:buffer, $(const hb_feature_t * pfeatures), $(unsigned int num_features), $set:glyphs); }|]

shape_plan_collect_lookups :: PrimMonad m => ShapePlan (PrimState m) -> Tag -> Set (PrimState m) -> m ()
shape_plan_collect_lookups shape_plan table_tag lookup_indices = unsafeIOToPrim
  [C.block|void { hb_ot_shape_plan_collect_lookups($shape-plan:shape_plan,$(hb_tag_t table_tag),$set:lookup_indices);}|]
