{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language TemplateHaskell #-}
-- |
module Graphics.Harfbuzz.OpenType.Shape
( shape_glyphs_closure
) where

import Control.Monad.IO.Class
import Foreign.Marshal.Array
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

shape_glyphs_closure :: MonadIO m => Font -> Buffer -> [Feature] -> Set -> m ()
shape_glyphs_closure font buffer features glyphs = liftIO $
  withArrayLen features $ \ (fromIntegral -> num_features) pfeatures ->
    [C.block|void { hb_ot_shape_glyphs_closure( $font:font, $buffer:buffer, $(const hb_feature_t * pfeatures), $(unsigned int num_features), $set:glyphs); }|]
