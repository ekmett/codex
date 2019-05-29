{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
-- |
module Graphics.Harfbuzz.OpenType.Font
( font_set_funcs
) where

import Control.Monad.IO.Class
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- | Use openType fonts with 'Graphics.Harfbuzz.Shape.shape'.
--
-- Note that fonts returned by 'Graphics.Harfbuzz.Font.font_create' default to using these functions, so most clients would never need to call this function directly.
font_set_funcs :: MonadIO m => Font -> m ()
font_set_funcs font = liftIO [C.block|void { hb_ot_font_set_funcs($font:font); }|]
