{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.OpenType.Font
( font_set_funcs
) where

import Control.Monad.Primitive
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.OpenType.Internal

C.context $ C.baseCtx <> harfbuzzOpenTypeCtx
C.include "<hb.h>"
C.include "<hb-ot.h>"

-- | Use openType fonts with 'Graphics.Harfbuzz.Shape.shape'.
--
-- Note that fonts returned by 'Graphics.Harfbuzz.Font.font_create' default to using these functions, so most clients would never need to call this function directly.
font_set_funcs :: PrimMonad m => Font (PrimState m) -> m ()
font_set_funcs font = unsafeIOToPrim [C.block|void { hb_ot_font_set_funcs($font:font); }|]
