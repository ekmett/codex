{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
module Graphics.Harfbuzz.ICU 
( icu_get_unicode_funcs
, icu_script_from_script
, icu_script_to_script
, IcuScript(IcuScript, ICU_SCRIPT)
-- * for inline-c
, harfbuzzIcuCtx
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as Map
import Foreign.C.Types (CInt)
import Foreign.Storable
import Graphics.Harfbuzz.Internal (harfbuzzCtx, Script(..), UnicodeFuncs, foreignUnicodeFuncs)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

newtype IcuScript = IcuScript CInt
  deriving (Eq,Ord,Show,Read,Num,Storable)

C.context $ C.baseCtx <> harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "UScriptCode", [t|IcuScript|])
    ]
  }

C.include "<hb.h>"
C.include "<hb-icu.h>"

icu_get_unicode_funcs :: MonadIO m => m UnicodeFuncs
icu_get_unicode_funcs = liftIO $ do
  [C.exp|hb_unicode_funcs_t * { hb_unicode_funcs_reference(hb_icu_get_unicode_funcs()) }|] >>= foreignUnicodeFuncs

icu_script_from_script :: Script -> IcuScript
icu_script_from_script s =
  [C.pure|UScriptCode { hb_icu_script_from_script($(hb_script_t s)) }|]

icu_script_to_script :: IcuScript -> Script
icu_script_to_script s = 
  [C.pure|hb_script_t { hb_icu_script_to_script($(UScriptCode s)) }|]

pattern ICU_SCRIPT :: Script -> IcuScript
pattern ICU_SCRIPT x <- (icu_script_to_script -> x) where
  ICU_SCRIPT x = icu_script_from_script x

harfbuzzIcuCtx :: C.Context
harfbuzzIcuCtx = harfbuzzCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "UScriptCode", [t|IcuScript|])
    ]
  }
