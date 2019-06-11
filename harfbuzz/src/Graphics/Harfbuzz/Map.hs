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
module Graphics.Harfbuzz.Map
( Map
, map_allocation_successful
, map_create
, map_clear
, map_del
, map_get
, map_get_population
, map_has
, map_is_empty
, map_set
, pattern MAP_VALUE_INVALID
) where

import Control.Monad.Primitive
import Data.Functor ((<&>))
import Foreign.C.Types
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as U

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

map_allocation_successful :: PrimMonad m => Map (PrimState m) -> m Bool
map_allocation_successful s = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_map_allocation_successful($map:s) }|] <&> cbool

map_clear :: PrimMonad m => Map (PrimState m) -> m ()
map_clear s = unsafeIOToPrim [U.block|void { hb_map_clear($map:s); }|]

map_create :: PrimMonad m => m (Map (PrimState m))
map_create = unsafeIOToPrim $ [U.exp|hb_map_t * { hb_map_create() }|] >>= foreignMap

map_del :: PrimMonad m => Map (PrimState m) -> Codepoint -> m ()
map_del s c = unsafeIOToPrim [U.block|void { hb_map_del($map:s,$(hb_codepoint_t c)); }|]

map_get :: PrimMonad m => Map (PrimState m) -> Codepoint -> m Codepoint
map_get s c = unsafeIOToPrim [U.exp|hb_codepoint_t { hb_map_get($map:s,$(hb_codepoint_t c)) }|]

map_get_population :: PrimMonad m => Map (PrimState m) -> m Int
map_get_population s = unsafeIOToPrim $ [U.exp|unsigned int { hb_map_get_population($map:s) }|] <&> fromIntegral

map_has :: PrimMonad m => Map (PrimState m) -> Codepoint -> m Bool
map_has s c = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_map_has($map:s,$(hb_codepoint_t c)) }|] <&> cbool

map_is_empty :: PrimMonad m => Map (PrimState m) -> m Bool
map_is_empty s = unsafeIOToPrim $ [U.exp|hb_bool_t { hb_map_is_empty($map:s) }|] <&> cbool

map_set :: PrimMonad m => Map (PrimState m) -> Codepoint -> Codepoint -> m ()
map_set s k v = unsafeIOToPrim [U.block|void { hb_map_set($map:s,$(hb_codepoint_t k),$(hb_codepoint_t v)); }|]
