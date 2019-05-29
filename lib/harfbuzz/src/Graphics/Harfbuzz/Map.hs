{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language QuasiQuotes #-}
{-# language LambdaCase #-}
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

import Control.Monad.IO.Class
import Data.Functor ((<&>))
import Foreign.C.Types
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

map_allocation_successful :: MonadIO m => Map -> m Bool
map_allocation_successful s = liftIO $ [C.exp|hb_bool_t { hb_map_allocation_successful($map:s) }|] <&> cbool

map_clear :: MonadIO m => Map -> m ()
map_clear s = liftIO [C.block|void { hb_map_clear($map:s); }|]

map_create :: MonadIO m => m Map
map_create = liftIO $ [C.exp|hb_map_t * { hb_map_create() }|] >>= foreignMap

map_del :: MonadIO m => Map -> Codepoint -> m ()
map_del s c = liftIO [C.block|void { hb_map_del($map:s,$(hb_codepoint_t c)); }|]

map_get :: MonadIO m => Map -> Codepoint -> m Codepoint
map_get s c = liftIO [C.exp|hb_codepoint_t { hb_map_get($map:s,$(hb_codepoint_t c)) }|]

map_get_population :: MonadIO m => Map -> m Int
map_get_population s = liftIO $ [C.exp|unsigned int { hb_map_get_population($map:s) }|] <&> fromIntegral

map_has :: MonadIO m => Map -> Codepoint -> m Bool
map_has s c = liftIO $ [C.exp|hb_bool_t { hb_map_has($map:s,$(hb_codepoint_t c)) }|] <&> cbool

map_is_empty :: MonadIO m => Map -> m Bool
map_is_empty s = liftIO $ [C.exp|hb_bool_t { hb_map_is_empty($map:s) }|] <&> cbool

map_set :: MonadIO m => Map -> Codepoint -> Codepoint -> m ()
map_set s k v = liftIO [C.block|void { hb_map_set($map:s,$(hb_codepoint_t k),$(hb_codepoint_t v)); }|]
