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
module Graphics.Harfbuzz.Set
( Set
, set_add
, set_add_range
, set_allocation_successful
, set_clear
, set_create
, set_del
, set_del_range
, set_get_max
, set_get_min
, set_get_population
, set_has
, set_intersect
, set_is_empty
, set_is_equal
, set_is_subset
, set_next
, set_next_range
, set_previous
, set_previous_range
, set_set
, set_subtract
, set_symmetric_difference
, set_union
, pattern SET_VALUE_INVALID
) where

import Control.Monad.Primitive
import Data.Functor ((<&>))
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import qualified Language.C.Inline as C

import Graphics.Harfbuzz.Internal
import Graphics.Harfbuzz.Private

C.context $ C.baseCtx <> harfbuzzCtx
C.include "<hb.h>"

-- * sets

set_add :: PrimMonad m => Set (PrimState m) -> Codepoint -> m ()
set_add s c = unsafeIOToPrim [C.block|void { hb_set_add($set:s,$(hb_codepoint_t c)); }|]

set_add_range :: PrimMonad m => Set (PrimState m) -> Codepoint -> Codepoint -> m ()
set_add_range s lo hi = unsafeIOToPrim [C.block|void { hb_set_add_range($set:s,$(hb_codepoint_t lo),$(hb_codepoint_t hi)); }|]

set_allocation_successful :: PrimMonad m => Set (PrimState m) -> m Bool
set_allocation_successful s = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_set_allocation_successful($set:s) }|] <&> cbool

set_clear :: PrimMonad m => Set (PrimState m) -> m ()
set_clear s = unsafeIOToPrim [C.block|void { hb_set_clear($set:s); }|]

set_create :: PrimMonad m => m (Set (PrimState m))
set_create = unsafeIOToPrim $ [C.exp|hb_set_t * { hb_set_create() }|] >>= foreignSet

set_del :: PrimMonad m => Set (PrimState m) -> Codepoint -> m ()
set_del s c = unsafeIOToPrim [C.block|void { hb_set_del($set:s,$(hb_codepoint_t c)); }|]

set_del_range :: PrimMonad m => Set (PrimState m) -> Codepoint -> Codepoint -> m ()
set_del_range s lo hi = unsafeIOToPrim [C.block|void { hb_set_del_range($set:s,$(hb_codepoint_t lo),$(hb_codepoint_t hi)); }|]

set_get_max :: PrimMonad m => Set (PrimState m) -> m Codepoint
set_get_max s = unsafeIOToPrim [C.exp|hb_codepoint_t { hb_set_get_max($set:s) }|]

set_get_min :: PrimMonad m => Set (PrimState m) -> m Codepoint
set_get_min s = unsafeIOToPrim [C.exp|hb_codepoint_t { hb_set_get_min($set:s) }|]

set_get_population :: PrimMonad m => Set (PrimState m) -> m Int
set_get_population s = unsafeIOToPrim $ [C.exp|unsigned int { hb_set_get_population($set:s) }|] <&> fromIntegral

set_has :: PrimMonad m => Set (PrimState m) -> Codepoint -> m Bool
set_has s c = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_set_has($set:s,$(hb_codepoint_t c)) }|] <&> cbool

set_intersect :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m ()
set_intersect s other = unsafeIOToPrim [C.block|void { hb_set_intersect($set:s,$set:other); }|]

set_is_empty :: PrimMonad m => Set (PrimState m) -> m Bool
set_is_empty s = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_set_is_empty($set:s) }|] <&> cbool

set_is_equal :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m Bool
set_is_equal s t = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_set_is_equal($set:s,$set:t) }|] <&> cbool

set_is_subset :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m Bool
set_is_subset s t = unsafeIOToPrim $ [C.exp|hb_bool_t { hb_set_is_subset($set:s,$set:t) }|] <&> cbool

-- | Start with SET_VALUE_INVALID
set_next :: PrimMonad m => Set (PrimState m) -> Codepoint -> m (Maybe Codepoint)
set_next s c = unsafeIOToPrim $ with c $ \p -> do
  b <- [C.exp|hb_bool_t { hb_set_next($set:s,$(hb_codepoint_t * p)) }|]
  if cbool b then Just <$> peek p else pure Nothing

-- | Start with SET_VALUE_INVALID
set_next_range :: PrimMonad m => Set (PrimState m) -> Codepoint -> m (Maybe (Codepoint, Codepoint))
set_next_range s c = unsafeIOToPrim $ allocaArray 2 $ \p -> do
  let q = advancePtr p 1
  poke q c
  b <- [C.exp|hb_bool_t { hb_set_next_range($set:s,$(hb_codepoint_t * p),$(hb_codepoint_t * q)) }|]
  if cbool b
  then do
    lo <- peek p
    hi <- peek q
    pure $ Just (lo,hi)
  else pure Nothing

-- | Start with SET_VALUE_INVALID
set_previous :: PrimMonad m => Set (PrimState m) -> Codepoint -> m (Maybe Codepoint)
set_previous s c = unsafeIOToPrim $ with c $ \p -> do
  b <- [C.exp|hb_bool_t { hb_set_previous($set:s,$(hb_codepoint_t * p)) }|]
  if cbool b then Just <$> peek p else pure Nothing

-- | Start with SET_VALUE_INVALID
set_previous_range :: PrimMonad m => Set (PrimState m) -> Codepoint -> m (Maybe (Codepoint, Codepoint))
set_previous_range s c = unsafeIOToPrim $ allocaArray 2 $ \p -> do
  let q = advancePtr p 1
  poke p c
  b <- [C.exp|hb_bool_t { hb_set_previous_range($set:s,$(hb_codepoint_t * p),$(hb_codepoint_t * q)) }|]
  if cbool b
  then do
    lo <- peek p
    hi <- peek q
    pure $ Just (lo,hi)
  else pure Nothing

set_set :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m ()
set_set s t = unsafeIOToPrim [C.block|void { hb_set_set($set:s,$set:t); }|]

set_subtract :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m ()
set_subtract s other = unsafeIOToPrim [C.block|void { hb_set_subtract($set:s,$set:other); }|]

set_symmetric_difference :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m ()
set_symmetric_difference s other = unsafeIOToPrim [C.block|void { hb_set_symmetric_difference($set:s,$set:other); }|]

set_union :: PrimMonad m => Set (PrimState m) -> Set (PrimState m) -> m ()
set_union s other = unsafeIOToPrim [C.block|void { hb_set_union($set:s,$set:other); }|]
