{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
-- |
-- Copyright :  (c) 2019 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- skyline packing using @stb_rect_pack.h@
module Data.Atlas
( Atlas
, atlas_create
, atlas_create_explicit
, atlas_reset
-- * Setup
, Heuristic(..)
, atlas_set_heuristic
, atlas_set_allow_out_of_mem
-- * Using a context
, Pt(..)
, atlas_pack
, atlas_pack1
) where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Atlas.Internal
import Data.Maybe (fromMaybe)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Language.C.Inline as C

C.context $ C.baseCtx <> C.fptrCtx <> atlasCtx
C.verbatim "#define STB_RECT_PACK_IMPLEMENTATION"
C.include "stb_rect_pack.h"

-- | Create a packing context.

atlas_create :: PrimMonad m => Int -> Int -> m (Atlas (PrimState m))
atlas_create w h = atlas_create_explicit w h Nothing

-- | Initialization with an optional node count, when @node count < width@ is used this results in quantization unless
-- 'atlas_set_allow_out_of_mem' is enabled. When no value is supplied, it defaults to the width of the 'Atlas'.
atlas_create_explicit :: PrimMonad m => Int -> Int -> Maybe Int -> m (Atlas (PrimState m))
atlas_create_explicit width@(fromIntegral -> w) height@(fromIntegral -> h) mn = unsafeIOToPrim $ do
  let nodes@(fromIntegral -> n) = fromMaybe width mn
  unless (width < 0xffff && height < 0xffff) $ fail $ "Atlas.new " ++ show width ++ " " ++ show height ++ ": atlas too large"
  fp <- mallocForeignPtrBytes (sizeOfAtlas + sizeOfNode * (2 + nodes)) -- i don't trust it not to overrun based on segfaults
  Atlas fp <$ [C.block|void {
    stbrp_context * p = $atlas:fp;
    stbrp_init_target(p,$(int w),$(int h),(stbrp_node*)(p+sizeof(stbrp_context)),$(int n));
  }|]

-- | Reinitialize an atlas with the same parameters
atlas_reset :: PrimMonad m => Atlas (PrimState m) -> m ()
atlas_reset atlas = unsafeIOToPrim
  [C.block| void {
    stbrp_context * p = $atlas:atlas;
    int heuristic = p->heuristic;
    int align = p->align;
    stbrp_init_target(p,p->width,p->height,(stbrp_node*)(p+sizeof(stbrp_context)),p->num_nodes);
    p->heuristic = heuristic;
    p->align = align;
  }|]

atlas_set_heuristic :: PrimMonad m => Atlas (PrimState m) -> Heuristic -> m ()
atlas_set_heuristic fp (heuristicId -> h) = unsafeIOToPrim [C.block|void { stbrp_setup_heuristic($atlas:fp,$(int h)); }|]

atlas_set_allow_out_of_mem :: PrimMonad m => Atlas (PrimState m) -> Bool -> m ()
atlas_set_allow_out_of_mem fp (fromIntegral . fromEnum -> b) = unsafeIOToPrim [C.block|void { stbrp_setup_allow_out_of_mem($atlas:fp,$(int b)); }|]

atlas_pack
  :: (PrimBase m, Traversable f)
  => Atlas (PrimState m)
  -> (a -> m Pt) -- extract extents
  -> (a -> Maybe Pt -> b) -- report a mix of successful and unsuccessful packings
  -> (a -> Pt -> c) -- report uniformly successful packings
  -> f a
  -> m (Either (f b) (f c))
atlas_pack fc f g h as = unsafeIOToPrim $ do
  let n = length as
  let cn = fromIntegral n
  allocaBytes (n*sizeOfRect) $ \ rs -> do
    iforOf_ folded as $ \i a -> do
      p <- unsafePrimToIO $ f a
      pokeWH (plusPtr rs (i*sizeOfRect)) p
    res <- [C.exp|int { stbrp_pack_rects($atlas:fc,$(stbrp_rect *rs),$(int cn)) }|]
    if res == 0
    then Left  <$> evalStateT (traverse (go peekMaybeXY g) as) rs -- partial
    else Right <$> evalStateT (traverse (go peekXY h) as) rs -- all allocated
  where
    go :: (Ptr Rect -> IO u) -> (a -> u -> d) -> a -> StateT (Ptr Rect) IO d
    go k gh a = StateT $ \p -> (\b -> (,) (gh a b) $! plusPtr p sizeOfRect) <$> k p
    {-# inline go #-}

atlas_pack1 :: PrimMonad m => Atlas (PrimState m) -> Pt -> m (Maybe Pt)
atlas_pack1 atlas p = stToPrim $ either runIdentity runIdentity <$> atlas_pack atlas pure (const id) (const Just) (Identity p)
