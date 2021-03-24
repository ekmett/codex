{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}
{-# language BlockArguments #-}
-- |
-- Copyright :  (c) 2019-2021 Edward Kmett
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- skyline packing using @stb_rect_pack.h@
module Data.Atlas
( Atlas
, create
, createExplicit
, reset
-- * Setup
, Heuristic(..)
, setHeuristic
, setAllowOutOfMem
-- * Using a context
, Pt(..)
, pack
, pack1
, packM
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
import GHC.Stack
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.context $ C.baseCtx <> C.fptrCtx <> atlasCtx
C.verbatim "#define STB_RECT_PACK_IMPLEMENTATION"
C.include "stb_rect_pack.h"

-- | Create a packing context.

create :: (HasCallStack, PrimMonad m) => Int -> Int -> m (Atlas (PrimState m))
create w h = createExplicit w h Nothing

-- | Initialization with an optional node count, when @node count < width@ is used this results in quantization unless
-- 'setAllowOutOfMem' is enabled. When no value is supplied, it defaults to the width of the 'Atlas'.
createExplicit :: HasCallStack => PrimMonad m => Int -> Int -> Maybe Int -> m (Atlas (PrimState m))
createExplicit width@(fromIntegral -> w) height@(fromIntegral -> h) mn = withFrozenCallStack $ unsafeIOToPrim do
  let nodes@(fromIntegral -> n) = fromMaybe width mn
  unless (width < 0xffff && height < 0xffff) $ die $ "Atlas.new " ++ show width ++ " " ++ show height ++ ": atlas too large"
  fp <- mallocForeignPtrBytes (sizeOfAtlas + sizeOfNode * nodes)
  Atlas fp <$ [CU.block|void {
      stbrp_context * p = $atlas:fp;
      stbrp_init_target(p,$(int w),$(int h),(stbrp_node *)(p+1),$(int n));
  }|]

-- | Reinitialize an atlas with the same parameters
reset :: PrimMonad m => Atlas (PrimState m) -> m ()
reset atlas = unsafeIOToPrim
  [CU.block| void {
    stbrp_context * p = $atlas:atlas;
    int heuristic = p->heuristic;
    int align = p->align;
    stbrp_init_target(p,p->width,p->height,(stbrp_node*)(p+1),p->num_nodes);
    p->heuristic = heuristic;
    p->align = align;
  }|]

setHeuristic :: PrimMonad m => Atlas (PrimState m) -> Heuristic -> m ()
setHeuristic fp (heuristicId -> h) = unsafeIOToPrim [CU.block|void { stbrp_setup_heuristic($atlas:fp,$(int h)); }|]

setAllowOutOfMem :: PrimMonad m => Atlas (PrimState m) -> Bool -> m ()
setAllowOutOfMem fp (fromIntegral . fromEnum -> b) = unsafeIOToPrim [CU.block|void { stbrp_setup_allow_out_of_mem($atlas:fp,$(int b)); }|]

pack
  :: (PrimMonad m, Traversable f)
  => Atlas (PrimState m)    -- ^ The atlas you want to pack these rectangles into.
  -> (a -> Pt)              -- ^ for each item you want to pack, extract the size.
  -> (Maybe Pt -> a -> b)   -- ^ when some fail to pack this will be called, with
                            -- 'Just' a position for each that succeeded, and
                            -- 'Nothing' for any that it failed on. The successes
                            -- and failures may well be out of order.
  -> (Pt -> a -> c)         -- ^ when all succeed this will be called with each position.
  -> f a                    -- ^ A container full of things that you'd like to pack into the atlas.
  -> m (Either (f b) (f c)) -- ^ Either a mixture of successes and failures, or a successful pack.
pack atlas f g h as = stToPrim $ packM atlas (pure . f) g h as

packM
  :: (PrimBase m, Traversable f)
  => Atlas (PrimState m)    -- ^ The 'Atlas' you want to pack these rectangles into.
  -> (a -> m Pt)            -- ^ for each item you want to pack, extract the size, 
                            --   with effects in @m@
  -> (Maybe Pt -> a -> b)   -- ^ when some fail to pack this will be called, with
                            -- 'Just' a position for each that succeeded, and 
                            -- 'Nothing' for any that it failed on. The successes
                            -- and failures may well be out of order.
  -> (Pt -> a -> c)         -- ^ when all succeed, this will be called with each position.
  -> f a                    -- ^ A container full of things that you'd like to pack into the atlas.
  -> m (Either (f b) (f c)) -- ^ 'Either' a mixture of successes and failures, or a successful pack.
packM fc f g h as = unsafeIOToPrim do
  let n = length as
  let cn = fromIntegral n
  allocaBytes (n*sizeOfRect) \ rs -> do
    iforOf_ folded as \i a -> do
      p <- unsafePrimToIO $ f a
      pokeWH (plusPtr rs (i*sizeOfRect)) p
    res <- [CU.exp|int { stbrp_pack_rects($atlas:fc,$(stbrp_rect *rs),$(int cn)) }|]
    if res == 0
    then Left  <$> evalStateT (traverse (go peekMaybeXY g) as) rs -- partial
    else Right <$> evalStateT (traverse (go peekXY h) as) rs -- all allocated
  where
    go :: (Ptr Rect -> IO u) -> (u -> a -> d) -> a -> StateT (Ptr Rect) IO d
    go k gh a = StateT \p -> (\b -> (,) (gh b a) $! plusPtr p sizeOfRect) <$> k p
    {-# inline go #-}

-- | Add one rectangle to the 'Atlas'. Using 'pack' can yield significantly better
-- packing than calling this one rectangle at a time.
pack1 :: PrimMonad m => Atlas (PrimState m) -> Pt -> m (Maybe Pt)
pack1 atlas p = stToPrim $ either runIdentity runIdentity <$> pack atlas id const (const . Just) (Identity p)
