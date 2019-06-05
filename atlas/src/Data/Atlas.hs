{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
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
, new
, new_
-- * Setup
, Heuristic(..)
, heuristic
, allowOOM
-- * Using a context
, pack
, pack1
, Pt(..), HasPt(..)
) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Atlas.Internal
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- | Create a packing context with an optional node count, when @node count < width@ is used this results in quantization unless allowOOM is set
-- when no value is supplied, it defaults to the width of the 'Atlas'.
--
-- Uses a 'ForeignPtr' behind the scenes, so there is no memory management to be done, it is handled by the garbage collector
new :: PrimMonad m => Int -> Int -> Maybe Int -> m (Atlas (PrimState m))
new width height mn = unsafeIOToPrim $ do
  let nodes = fromMaybe width mn
  unless (width < 0xffff && height < 0xffff) $ fail $ "Atlas.new " ++ show width ++ show height ++ ": atlas too large"
  fp <- mallocForeignPtrBytes (sizeOfAtlas + sizeOfNode * nodes)
  withForeignPtr fp $ \ p -> stbrp_init_target p (fromIntegral width) (fromIntegral height) (plusPtr p sizeOfAtlas) (fromIntegral nodes)
  pure $ Atlas fp

new_ :: PrimMonad m => Int -> Int -> m (Atlas (PrimState m))
new_ w h = new w h Nothing

--reset :: PrimMonad m => Atlas (PrimState m) -> m ()
--reset atlas = withAtlas fp c_reset_atlas

heuristic :: PrimMonad m => Atlas (PrimState m) -> Heuristic -> m ()
heuristic fp h = unsafeIOToPrim $ withAtlas fp $ \p -> stbrp_setup_heuristic p (heuristicId h)

allowOOM :: PrimMonad m => Atlas (PrimState m) -> Bool -> m ()
allowOOM fp b = unsafeIOToPrim $ withAtlas fp $ \p -> stbrp_setup_allow_out_of_mem p $ fromIntegral $ fromEnum b

pack
  :: (PrimBase m, Traversable f)
  => Atlas (PrimState m)
  -> (a -> m Pt) -- extract extents
  -> (a -> Maybe Pt -> b) -- report a mix of successful and unsuccessful packings
  -> (a -> Pt -> c) -- report uniformly successful packings
  -> f a
  -> m (Either (f b) (f c))
pack fc f g h as = unsafeIOToPrim $ do
    let n = length as
    allocaBytes (n*sizeOfRect) $ \ rs -> do
      ifor as $ \i a -> do
        pt <- unsafePrimToIO $ f a
        pokeWH (plusPtr (i*sizeOfRect)) pt
      withAtlas fc $ \c ->
        stbrp_pack_rects c rs (fromIntegral n) >>= \res -> if res == 0
          then Left  <$> evalStateT (traverse (go peekMaybeXY g) as) rs -- partial
          else Right <$> evalStateT (traverse (go peekXY h) as) rs -- all allocated
  where
    go :: (Ptr Rect -> IO u) -> (a -> u -> d) -> a -> StateT (Ptr Rect) IO d
    go k gh a = StateT $ \p -> (\b -> (,) (gh a b) $! plusPtr p sizeOfRect) <$> k p
    {-# inline go #-}

pack1 :: PrimMonad m => Atlas (PrimState m) -> Pt -> m (Maybe Pt)
pack1 atlas p = either runIdentity runIdentity <$> pack atlas id (const pure ) (const (pure . Just)) (Identity p)
