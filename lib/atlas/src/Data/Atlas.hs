{-# language TypeOperators #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

-- | skyline packing using @stb_rect_pack.h@
module Data.Atlas
  ( Atlas
  , new, new_
  -- * Setup
  , heuristic, Heuristic(..)
  , allowOOM
  -- * Using a context
  , pack
  , Pt(..), HasPt(..)
  , Box(..), HasBox(..)

  -- convenience
  , boxy
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans.State.Strict
import Data.Atlas.Raw
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- | create a packing context with an optional node count, when @node count < width@ is used this results in quantization unless allowOOM is set
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

heuristic :: PrimMonad m => Atlas (PrimState m) -> Heuristic -> m ()
heuristic fp h = unsafeIOToPrim $ withAtlas fp $ \p -> stbrp_setup_heuristic p (heuristicId h)

allowOOM :: PrimMonad m => Atlas (PrimState m) -> Bool -> m ()
allowOOM fp b = unsafeIOToPrim $ withAtlas fp $ \p -> stbrp_setup_allow_out_of_mem p $ fromIntegral $ fromEnum b

peekBox :: (Ptr Rect -> IO (f Pt)) -> Ptr Rect -> IO (Box f)
peekBox k p = Box <$> k p <*> peekWH p
{-# inline peekBox #-}

boxy :: Pt -> Box Proxy
boxy = Box Proxy

boxId :: Ptr Rect -> IO (Identity Pt)
boxId = fmap Identity . peekXY

pack
  :: (PrimMonad m, Traversable f)
  => Atlas (PrimState m)
  -> (a -> Box t)             -- extract extents
  -> (a -> Box Maybe -> b)    -- report a mix of successful and unsuccessful packings
  -> (a -> Box Identity -> c) -- report uniformly successful packings
  -> f a
  -> m (Either (f b) (f c))
pack fc f g h as = unsafeIOToPrim $ do
    let n = length as
    allocaBytes (n*sizeOfRect) $ \ rs -> do
      pokePts f rs (toList as)
      withAtlas fc $ \c ->
        stbrp_pack_rects c rs (fromIntegral n) >>= \res -> if res == 0
          then Left  <$> evalStateT (traverse (go boxMaybe g) as) rs -- partial
          else Right <$> evalStateT (traverse (go boxId h) as) rs -- all allocated
  where
    go :: (Ptr Rect -> IO (u Pt)) -> (a -> Box u -> d) -> a -> StateT (Ptr Rect) IO d
    go k gh a = StateT $ \p -> (\b -> (,) (gh a b) $! plusPtr p sizeOfRect) <$> peekBox k p
    {-# inline go #-}
