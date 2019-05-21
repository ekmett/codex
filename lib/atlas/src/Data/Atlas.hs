{-# language BangPatterns #-}
{-# language ForeignFunctionInterface #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# language ViewPatterns #-}
{-# language TypeOperators #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language TemplateHaskell #-}
{-# language FlexibleInstances #-}

-- | skyline packing using @stb_rect_pack.h@
module Data.Atlas
  ( Atlas
  , new, new_
  , delete
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
import Foreign.Marshal.Alloc
import Foreign.Ptr

-- create a packing context with a fixed node count, when node count < width is used, this results in quantization unless allowOOM is set
new :: PrimMonad m => Int -> Int -> Maybe Int -> m (Atlas (PrimState m))
new width height mn = unsafeIOToPrim $ do
  let nodes = fromMaybe width mn
  unless (width < 0xffff && height < 0xffff) $ fail $ "Atlas.new " ++ show width ++ show height ++ ": atlas too large"
  c <- mallocBytes (sizeOfAtlas + sizeOfNode * nodes)
  let n = plusPtr c sizeOfAtlas
  Atlas c <$ stbrp_init_target (Atlas c) (fromIntegral width) (fromIntegral height) n (fromIntegral nodes)

new_ :: PrimMonad m => Int -> Int -> m (Atlas (PrimState m))
new_ w h = new w h Nothing

delete :: PrimMonad m => Atlas (PrimState m) -> m ()
delete (Atlas c) = unsafeIOToPrim $ free c

heuristic :: PrimMonad m => Atlas (PrimState m) -> Heuristic -> m ()
heuristic c h = unsafeIOToPrim $ stbrp_setup_heuristic c (heuristicId h)

allowOOM :: PrimMonad m => Atlas (PrimState m) -> Bool -> m ()
allowOOM c b = unsafeIOToPrim $ stbrp_setup_allow_out_of_mem c $ fromIntegral $ fromEnum b

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
pack c f g h as = unsafeIOToPrim $ do
    let n = length as
    allocaBytes (n*sizeOfRect) $ \ rs -> do
      pokePts f rs (toList as)
      stbrp_pack_rects c rs (fromIntegral n) >>= \res -> if res == 0
        then Left  <$> evalStateT (traverse (go boxMaybe g) as) rs -- partial
        else Right <$> evalStateT (traverse (go boxId h) as) rs -- all allocated
  where
    go :: (Ptr Rect -> IO (u Pt)) -> (a -> Box u -> d) -> a -> StateT (Ptr Rect) IO d
    go k gh a = StateT $ \p -> (\b -> (,) (gh a b) $! plusPtr p sizeOfRect) <$> peekBox k p
    {-# inline go #-}
