{-# language PatternSynonyms #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures -Wno-incomplete-patterns #-}
-- | This implements a pretty standard game-like
-- opengl state caching layer.
module Graphics.Glow.State
( StateBits
, DepthFunc(..), depthFunc
, BlendFunc(..), srcBlend, dstBlend
, polygonModeLine
, redMask, greenMask, blueMask, alphaMask, depthMask
, glState
) where

import Control.Monad
import Control.Lens hiding (enum)
import Data.Bits
import Data.Bool
import Data.IORef
import Data.StateVar
import GHC.Arr (Ix)
import Graphics.GL
import System.IO.Unsafe

newtype StateBits = StateBits { getStateBits :: Int }
  deriving (Eq,Ord,Show,Bits)
 
glowStateBits :: IORef StateBits
glowStateBits = unsafePerformIO $ newIORef $ StateBits 0
{-# noinline glowStateBits #-}

pattern DEPTHFUNC_MASK   = 0x00000003
pattern DEPTHFUNC_EQUAL  = 0x00000000
pattern DEPTHFUNC_ALWAYS = 0x00000001
pattern DEPTHFUNC_LEQUAL = 0x00000002
pattern DEPTHFUNC_GEQUAL = 0x00000003
pattern DEPTH_MASK    = 0x4
pattern POLYGON_MODE_LINE = 0x8
pattern BLEND_MASK = 0x770
pattern SRCBLEND_MASK = 0x70
pattern SRCBLEND_ZERO = 0x00
pattern SRCBLEND_ONE  = 0x10
pattern SRCBLEND_DST_COLOR = 0x20
pattern SRCBLEND_ONE_MINUS_DST_COLOR = 0x30
pattern SRCBLEND_SRC_ALPHA = 0x40
pattern SRCBLEND_ONE_MINUS_SRC_ALPHA = 0x50
pattern SRCBLEND_DST_ALPHA = 0x60
pattern SRCBLEND_ONE_MINUS_DST_ALPHA = 0x70
--pattern UNUSED_MASK 0x80
pattern DSTBLEND_MASK = 0x700
pattern DSTBLEND_ZERO = 0x000
pattern DSTBLEND_ONE  = 0x100
pattern DSTBLEND_DST_COLOR = 0x200
pattern DSTBLEND_ONE_MINUS_DST_COLOR = 0x300
pattern DSTBLEND_SRC_ALPHA = 0x400
pattern DSTBLEND_ONE_MINUS_SRC_ALPHA = 0x500
pattern DSTBLEND_DST_ALPHA = 0x600
pattern DSTBLEND_ONE_MINUS_DST_ALPHA = 0x700
--pattern UNUSED_MASK 0x800
pattern COLOR_MASK = 0xf000
pattern RED_MASK   = 0x1000
pattern BLUE_MASK  = 0x2000
pattern GREEN_MASK = 0x4000
pattern ALPHA_MASK = 0x8000

data DepthFunc
  = DepthFuncEqual
  | DepthFuncAlways
  | DepthFuncLequal
  | DepthFuncGEqual
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

depthFunc :: Lens' StateBits DepthFunc
depthFunc f (StateBits s) = f (toEnum $ s .&. DEPTHFUNC_MASK) <&> 
  \e -> StateBits $ (s .&. complement DEPTHFUNC_MASK) .|. fromEnum e

enum :: Enum a => Int -> Int -> Lens' StateBits a
enum mask i f (StateBits s) = f (toEnum $ unsafeShiftR (s .&. mask) i) <&>
  \e -> StateBits $ (s .&. complement mask) .|. unsafeShiftL (fromEnum e) i

bitmask :: Int -> Lens' StateBits Bool
bitmask i f (StateBits s) = f (s .&. i /= 0) <&> StateBits . bool (s .&. complement i) (s .|. i) 

data BlendFunc
  = BlendZero
  | BlendOne
  | BlendDstColor
  | BlendOneMinusDstColor
  | BlendAlpha
  | BlendOneMinusAlpha
  | BlendDstAlpha
  | BlendOneMinusDstAlpha
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

srcBlend :: Lens' StateBits BlendFunc
srcBlend = enum SRCBLEND_MASK 4
{-# inline srcBlend #-}

dstBlend :: Lens' StateBits BlendFunc
dstBlend = enum DSTBLEND_MASK 8
{-# inline dstBlend #-}

depthMask :: Lens' StateBits Bool
depthMask = bitmask DEPTH_MASK
{-# inline depthMask #-}

redMask, greenMask, blueMask, alphaMask :: Lens' StateBits Bool
redMask = bitmask RED_MASK
{-# inline redMask #-}
blueMask = bitmask BLUE_MASK
{-# inline blueMask #-}
greenMask = bitmask GREEN_MASK
{-# inline greenMask #-}
alphaMask = bitmask ALPHA_MASK
{-# inline alphaMask #-}

polygonModeLine :: Lens' StateBits Bool
polygonModeLine = bitmask POLYGON_MODE_LINE
{-# iNline polygonModeLine #-}

glState :: StateVar StateBits
glState = StateVar (readIORef glowStateBits) $ \(StateBits stateBits) -> do
  StateBits currentStateBits <- readIORef glowStateBits

  let diff = xor stateBits currentStateBits
      flag mask = stateBits .&. mask /= 0
      glflag mask = bool GL_FALSE GL_TRUE (flag mask)
      changed mask = diff .&. mask /= 0

  unless (diff == 0) $ do
    when (changed DEPTHFUNC_MASK) $
      case stateBits .&. DEPTHFUNC_MASK of
        DEPTHFUNC_EQUAL  -> glDepthFunc GL_EQUAL
        DEPTHFUNC_ALWAYS -> glDepthFunc GL_ALWAYS
        DEPTHFUNC_LEQUAL -> glDepthFunc GL_LEQUAL
        DEPTHFUNC_GEQUAL -> glDepthFunc GL_GEQUAL
    when (changed $ BLEND_MASK) $ let
      srcFactor = case stateBits .&. SRCBLEND_MASK of
        SRCBLEND_ZERO -> GL_ZERO
        SRCBLEND_ONE -> GL_ONE
        SRCBLEND_DST_COLOR -> GL_DST_COLOR
        SRCBLEND_ONE_MINUS_DST_COLOR -> GL_ONE_MINUS_DST_COLOR
        SRCBLEND_SRC_ALPHA -> GL_SRC_ALPHA
        SRCBLEND_ONE_MINUS_SRC_ALPHA -> GL_ONE_MINUS_SRC_ALPHA
        SRCBLEND_DST_ALPHA -> GL_DST_ALPHA
        SRCBLEND_ONE_MINUS_DST_ALPHA -> GL_ONE_MINUS_DST_ALPHA
      dstFactor = case stateBits .&. DSTBLEND_MASK of
        DSTBLEND_ZERO -> GL_ZERO
        DSTBLEND_ONE -> GL_ONE
        DSTBLEND_DST_COLOR -> GL_DST_COLOR
        DSTBLEND_ONE_MINUS_DST_COLOR -> GL_ONE_MINUS_DST_COLOR
        DSTBLEND_SRC_ALPHA -> GL_SRC_ALPHA
        DSTBLEND_ONE_MINUS_SRC_ALPHA -> GL_ONE_MINUS_SRC_ALPHA
        DSTBLEND_DST_ALPHA -> GL_DST_ALPHA
        DSTBLEND_ONE_MINUS_DST_ALPHA -> GL_ONE_MINUS_DST_ALPHA
      in if srcFactor == GL_ONE && dstFactor == GL_ZERO then glDisable GL_BLEND
      else glEnable GL_BLEND *> glBlendFunc srcFactor dstFactor
    when (changed DEPTH_MASK) $ glDepthMask (glflag DEPTH_MASK)
    when (changed COLOR_MASK) $ glColorMask (glflag RED_MASK) (glflag GREEN_MASK) (glflag BLUE_MASK) (glflag ALPHA_MASK)
    when (changed POLYGON_MODE_LINE) $ glPolygonMode GL_FRONT_AND_BACK $ bool GL_FILL GL_LINE $ flag POLYGON_MODE_LINE
    
