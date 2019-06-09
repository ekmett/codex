{-# language PatternSynonyms #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RankNTypes #-}
{-# language LambdaCase #-}
{-# options_ghc -Wno-missing-pattern-synonym-signatures -Wno-incomplete-patterns #-}
-- | This implements a pretty standard game-like
-- opengl state caching layer.
module Graphics.Glow.State
( StateBits
, DepthFunc(..), depthFunc
, BlendFunc(..), srcBlendFuncRGB, dstBlendFuncRGB, srcBlendFuncAlpha, dstBlendFuncAlpha
, BlendEquation(..), blendEquationRGB, blendEquationAlpha
, StencilFunc(..), stencilFunc, stencilFuncRef, stencilFuncMask
, StencilOp(..), stencilOpFail, stencilOpZFail, stencilOpPass, forceStencilOpState
, polygonModeLine
, redMask, greenMask, blueMask, alphaMask, depthMask
, forceState
, stateBits -- toggle common features listed here with caching
, setCommonState -- enable stencil, etc.
-- for debugging
, printStateBits
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens hiding (enum)
import Data.Bits
import Data.Bool
import Data.Default
import Data.IORef
import Data.StateVar
import Data.Word
import GHC.Arr (Ix)
import Graphics.GL
import System.IO.Unsafe

newtype StateBits = StateBits { getStateBits :: Word64 }
  deriving (Eq,Ord,Show,Bits)

instance Default StateBits where
  def = StateBits DEFAULT_BITS -- not 0

glowStateBits :: IORef StateBits
glowStateBits = unsafePerformIO $ newIORef def
{-# noinline glowStateBits #-}

pattern DEPTHFUNC_MASK               = 0x0000000000000003
pattern DEPTH_MASK                   = 0x0000000000000004
pattern POLYGON_MODE_LINE            = 0x0000000000000008
---------------------------------------------------------
pattern BLEND_EQUATION_MASK          = 0x0000000000000770
pattern BLEND_EQUATION_RGB_MASK      = 0x0000000000000070; pattern BLEND_EQUATION_RGB_SHIFT = 4
pattern FORCE_STENCIL_OP_STATE       = 0x0000000000000080 -- e.g. in case you want to manually use 'glStencilOpSeparate'
pattern BLEND_EQUATION_ALPHA_MASK    = 0x0000000000000700; pattern BLEND_EQUATION_ALPHA_SHIFT = 8
pattern FORCE_STATE                  = 0x0000000000000800 -- if this bit is set, we'll forcibly reset the state
---------------------------------------------------------
pattern COLOR_MASK                   = 0x000000000000f000
pattern RED_MASK                     = 0x0000000000001000
pattern BLUE_MASK                    = 0x0000000000002000
pattern GREEN_MASK                   = 0x0000000000004000
pattern ALPHA_MASK                   = 0x0000000000008000
---------------------------------------------------------
pattern STENCIL_FUNC_FIELDS_MASK     = 0x00000007ffff0000
pattern STENCIL_FUNC_MASK            = 0x0000000000070000; pattern STENCIL_FUNC_SHIFT = 16
pattern STENCIL_FUNC_REF_MASK        = 0x0000000007f80000; pattern STENCIL_FUNC_REF_SHIFT = 19
pattern STENCIL_FUNC_MASK_MASK       = 0x00000007f8000000; pattern STENCIL_FUNC_MASK_SHIFT = 27
---------------------------------------------------------
pattern STENCIL_OP_MASK              = 0x00000ff800000080 -- note inclusion of FORCE_STENCIL_OP_STATE
pattern STENCIL_OP_FAIL_MASK         = 0x0000003800000000; pattern STENCIL_OP_FAIL_SHIFT  = 35
pattern STENCIL_OP_ZFAIL_MASK        = 0x000001c000000000; pattern STENCIL_OP_ZFAIL_SHIFT = 38
pattern STENCIL_OP_PASS_MASK         = 0x00000e0000000000; pattern STENCIL_OP_PASS_SHIFT  = 41
---------------------------------------------------------
pattern BLEND_MASK                   = 0xfffff00000000000
pattern BLEND_SRC_RGB_MASK           = 0x0001f00000000000; pattern BLEND_SRC_RGB_SHIFT = 44
pattern BLEND_DST_RGB_MASK           = 0x003e000000000000; pattern BLEND_DST_RGB_SHIFT = 49
pattern BLEND_SRC_ALPHA_MASK         = 0x07c0000000000000; pattern BLEND_SRC_ALPHA_SHIFT = 54
pattern BLEND_DST_ALPHA_MASK         = 0xf800000000000000; pattern BLEND_DST_ALPHA_SHIFT = 59
pattern DEFAULT_BITS                 = 0x0002100000000000 -- makes GL_ONE the default for SRCBLEND*

data DepthFunc
  = DepthFuncLEqual
  | DepthFuncEqual
  | DepthFuncAlways
  | DepthFuncGEqual
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

class AsGLenum t where
  glenum :: t -> GLenum

instance AsGLenum DepthFunc where
  glenum = \case
    DepthFuncLEqual -> GL_LEQUAL
    DepthFuncEqual  -> GL_EQUAL
    DepthFuncAlways -> GL_ALWAYS
    DepthFuncGEqual -> GL_GEQUAL

instance Default DepthFunc where
  def = DepthFuncLEqual

depthFunc :: Lens' StateBits DepthFunc
depthFunc f (StateBits s) = f (toEnum $ fromIntegral $ s .&. DEPTHFUNC_MASK) <&>
  \e -> StateBits $ (s .&. complement DEPTHFUNC_MASK) .|. fromIntegral (fromEnum e)

data StencilFunc
  = StencilFuncAlways
  | StencilFuncLess
  | StencilFuncEqual
  | StencilFuncLEqual
  | StencilFuncGreater
  | StencilFuncNotEqual
  | StencilFuncGEqual
  | StencilFuncNever
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

instance AsGLenum StencilFunc where
  glenum = \case
    StencilFuncAlways -> GL_ALWAYS
    StencilFuncLess -> GL_LESS
    StencilFuncEqual -> GL_EQUAL
    StencilFuncLEqual -> GL_LEQUAL
    StencilFuncGreater -> GL_GREATER
    StencilFuncNotEqual -> GL_NOTEQUAL
    StencilFuncGEqual -> GL_GEQUAL
    StencilFuncNever -> GL_NEVER

instance Default StencilFunc where
  def = StencilFuncAlways

stencilFunc :: Lens' StateBits StencilFunc
stencilFunc = enum STENCIL_FUNC_MASK STENCIL_FUNC_SHIFT

stencilFuncRef, stencilFuncMask :: Lens' StateBits Word8
stencilFuncRef = enum STENCIL_FUNC_REF_MASK STENCIL_FUNC_REF_SHIFT
stencilFuncMask = enum STENCIL_FUNC_MASK_MASK STENCIL_FUNC_MASK_SHIFT

-- |
-- To use a 'glStencilOpSeparate' in your local state you probably want something like
--
-- @
-- myState = def
--   & forceStencilState .~ True -- and set up the stencil parameters you want to have pre-loadedd
--   & stencilOpFail .~ StencilOpKeep
--   & stencilOpZFail .~ StencilOpZero
--   & stencilOpPass .~ StencilOpReplace
-- ...
--
-- glState $= myState -- which calls glStencilOp ... setting both front and back
-- glStencilOpSeparate GL_BACK GL_ZERO GL_KEEP GL_REPLACE
--
-- glState $= someOtherState -- will now reset both stencils
-- @

data StencilOp
  = StencilOpKeep
  | StencilOpZero
  | StencilOpReplace
  | StencilOpIncr
  | StencilOpDecr
  | StencilOpInvert
  | StencilOpIncrWrap
  | StencilOpDecrWrap
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

instance AsGLenum StencilOp where
  glenum = \case
    StencilOpKeep -> GL_KEEP
    StencilOpZero -> GL_ZERO
    StencilOpReplace -> GL_REPLACE
    StencilOpIncr -> GL_INCR
    StencilOpDecr -> GL_DECR
    StencilOpInvert -> GL_INVERT
    StencilOpIncrWrap -> GL_INCR_WRAP
    StencilOpDecrWrap -> GL_DECR_WRAP

instance Default StencilOp where
  def = StencilOpKeep

stencilOpPass, stencilOpFail, stencilOpZFail :: Lens' StateBits StencilOp
stencilOpPass  = enum STENCIL_OP_PASS_MASK STENCIL_OP_PASS_SHIFT
stencilOpFail  = enum STENCIL_OP_FAIL_MASK STENCIL_OP_FAIL_SHIFT
stencilOpZFail = enum STENCIL_OP_ZFAIL_MASK STENCIL_OP_ZFAIL_SHIFT

enum :: Enum a => Word64 -> Int -> Lens' StateBits a
enum mask i f (StateBits s) = f (toEnum $ fromIntegral $ unsafeShiftR (s .&. mask) i) <&>
  \e -> StateBits $ (s .&. complement mask) .|. unsafeShiftL (fromIntegral $ fromEnum e) i

bitmask :: Word64 -> Lens' StateBits Bool
bitmask i f (StateBits s) = f (s .&. i /= 0) <&> StateBits . bool (s .&. complement i) (s .|. i)

data BlendFunc
  = BlendZero
  | BlendOne
  | BlendSrcColor
  | BlendOneMinusSrcColor
  | BlendDstColor
  | BlendOneMinusDstColor
  | BlendSrcAlpha
  | BlendOneMinusSrcAlpha
  | BlendDstAlpha
  | BlendOneMinusDstAlpha
  | BlendConstantColor
  | BlendOneMinusConstantColor
  | BlendConstantAlpha
  | BlendOneMinusConstantAlpha
  | BlendSrcAlphaSaturate
  -- dual source blending
  | BlendSrc1Color
  | BlendOneMinusSrc1Color
  | BlendSrc1Alpha
  | BlendOneMinusSrc1Alpha
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

instance AsGLenum BlendFunc where
  glenum = \case
    BlendZero -> GL_ZERO
    BlendOne -> GL_ONE
    BlendSrcColor -> GL_SRC_COLOR
    BlendOneMinusSrcColor -> GL_ONE_MINUS_SRC_COLOR
    BlendDstColor -> GL_DST_COLOR
    BlendOneMinusDstColor -> GL_ONE_MINUS_DST_COLOR
    BlendSrcAlpha -> GL_SRC_ALPHA
    BlendOneMinusSrcAlpha -> GL_ONE_MINUS_SRC_ALPHA
    BlendDstAlpha -> GL_DST_ALPHA
    BlendOneMinusDstAlpha -> GL_ONE_MINUS_DST_ALPHA
    BlendConstantColor -> GL_CONSTANT_COLOR
    BlendOneMinusConstantColor -> GL_ONE_MINUS_CONSTANT_COLOR
    BlendConstantAlpha -> GL_CONSTANT_ALPHA
    BlendOneMinusConstantAlpha -> GL_ONE_MINUS_CONSTANT_ALPHA
    BlendSrcAlphaSaturate -> GL_SRC_ALPHA_SATURATE
    BlendSrc1Color -> GL_SRC1_COLOR
    BlendOneMinusSrc1Color -> GL_ONE_MINUS_SRC1_COLOR
    BlendSrc1Alpha -> GL_SRC1_ALPHA
    BlendOneMinusSrc1Alpha -> GL_ONE_MINUS_SRC1_ALPHA

-- | glBlendFuncSeparate srcBlendFuncRGB dstBlendFuncRGB srcBlendFuncAlpha dstBlendFuncAlpha
srcBlendFuncRGB, dstBlendFuncRGB, srcBlendFuncAlpha, dstBlendFuncAlpha :: Lens' StateBits BlendFunc
-- initially BlendOne
srcBlendFuncRGB = enum BLEND_SRC_RGB_MASK BLEND_SRC_RGB_SHIFT
{-# inline srcBlendFuncRGB #-}
-- initially BlendZero
dstBlendFuncRGB = enum BLEND_DST_RGB_MASK BLEND_DST_RGB_SHIFT
{-# inline dstBlendFuncRGB #-}
-- initially BlendOne
srcBlendFuncAlpha = enum BLEND_SRC_ALPHA_MASK BLEND_SRC_ALPHA_SHIFT
{-# inline srcBlendFuncAlpha #-}
-- initially BlendZero
dstBlendFuncAlpha = enum BLEND_DST_ALPHA_MASK BLEND_DST_ALPHA_SHIFT
{-# inline dstBlendFuncAlpha #-}

data BlendEquation
  = BlendEquationAdd
  | BlendEquationSubtract
  | BlendEquationReverseSubtract
  | BlendEquationMin
  | BlendEquationMax
  deriving (Eq,Ord,Show,Enum,Ix,Bounded)

instance AsGLenum BlendEquation where
  glenum = \case
    BlendEquationAdd -> GL_FUNC_ADD
    BlendEquationSubtract -> GL_FUNC_SUBTRACT
    BlendEquationReverseSubtract -> GL_FUNC_REVERSE_SUBTRACT
    BlendEquationMin -> GL_MIN
    BlendEquationMax -> GL_MAX

instance Default BlendEquation where
  def = BlendEquationAdd

-- | Initially 'BlendEquationAdd'
blendEquationRGB :: Lens' StateBits BlendEquation
blendEquationRGB = enum BLEND_EQUATION_RGB_MASK BLEND_EQUATION_RGB_SHIFT
{-# inline blendEquationRGB #-}

-- | Initially 'BlendEquationAdd'
blendEquationAlpha :: Lens' StateBits BlendEquation
blendEquationAlpha = enum BLEND_EQUATION_ALPHA_MASK BLEND_EQUATION_ALPHA_SHIFT
{-# inline blendEquationAlpha #-}

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

forceStencilOpState :: Lens' StateBits Bool
forceStencilOpState = bitmask FORCE_STENCIL_OP_STATE

forceState :: Lens' StateBits Bool
forceState = bitmask FORCE_STATE

printStateBits :: StateBits -> IO ()
printStateBits s = do
  putStrLn $ "depthFunc " ++ show (s^.depthFunc)
  putStrLn $ "blendFuncSeparate "
    ++ show (s^.srcBlendFuncRGB) ++ " "
    ++ show (s^.dstBlendFuncRGB) ++ " "
    ++ show (s^.srcBlendFuncAlpha) ++ " "
    ++ show (s^.dstBlendFuncAlpha)
  putStrLn $ "depthMask " ++ show (s^.depthMask)
  putStrLn $ "redMask " ++ show (s^.redMask)
  putStrLn $ "greenMask " ++ show (s^.greenMask)
  putStrLn $ "blueMask " ++ show (s^.blueMask)
  putStrLn $ "alphaMask " ++ show (s^.alphaMask)
  putStrLn $ "polygonModeLine " ++ show (s^.polygonModeLine)
  putStrLn $ "stencilFunc " ++ show (s^.stencilFunc) ++ " " ++ show (s^.stencilFuncRef) ++ " " ++ show (s^.stencilFuncMask)
  putStrLn $ "stencilOp " ++ show (s^.stencilOpFail) ++ " " ++ show (s^.stencilOpZFail) ++ " " ++ show (s^.stencilOpPass)

stateBits :: StateVar StateBits
stateBits = StateVar (get glowStateBits) $ \s@(StateBits sb) -> do
  StateBits currentStateBits <- get glowStateBits
  glowStateBits $= s

  let diff = xor sb currentStateBits
       .|. if sb .&. FORCE_STATE /= 0 then complement 0 else 0 -- invalidate everything
      flag mask = sb .&. mask /= 0
      glflag mask = bool GL_FALSE GL_TRUE $ flag mask
      changed mask = diff .&. mask /= 0
      field :: (AsGLenum a, Integral b) => Getting b StateBits a -> b
      field l = views l (fromIntegral . glenum) s
      {-# inline field #-}

  unless (diff == 0) $ do
    when (changed DEPTHFUNC_MASK) $ glDepthFunc $ field depthFunc
    when (changed BLEND_MASK) $
      if sb .&. BLEND_MASK == 0 then glDisable GL_BLEND
      else do
        glEnable GL_BLEND
        glBlendFuncSeparate
          (field srcBlendFuncRGB) (field dstBlendFuncRGB)
          (field srcBlendFuncAlpha) (field dstBlendFuncAlpha)
    when (changed BLEND_EQUATION_MASK) $ glBlendEquationSeparate (field blendEquationRGB) (field blendEquationAlpha)
    when (changed DEPTH_MASK) $ glDepthMask (glflag DEPTH_MASK)
    when (changed COLOR_MASK) $ glColorMask (glflag RED_MASK) (glflag GREEN_MASK) (glflag BLUE_MASK) (glflag ALPHA_MASK)
    when (changed POLYGON_MODE_LINE) $ glPolygonMode GL_FRONT_AND_BACK $ bool GL_FILL GL_LINE $ flag POLYGON_MODE_LINE
    when (changed STENCIL_FUNC_FIELDS_MASK) $ glStencilFunc
      (field stencilFunc) (views stencilFuncRef fromIntegral s) (views stencilFuncMask fromIntegral s)
    when (changed STENCIL_OP_MASK) $ glStencilOp (field stencilOpFail) (field stencilOpZFail) (field stencilOpPass)

-- | just stencil for now
setCommonState :: MonadIO m => m ()
setCommonState = glEnable GL_STENCIL_TEST
