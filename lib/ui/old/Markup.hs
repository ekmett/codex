-- | Bindings for markup, designed to be imported qualified
{-# LANGUAGE RecordWildCards #-}
module UI.Text.Markup
    ( Markup(..), def
    , Color(..)
    , withMarkupPtr
    ) where

import qualified Bindings.FreetypeGL.Markup as Markup
import Bindings.FreetypeGL.Vec234 (C'vec4(..))
import Data.Default
import Data.Function ((&))
import Data.Maybe (fromMaybe, isJust)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import UI.Text.Color (Color(..))
import qualified UI.Text.Color as Color
import UI.Text.Font (Font(..))
import qualified UI.Text.Font as Font

-- We intentionally omit the family,size,bold,italic fields which
-- really don't belong in Markup, and are used to select a font. We
-- prefer to load fonts via file paths directly.

data Markup = Markup
  { spacing :: !Float
  , gamma :: !Float
  , foregroundColor :: !Color
  , backgroundColor :: !Color
  , outlineColor :: !(Maybe Color)
  , underlineColor :: !(Maybe Color)
  , overlineColor :: !(Maybe Color)
  , strikethroughColor :: !(Maybe Color)
  }

instance Default Markup where
  def = Markup
    { spacing = 0.0
    , gamma = 1.0
    , foregroundColor = white
    , backgroundColor = def
    , outlineColor = Nothing
    , underlineColor = Nothing
    , overlineColor = Nothing
    , strikethroughColor = Nothing
    }
  
white :: Color
white = Color 1.0 1.0 1.0 1.0

withMarkupPtr :: Markup -> Font -> (Ptr Markup.C'markup_t -> IO a) -> IO a
withMarkupPtr Markup{..} font@(Font fontPtr) act = alloca $ \ptr -> do
  size <- Font.size font
  poke ptr $ Markup.C'markup_t
    -- family,bold,italic unneeded, we always give a font:
    nullPtr (realToFrac size) 0 0
    (realToFrac spacing)
    (realToFrac gamma)
    (Color.toC'vec4 foregroundColor)
    (Color.toC'vec4 backgroundColor)
    & passColorAsCParams outlineColor
    & passColorAsCParams underlineColor
    & passColorAsCParams overlineColor
    & passColorAsCParams strikethroughColor
    & ($ fontPtr)
  act ptr

passColorAsCParams :: Maybe Color -> (CInt -> C'vec4 -> a) -> a
passColorAsCParams mColor f =
  f
  ((fromIntegral . fromEnum . isJust) mColor)
  (Color.toC'vec4 (fromMaybe def mColor))
