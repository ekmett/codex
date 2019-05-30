-- | Texture fonts, designed to be imported qualified
module UI.Text.Font
    ( Font(..), newFromFile, newFromMemory, delete
    , RenderMode(..), PtSize, size, OutlineThickness
    , height, lineGap, ascender, descender, underlinePosition, underlineThickness
    ) where

import qualified Bindings.FreetypeGL.TextureFont as TF
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Foreign.C.String (withCString)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (peek, poke)
import           UI.Text.Atlas (Atlas)
import qualified UI.Text.Atlas as Atlas

newtype Font = Font (Ptr TF.C'texture_font_t)

type PtSize = Float
type OutlineThickness = Float

data RenderMode
  = RenderNormal
  | RenderOutlineEdge OutlineThickness
  | RenderOutlinePositive OutlineThickness
  | RenderOutlineNegative OutlineThickness
  | RenderSignedDistanceField
  deriving Show

c'renderMode :: RenderMode -> (TF.C'rendermode_t, OutlineThickness)
c'renderMode RenderNormal = (TF.c'RENDER_NORMAL, 0)
c'renderMode (RenderOutlineEdge x) = (TF.c'RENDER_OUTLINE_EDGE, x)
c'renderMode (RenderOutlinePositive x) = (TF.c'RENDER_OUTLINE_POSITIVE, x)
c'renderMode (RenderOutlineNegative x) = (TF.c'RENDER_OUTLINE_NEGATIVE, x)
c'renderMode RenderSignedDistanceField = (TF.c'RENDER_SIGNED_DISTANCE_FIELD, 0)

setRenderMode :: RenderMode -> Ptr TF.C'texture_font_t -> IO ()
setRenderMode mode ptr = do
  let (cRenderMode, outline) = c'renderMode mode 
  poke (TF.p'texture_font_t'rendermode ptr) cRenderMode
  poke (TF.p'texture_font_t'outline_thickness ptr) (realToFrac outline)

newFromFile :: Atlas -> PtSize -> RenderMode -> FilePath -> IO Font
newFromFile atlas size_ mode path = withCString path $ \cPath -> Font <$> do
  font <- throwIfNull "texture_font_new_from_file failed" $
    TF.c'texture_font_new_from_file (Atlas.ptr atlas) (realToFrac size_) cPath
  font <$ setRenderMode mode font

newFromMemory :: Atlas -> PtSize -> RenderMode -> ByteString -> IO Font
newFromMemory atlas size_ mode mem = BS.useAsCStringLen mem $ \(cStr, len) -> Font <$> do
  font <- throwIfNull "texture_font_new_from_memory failed" $
    TF.c'texture_font_new_from_memory (Atlas.ptr atlas)
    (realToFrac size_) (castPtr cStr) (fromIntegral len)
  font <$ setRenderMode mode font

delete :: Font -> IO ()
delete (Font ptr) = TF.c'texture_font_delete ptr

lineGap :: Font -> IO Float
lineGap (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'linegap ptr)

height :: Font -> IO Float
height (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'height ptr)

ascender :: Font -> IO Float
ascender (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'ascender ptr)

descender :: Font -> IO Float
descender (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'descender ptr)

underlinePosition :: Font -> IO Float
underlinePosition (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_position ptr)

underlineThickness :: Font -> IO Float
underlineThickness (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_thickness ptr)

size :: Font -> IO PtSize
size (Font ptr) = realToFrac <$> peek (TF.p'texture_font_t'size ptr)
