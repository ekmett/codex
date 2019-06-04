{-# language RecordWildCards #-}

import Codec.Picture
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.Storable
import Graphics.FreeType

main :: IO ()
main = do
  lib <- init_library
  face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
  set_pixel_sizes face 0 1024
  glyph_index <- get_char_index face (fromIntegral (fromEnum 'a'))
  load_glyph face glyph_index LOAD_RENDER
  glyphslot <- face_glyph face
  withForeignPtr (act glyphslot glyphslot_bitmap) $ \p -> do
    Bitmap{..} <- peek p
    result <- withImage (fromIntegral bitmap_width) (fromIntegral bitmap_rows) $ \x y -> do
      peek $ bitmap_buffer `plusPtr` (x + y * fromIntegral bitmap_pitch)
    writePng "example_a.png" (result :: Image Pixel8)
