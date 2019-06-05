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
  load_char face (fromIntegral (fromEnum 'a'))  LOAD_RENDER
  bitmap <- act glyphslot_bitmap <$> face_glyph face
  withForeignPtr bitmap $ \p -> do
    Bitmap{..} <- peek p
    result <- withImage (fromIntegral bitmap_width) (fromIntegral bitmap_rows) $ \x y -> do
      peek $ bitmap_buffer `plusPtr` (x + y * fromIntegral bitmap_pitch)
    writePng "example_a.png" (result :: Image Pixel8)
