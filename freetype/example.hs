{-# language RecordWildCards #-}

import Codec.Picture
import Control.Exception
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.Storable
import GHC.Conc
import Graphics.FreeType
import System.IO

main :: IO ()
main = do
  setUncaughtExceptionHandler $ putStrLn . ("Error: "++) . displayException
  hSetBuffering stdout NoBuffering

  putStrLn "loading library"
  lib <- init_library

  putStrLn "loading face"
  face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0

  putStrLn "setting pixel size"
  set_pixel_sizes face 0 1024

  putStrLn "getting character index"
  glyph_index <- get_char_index face (fromIntegral (fromEnum 'a'))

  putStrLn "loading glyph"
  load_glyph face glyph_index LOAD_RENDER
  
  putStrLn "accessing glyph slot"
  glyphslot <- face_glyph face
  
    --putStrLn "rendering glyph"
    --render_glyph glyphslot RENDER_MODE_NORMAL
  
  putStrLn "fetching bitmap"
  withForeignPtr (act glyphslot glyphslot_bitmap) $ \p -> do
    Bitmap{..} <- peek p
    result <- withImage (fromIntegral bitmap_width) (fromIntegral bitmap_rows) $ \x y -> do
      peek $ bitmap_buffer `plusPtr` (x + y * fromIntegral bitmap_pitch)
    putStrLn "rendering png"
    writePng "example_a.png" (result :: Image Pixel8)
  putStrLn "cleaning up"
