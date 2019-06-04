
--import Codec.Picture
import Graphics.FreeType

main :: IO ()
main = do
  lib <- init_library
  face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
  -- set_char_size face 0 (16*64) 300 300
  set_pixel_sizes face 0 16
  glyph_index <- get_char_index face (fromIntegral (fromEnum 'a'))
  load_glyph face glyph_index 0
  glyphslot <- face_glyph face
  render_glyph glyphslot RENDER_MODE_NORMAL
  pure ()
