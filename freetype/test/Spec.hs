{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
module Main
( main
) where

import Codec.Picture
import qualified Data.ByteString as ByteString
import Data.Coerce
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Ptr.Diff
import Foreign.Storable
import System.Mem (performMajorGC)
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Hspec

import Graphics.FreeType
import Graphics.FreeType.Internal

c2w :: Char -> Word32
c2w = fromIntegral . fromEnum

spec :: IO TestTree
spec = testSpec "spec" $ do -- Hspec.after_ performMajorGC $ do
  describe "Library" $ do
--    it "can be constructed from scratch" $ do
--      lib <- new_uninitialized_library
--      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
    it "can be constructed with defaults" $ do
      lib <- init_library
      unsafeForeignPtrToPtr (coerce lib) `shouldNotBe`  nullPtr
    it "can be reference counted" $ do
      lib <- init_library
      reference_library lib
      done_library lib `shouldReturn` () -- we don't crash during the subsequent gc
  describe "Face" $ do
    it "can load a file" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
      has_multiple_masters face `shouldReturn` False
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_first_char face `shouldReturn` (c2w ' ',1)
      get_next_char face (c2w ' ') `shouldReturn` (c2w '!',918)
      get_font_format face `shouldReturn` "CFF"
    it "can load a variable otf font (from memory)" $ do
      lib <- init_library
      blob <- ByteString.readFile "test/fonts/SourceCodeVariable-Roman.otf"
      face <- new_memory_face lib blob 0
      has_multiple_masters face `shouldReturn` True
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_font_format face `shouldReturn` "CFF"
      get_first_char face `shouldReturn` (c2w ' ',1)
      set_pixel_sizes face 32 32 -- we can set the current pixel size
    it "can load a variable ttf font" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/SourceCodeVariable-Roman.ttf" 0
      has_multiple_masters face `shouldReturn` True
      get_char_index face (c2w 'a') `shouldReturn` 28 -- mined from the font itself
      get_first_char face `shouldReturn` (c2w ' ',1)
      get_font_format face `shouldReturn` "TrueType"
    it "can load sanskrit font" $ do
      lib <- init_library
      face <- new_face lib "test/fonts/Sanskrit2003.ttf" 0
      has_multiple_masters face `shouldReturn` False

golden :: IO TestTree
golden = do
  lib <- init_library
  face <- new_face lib "test/fonts/SourceCodePro-Regular.otf" 0
  set_pixel_sizes face 0 64
  bitmap <- (`act` glyphslot_bitmap) <$> face_glyph face
  let goldenChar :: Char -> TestTree
      goldenChar c = goldenVsFile g g a $ do
          load_char face (fromIntegral $ fromEnum c) LOAD_RENDER
          image <- withForeignPtr bitmap $ \p -> do
            Bitmap{..} <- peek p
            withImage (fromIntegral bitmap_width) (fromIntegral bitmap_rows) $ \x y -> do
              peek $ bitmap_buffer `plusPtr` (x + y * fromIntegral bitmap_pitch)
          writePng a (image :: Image Pixel8)
        where a, g :: FilePath
              a = "test/golden/" ++ (c :".actual.png")
              g = "test/golden/" ++ (c :".golden.png")
  return $ testGroup "golden" $ goldenChar <$> ['a'..'z']

main :: IO ()
main = do
  tests <- sequence [golden,spec]
  defaultMain $ testGroup "tests" tests
