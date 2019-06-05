{-# language OverloadedStrings #-}

import Data.Char
import Data.Const.ByteString
import Data.Default
import Data.Functor
import Data.Primitive.StateVar
import Data.Text.Foreign (lengthWord16)
import Graphics.Harfbuzz
import System.Mem
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

gc :: UnicodeGeneralCategory -> GeneralCategory
gc (UNICODE_GENERAL_CATEGORY x) = x


spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "unicode-funcs" $ do
    it "can identify general categories" $ do
      uf <- unicode_funcs_get_default
      (unicode_general_category uf 'a' <&> gc) `shouldReturn` LowercaseLetter
      (unicode_general_category uf 'A' <&> gc) `shouldReturn` UppercaseLetter
    it "can override general categories on default" $ do
      uf <- unicode_funcs_get_default
      uf' <- unicode_funcs_create uf
      unicode_funcs_set_general_category_func uf' $ \_ -> return $ UNICODE_GENERAL_CATEGORY Control
      (unicode_general_category uf' 'a' <&> gc) `shouldReturn` Control
    it "can override general categories on empty" $ do
      uf <- unicode_funcs_create def
      unicode_funcs_set_general_category_func uf $ \_ -> return $ UNICODE_GENERAL_CATEGORY Control
      (unicode_general_category uf 'a' <&> gc) `shouldReturn` Control
    it "can compose" $ do
      uf <- unicode_funcs_get_default
      unicode_compose uf 'e' '\x0301' `shouldReturn` Just '\x00e9'
  describe "tag" $ do
    it "TAG matches" $ script_to_iso15924_tag SCRIPT_HEBREW `shouldBe` TAG 'H' 'e' 'b' 'r'
    it "TAG constructs" $ script_from_iso15924_tag (TAG 'H' 'e' 'b' 'r') == SCRIPT_HEBREW
  describe "script" $ do
    it "compares" $ (SCRIPT_HEBREW == SCRIPT_TAMIL) `shouldBe` False
    it "has Hebrew" $ do script_to_string SCRIPT_HEBREW `shouldBe` "Hebr"
    it "has Tamil" $ do script_to_string SCRIPT_TAMIL `shouldBe` "Taml"
    it "has Braille" $ do script_to_string SCRIPT_BRAILLE `shouldBe` "Brai"
    it "corrects case " $ do script_from_string "HEBR" `shouldBe` SCRIPT_HEBREW
  describe "blob" $ do
    it "should not construct the empty blob by default" $ do
      blob_create "hello" MEMORY_MODE_READONLY `shouldNotReturn` def
    it "has length" $ do
      (blob_create "hello" MEMORY_MODE_READONLY >>= blob_get_length) `shouldReturn` 5
    it "readonly is not immutable" $ do
      (blob_create "hello" MEMORY_MODE_READONLY >>= blob_is_immutable) `shouldReturn` False
    it "make_immutable makes things immutable" $ do
      x <- blob_create "hello" MEMORY_MODE_READONLY
      blob_is_immutable x `shouldReturn` False
      blob_make_immutable x
      blob_is_immutable x `shouldReturn` True
    it "we get out what we put in" $ do
      x <- blob_create "hello" MEMORY_MODE_READONLY
      with_blob_data x packACStringLen `shouldReturn` "hello"
    it "trims correctly" $ do
      x <- blob_create "hello" MEMORY_MODE_WRITABLE
      y <- blob_create_sub_blob x 2 2
      with_blob_data x packACStringLen `shouldReturn` "hello"
      with_blob_data y packACStringLen `shouldReturn` "ll"
    it "forming a sub_blob renders the parent immutable" $ do
      x <- blob_create "hello" MEMORY_MODE_WRITABLE
      _ <- blob_create_sub_blob x 2 2
      blob_is_immutable x `shouldReturn` True
    it "supports user data" $ do
      let u = 1 :: Int
          v = 2 :: Int
      ku <- key_create
      kv <- key_create
      x <- blob_create "hello" MEMORY_MODE_READONLY
      object_set_user_data x ku u False `shouldReturn` True
      object_set_user_data x kv v False `shouldReturn` True
      object_get_user_data x ku `shouldReturn` Just u
      object_get_user_data x kv `shouldReturn` Just v
      object_set_user_data x ku v False `shouldReturn` False
      object_get_user_data x ku `shouldReturn` Just u
  describe "direction" $ do
    describe "direction_from_string" $ do
      it "DIRECTION_LTR" $ do direction_from_string "l" `shouldBe` DIRECTION_LTR
      it "DIRECTION_RTL" $ do direction_from_string "rtl" `shouldBe` DIRECTION_RTL
      it "DIRECTION_BTT" $ do direction_from_string "bottom-to-top" `shouldBe` DIRECTION_BTT
      it "DIRECTION_TTB" $ do direction_from_string "top-to-bottom" `shouldBe` DIRECTION_TTB
      it "DIRECTION_INVALID" $ do direction_from_string "garbage" `shouldBe` DIRECTION_INVALID
    describe "direction_to_string" $ do
      it "ltr" $ do direction_to_string DIRECTION_LTR `shouldBe` "ltr"
      it "rtl" $ do direction_to_string DIRECTION_RTL `shouldBe` "rtl"
      it "btt" $ do direction_to_string DIRECTION_BTT `shouldBe` "btt"
      it "ttb" $ do direction_to_string DIRECTION_TTB `shouldBe` "ttb"
      it "invalid" $ do direction_to_string DIRECTION_INVALID `shouldBe` "invalid"
  describe "feature" $ do
    it "parses" $ do "kern[1:5]" `shouldBe` Feature "kern" 1 1 5
    it "compares" $ do "kern[1:5]" `shouldNotBe` ("kern[1:6]" :: Feature)
    it "+kern" $ do "+kern" `shouldBe` Feature "kern" 1 0 maxBound
    it "-kern" $ do "-kern" `shouldBe` Feature "kern" 0 0 maxBound
    it "kern=1" $ do "kern=1" `shouldBe` Feature "kern" 1 0 maxBound
    it "kern=0" $ do "kern=0" `shouldBe` Feature "kern" 0 0 maxBound
    it "aalt=2" $ do "aalt=2" `shouldBe` Feature "aalt" 2 0 maxBound
    it "kern[]" $ do "kern[]" `shouldBe` Feature "kern" 1 0 maxBound
    it "kern[:]" $ do "kern[:]" `shouldBe` Feature "kern" 1 0 maxBound
    it "kern[5:]" $ do "kern[5:]" `shouldBe` Feature "kern" 1 5 maxBound
    it "kern[:5]" $ do "kern[:5]" `shouldBe` Feature "kern" 1 0 5
    it "kern[3]" $ do "kern[3]" `shouldBe` Feature "kern" 1 3 4
    it "aalt[3:5]=2" $ do "aalt[3:5]=2" `shouldBe` Feature "aalt" 2 3 5
  describe "variation" $ do
    it "parses" $ do "wght=10" `shouldBe` Variation "wght" 10
  describe "buffer" $ do
    it "create produces an empty buffer" $ do
      (buffer_create >>= buffer_get_length) `shouldReturn` 0
    it "can add to a buffer" $ do
      x <- buffer_create
      buffer_add_char x 'a' 0
      buffer_get_length x `shouldReturn` 1
    it "can be reset" $ do
      x <- buffer_create
      buffer_add_char x 'a' 0
      buffer_reset x
      buffer_get_length x `shouldReturn` 0
    it "can append" $ do
      x <- buffer_create
      buffer_add_char x 'a' 0
      buffer_add_char x 'b' 0
      buffer_get_length x `shouldReturn` 2
      y <- buffer_create
      buffer_add_char y 'c' 0
      buffer_add_char y 'd' 0
      buffer_add_char y 'e' 0
      buffer_add_char y 'f' 0
      buffer_append x y 1 3 -- abde
      buffer_get_length x `shouldReturn` 4
    it "can guess" $ do
      x <- buffer_create
      buffer_segment_properties x $= def
      buffer_content_type x $= BUFFER_CONTENT_TYPE_UNICODE
      buffer_add_char x '\x05d3' 0 -- a Hebrew letter, dalet
      buffer_guess_segment_properties x
      get (buffer_script x) `shouldReturn` "Hebr"
      get (buffer_direction x) `shouldReturn` "rtl"
      default_language <- language_get_default
      get (buffer_language x) `shouldReturn` default_language -- harfbuzz does not guess language off content, takes host language
  describe "face" $ do
    it "can load a file" $ do
      blob <- blob_create_from_file "test/fonts/SourceCodeVariable-Roman.otf"
      face_count blob `shouldReturn` 1
      face <- face_create blob 0
      face `shouldNotBe` def -- non-empty face
      get (face_index face) `shouldReturn` 0
      get (face_upem face) `shouldReturn` 1000
      face_is_immutable face `shouldReturn` False
      get (face_glyph_count face) `shouldReturn` 1585 -- ?
      font <- font_create face
      font `shouldNotBe` def -- non-empty font
      face_is_immutable face `shouldReturn` True -- creating a font freezes the face
  describe "shape" $ do
    it "Hindi" $ do
      blob <- blob_create_from_file "test/fonts/Sanskrit2003.ttf"
      face <- face_create blob 0
      font <- font_create face
      buffer <- buffer_create
      buffer_direction buffer $= DIRECTION_LTR
      buffer_language buffer $= "hi"
      buffer_script buffer $= SCRIPT_DEVANAGARI
      let text = "हालाँकि प्रचलित रूप पूजा"
      buffer_add_text buffer text 0 (lengthWord16 text)
      shape font buffer mempty
      gis <- buffer_get_glyph_infos buffer
      map glyph_info_codepoint gis `shouldBe` [199,548,193,548,559,454,166,2,0,0]
      -- gps <- buffer_get_glyph_positions buffer

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
