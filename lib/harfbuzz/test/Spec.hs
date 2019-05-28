{-# language OverloadedStrings #-}

import Data.Char
import Data.Const.ByteString
import Data.Default
import Data.Functor
import Data.StateVar
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Utils (new)
import Graphics.Harfbuzz
import System.Mem
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

gc :: UnicodeGeneralCategory -> GeneralCategory
gc (UNICODE_GENERAL_CATEGORY x) = x

spec :: Spec
spec = Hspec.after_ performMajorGC $ do
  describe "hb_unicode_funcs_t" $ do
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
  describe "hb_tag_t" $ do
    it "TAG matches" $ (case script_to_iso15924_tag SCRIPT_HEBREW of TAG a b c d -> (a,b,c,d); _ -> undefined) `shouldBe` ('H','e','b','r')
    it "TAG constructs" $ script_from_iso15924_tag (TAG 'H' 'e' 'b' 'r') == SCRIPT_HEBREW
  describe "hb_script_t" $ do
    it "compares" $ (SCRIPT_HEBREW == SCRIPT_TAMIL) `shouldBe` False
    it "has Hebrew" $ do script_to_string SCRIPT_HEBREW `shouldBe` "Hebr"
    it "has Tamil" $ do script_to_string SCRIPT_TAMIL `shouldBe` "Taml"
    it "has Braille" $ do script_to_string SCRIPT_BRAILLE `shouldBe` "Brai"
    it "corrects case " $ do script_from_string "HEBR" `shouldBe` SCRIPT_HEBREW
  describe "hb_blob_t" $ do
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
      withBlobData x packACStringLen `shouldReturn` "hello"
    it "trims correctly" $ do
      x <- blob_create "hello" MEMORY_MODE_WRITABLE
      y <- blob_create_sub_blob x 2 2
      withBlobData x packACStringLen `shouldReturn` "hello"
      withBlobData y packACStringLen `shouldReturn` "ll"
    it "forming a sub_blob renders the parent immutable" $ do
      x <- blob_create "hello" MEMORY_MODE_WRITABLE
      _ <- blob_create_sub_blob x 2 2
      blob_is_immutable x `shouldReturn` True
    it "supports user data" $ do
      u <- new ()
      v <- new ()
      ku <- key_create
      kv <- key_create
      x <- blob_create "hello" MEMORY_MODE_READONLY
      set_user_data x ku u finalizerFree False `shouldReturn` True
      set_user_data x kv v finalizerFree False `shouldReturn` True
      get_user_data x ku `shouldReturn` u
      get_user_data x kv `shouldReturn` v
      set_user_data x ku v finalizerFree False `shouldReturn` False
      get_user_data x ku `shouldReturn` u
  describe "hb_direction_t" $ do
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
  describe "hb_feature_t" $ do
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
  describe "hb_variation_t" $ do
    it "parses" $ do "wght=10" `shouldBe` Variation "wght" 10
  describe "hb_buffer_t" $ do
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

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain $
    testGroup "tests"
      [ spec'
      ]
