{-# language OverloadedStrings #-}

import Data.Const.ByteString
import Data.Default
import Graphics.Harfbuzz
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec :: Spec
spec = do
  describe "hb_blob_t" $ do
    it "should not construct the empty blob by default" $ do
      blob_create "hello" MEMORY_MODE_READONLY `shouldNotReturn` def
    it "has length" $ do
      (blob_create "hello" MEMORY_MODE_READONLY >>= blob_get_length) `shouldReturn` 5
    it "readonly is not immutable" $ do
      (blob_create "hello" MEMORY_MODE_READONLY >>= blob_is_immutable) `shouldReturn` False
    it "make_immutable makes things immutable" $ do
      let task = do
           x <- blob_create "hello" MEMORY_MODE_READONLY
           blob_make_immutable x
           blob_is_immutable x
      task `shouldReturn` True 
    it "we get out what we put in" $ do
      let task = do
           x <- blob_create "hello" MEMORY_MODE_READONLY 
           withBlobData x packACStringLen
      task `shouldReturn` "hello"
    it "trims correctly" $ do
      let task = do
           x <- blob_create "hello" MEMORY_MODE_WRITABLE
           y <- blob_create_sub_blob x 2 2
           (,) <$> withBlobData x packACStringLen <*> withBlobData y packACStringLen
      task `shouldReturn` ("hello","ll")
    it "forming a sub_blob renders the parent immutable" $ do
      let task = do
           x <- blob_create "hello" MEMORY_MODE_WRITABLE
           _ <- blob_create_sub_blob x 2 2
           blob_is_immutable x
      task `shouldReturn` True
  describe "hb_script_t" $ do
    it "has Hebrew" $ do script_to_string SCRIPT_HEBREW `shouldBe` "Hebr"
    it "has Tamil" $ do script_to_string SCRIPT_TAMIL `shouldBe` "Taml"
    it "has Braille" $ do script_to_string SCRIPT_BRAILLE `shouldBe` "Brai"
    it "corrects csase " $ do script_from_string "HEBR" `shouldBe` SCRIPT_HEBREW
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

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain $
    testGroup "tests"
      [ spec'
      ]
