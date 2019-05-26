{-# language OverloadedStrings #-}

import Data.Const.ByteString
import Data.Default
import Graphics.Harfbuzz
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec :: Spec
spec =
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

main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain $
    testGroup "tests"
      [ spec'
      ]
