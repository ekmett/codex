module Main
( main
) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Atlas
import Data.Functor.Identity
import Test.Hspec

harnessM :: PrimMonad m => Int -> Int -> [Pt] -> m (Either [Maybe Pt] [Pt])
harnessM w h xs = do
  ctx <- new_ w h
  pack ctx boxy (\_ -> _boxPosition) (\_ -> runIdentity . _boxPosition) xs

harness :: Int -> Int -> [Pt] -> Either [Maybe Pt] [Pt]
harness w h xs = runST $ harnessM w h xs

main :: IO ()
main = hspec $
  describe "Data.Atlas" $ do
    it "can fill the packing region" $
      harness 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldBe` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
    it "can run in IO" $
      harnessM 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldReturn` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
    it "can be setup before you feed it data" $ 
      (do ctx <- new_ 10 10; heuristic ctx BF) `shouldReturn` ()
