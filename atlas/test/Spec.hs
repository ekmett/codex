module Main
( main
) where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Atlas
import Test.Hspec

harnessM :: PrimBase m => Int -> Int -> [Pt] -> m (Either [Maybe Pt] [Pt])
harnessM w h xs = do
  ctx <- atlas_create w h
  atlas_pack ctx id const const xs

harness :: Int -> Int -> [Pt] -> Either [Maybe Pt] [Pt]
harness w h xs = runST $ harnessM w h xs

main :: IO ()
main = hspec $
  describe "Data.Atlas" $ do
    it "can fill the packing region" $
      harness 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldBe` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
    it "can run in IO" $
      harnessM 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldReturn` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
    it "can be setup" $ 
      (do ctx <- atlas_create 10 10; atlas_set_heuristic ctx BestFirst) `shouldReturn` ()
