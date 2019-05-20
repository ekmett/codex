import Control.Monad.Primitive
import Control.Monad.ST
import Data.Functor.Identity
import Data.Proxy
import Test.Hspec

import Codex.Packing
-- import qualified Codex.UI as UI

harnessM :: PrimMonad m => Int -> Int -> Int -> [Pt] -> m (Either [Maybe Pt] [Pt])
harnessM w h n xs = do
  ctx <- new w h n
  pack ctx (\(Pt w' h') -> Box w' h' Proxy) (const _boxPosition) (const (runIdentity . _boxPosition)) xs

harness :: Int -> Int -> Int -> [Pt] -> Either [Maybe Pt] [Pt]
harness w h n xs = runST $ harnessM w h n xs

main :: IO ()
main = hspec $ do
  describe "Codex.Packing" $ do
    it "can fill the packing region" $
      harness 100 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldBe` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
    it "can run in IO" $
      harnessM 100 100 100 [Pt 50 50, Pt 50 50, Pt 50 50, Pt 50 50] `shouldReturn` Right [Pt 0 0,Pt 50 0,Pt 0 50,Pt 50 50]
