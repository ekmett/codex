import Data.Primitive.MutVar
import Data.Watch
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec :: Spec
spec = do
  it "IO read/write" $ do
    x <- newVar 12
    readVar x `shouldReturn` 12
    writeVar x 13
    readVar x `shouldReturn` 13
    modifyVar x (2+)
    readVar x `shouldReturn` 15
    atomicModifyVar x (\a -> (a + 3, a + 2)) `shouldReturn` 17
    readVar x `shouldReturn` 18
    modifyVar x (`div` 2)
    readVar x `shouldReturn` 9
  it "delay/force" $ do
    r <- newMutVar 0 -- evil side effects
    t <- delay $ modifyMutVar r (+1)
    readMutVar r `shouldReturn` 0
    force t
    readMutVar r `shouldReturn` 1
    force t
    readMutVar r `shouldReturn` 1
    release t
  it "delay/read" $ do
    r <- newVar "hello"
    m <- newMutVar "nope" -- for detecting effects
    t <- delay $ do
      v <- readVar r
      v <$ writeMutVar m v
    readMutVar m `shouldReturn` "nope"
    force t
    readMutVar m `shouldReturn` "hello"
    writeMutVar m "oh"
    force t
    readMutVar m `shouldReturn` "oh"
    force t
    readMutVar m `shouldReturn` "oh"
    writeVar r "bye"
    force t
    readMutVar m `shouldReturn` "bye"
  it "delayWithIO" $ do
    r <- newVar "hello"
    m <- newMutVar 0
    t <- delayWithIO (writeMutVar m) $ length <$> readVar r
    force t `shouldReturn` 5
    readMutVar m `shouldReturn` 0
    writeVar r "bye"
    force t `shouldReturn` 3
    readMutVar m `shouldReturn` 5
  it "chains" $ do
    r <- newVar "hello"
    x <- delay $ readVar r
    force x `shouldReturn` "hello"
    y <- delay $ length <$> force x
    force y `shouldReturn` 5
    writeVar r "bye"
    force y `shouldReturn` 3
    
main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
