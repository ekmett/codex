import Data.Primitive.MutVar
import Data.Watch
import Test.Hspec as Hspec
import Test.Tasty
import Test.Tasty.Hspec

spec :: Spec
spec = do
  it "IO read/write" $ do
    x <- newRef 12
    readRef x `shouldReturn` 12
    writeRef x 13
    readRef x `shouldReturn` 13
    modifyRef x (2+)
    readRef x `shouldReturn` 15
    atomicModifyRef x (\a -> (a + 3, a + 2)) `shouldReturn` 17
    readRef x `shouldReturn` 18
    modifyRef x (`div` 2)
    readRef x `shouldReturn` 9
  it "delay/force" $ do
    r <- newMutVar 0 -- evil side effects
    t <- delay $ modifyMutVar r (+1)
    readMutVar r `shouldReturn` 0
    force t
    readMutVar r `shouldReturn` 1
    force t
    readMutVar r `shouldReturn` 1
    release t
    force t
    readMutVar r `shouldReturn` 2
    force t
    readMutVar r `shouldReturn` 2
  it "delay/read" $ do
    r <- newRef "hello"
    m <- newMutVar "nope" -- for detecting effects
    t <- delay $ do
      v <- readRef r
      v <$ writeMutVar m v
    readMutVar m `shouldReturn` "nope"
    force t
    readMutVar m `shouldReturn` "hello"
    writeMutVar m "oh"
    force t
    readMutVar m `shouldReturn` "oh"
    force t
    readMutVar m `shouldReturn` "oh"
    writeRef r "bye"
    force t
    readMutVar m `shouldReturn` "bye"
  it "delayWithIO" $ do
    r <- newRef "hello"
    m <- newMutVar 0
    t <- delayWithIO (writeMutVar m) $ length <$> readRef r
    force t `shouldReturn` 5
    readMutVar m `shouldReturn` 0
    writeRef r "bye"
    force t `shouldReturn` 3
    readMutVar m `shouldReturn` 5



main :: IO ()
main = do
  spec' <- testSpec "spec" spec
  defaultMain spec'
