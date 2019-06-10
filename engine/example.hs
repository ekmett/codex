import Engine

main :: IO ()
main = withEngine $ \ drive -> do
  -- setup ... 
  drive $ do
    -- per frame
    pure ()
  -- teardown
