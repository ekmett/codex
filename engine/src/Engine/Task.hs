{-# language ImplicitParams #-}
{-# language ConstraintKinds #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}
{-# language RankNTypes #-}
{-# language BlockArguments #-}
-- | Used to send work to the main thread when working with non-thread safe libraries
--
-- Usage:
--
-- @
-- main = withTasks $ \pumpTasks -> do
--   ...
--   forever $ do
--     forkIO $ do
--       ... compute stuff ...
--       task $ do ... send it to opengl ...
--     ...
--     pumpTasks
-- @
module Engine.Task 
( Task
, task
, withTasks
, GivenTasks
) where

import Control.Concurrent.STM
import Control.Monad.Fix
import Control.Monad.IO.Class

type Task = IO ()
type GivenTasks = (?tasks :: TChan Task)

withTasks :: MonadIO m => (GivenTasks => IO () -> m ()) -> m ()
withTasks k = do
  (broadcastChan,listenerChan) <- liftIO $ atomically do
    broadcastChan <- newBroadcastTChan 
    (broadcastChan,) <$> dupTChan broadcastChan
  let ?tasks = broadcastChan
  k $ fix \loop -> 
    atomically (tryReadTChan listenerChan) >>= \case
      Just a  -> a *> loop
      Nothing -> pure ()

task :: (MonadIO m, GivenTasks) => IO () -> m ()
task = liftIO . atomically . writeTChan ?tasks 
