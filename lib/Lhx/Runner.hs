module Lhx.Runner where

import Control.Concurrent

interactWith :: ((a -> IO ()) -> IO ()) -> IO a
interactWith app = do
  ref <- newEmptyMVar
  tid <- forkIO . app $ putMVar ref
  result <- readMVar ref
  killThread tid
  pure result
