module Lhx.Browser where

import Control.Monad (void)
import Control.Concurrent
import Network.Socket.Free (getFreePort)
import Network.Socket.Wait (waitWith)
import System.Process

withBrowserOnFreePort :: (Int -> IO ()) -> IO ()
withBrowserOnFreePort serveOn = do
  port <- getFreePort
  void $ forkIO do
    waitWith mempty 100000 "127.0.0.1" port
    void . spawnCommand $ "xdg-open http://127.0.0.1:" <> show port
  serveOn port

withFreePort :: (Int -> IO ()) -> IO ()
withFreePort = (getFreePort >>=)
