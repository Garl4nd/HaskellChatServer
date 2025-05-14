module NetworkUtils (
  withSocketsDo,
  listenOn,
  NetworkUtils.accept,
) where

import Control.Exception
import qualified Data.List.NonEmpty as NEL
import qualified Network.Run.TCP as Tcp
import Network.Socket as Socket
import System.IO

listenOn :: Int -> (Socket -> IO a) -> IO a
listenOn port server = do
  addr <- Tcp.resolve Stream (Just "0.0.0.0") (show port) [AI_PASSIVE] NEL.head
  print $ "Listening on " <> show (addrAddress addr)
  bracket (Tcp.openTCPServerSocket addr) close server

accept :: Socket -> ((Handle, SockAddr) -> IO a) -> IO a
accept sock server = do
  bracketOnError (Socket.accept sock) (close . fst) $ \(conn, peer) -> do
    handle <- socketToHandle conn ReadWriteMode
    server (handle, peer)
