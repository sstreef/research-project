{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Socket


import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever)
import qualified Data.ByteString as S
import Network.Socket.ByteString (recv, sendAll)

bufferSize :: Int
bufferSize = 1024


run :: IO ()
run = runTCPServer Nothing "3000" talk
  where
    talk s = do
      -- Try to receive 1024 bytes
      msg <- recv s bufferSize
      -- If message is not empty consume and recurse
      unless (S.null msg) $ do
        -- Send all bytes to socket
        sendAll s msg
        print $ show msg
        -- Tail recurse to consume all bytes until none are left.
        talk s

-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    -- 
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    -- 
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    -- Accept connections and resolve data with "server" function
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        thr_id <- forkFinally (server conn) (const $ gracefulClose conn 5000)
        print $ "{ ThreadId: " ++ show thr_id ++ ", PeerConnection: " ++ show _peer ++ " }"

