{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Socket


import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever)
import qualified Data.ByteString as S
import Data.ByteString.Char8 (pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, Embed, embed, runM)
import Data.Function ( (&) )

import HTTP.Types.Response

type RawHandler = S.ByteString -> S.ByteString

type HTTPHandler = S.ByteString -> HTTPResponse

convertResponse :: HTTPResponse -> S.ByteString
convertResponse x = pack $ show x

createTCPHandler :: RawHandler -> (Socket -> IO ())
createTCPHandler handle = listener
    where
      listener s = do
        -- Try to receive 1024 bytes
        msg <- recv s 1024
        -- If message is not empty consume and recurse
        unless (S.null msg) $ do
          -- Send all bytes to socket
          sendAll s $ handle msg
          print $ show msg
          -- Tail recurse to consume all bytes until none are left.
          listener s

run :: HTTPHandler -> IO ()
run handler = runTCPServer Nothing "3000" (createTCPHandler (convertResponse . handler))
  & runM
    

-- from the "network-run" package.
runTCPServer :: Member (Embed IO) r => Maybe HostName -> ServiceName -> (Socket -> IO a) -> Sem r ()
runTCPServer mhost port server = embed $ withSocketsDo $ do
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

