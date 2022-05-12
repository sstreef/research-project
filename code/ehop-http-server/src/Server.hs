{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Socket


import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, Embed, embed, runM)
import Data.Function ( (&) )

import Parsers.Parser as P ( parseRequest )
import Types.HTTP.Response (HTTPResponse)
import Types.HTTP.Request (HTTPRequest)

type HTTPRequestHandler = HTTPRequest -> HTTPResponse

requestMediator :: HTTPRequestHandler -> S.ByteString -> HTTPResponse
requestMediator handler s  = case P.parseRequest $ C.unpack s of
      (Right req) -> handler req
      (Left resp) -> resp

createTCPHandler :: (S.ByteString -> S.ByteString) -> (Socket -> IO ())
createTCPHandler handle = listener
    where
      listener s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s $ handle msg
          listener s

run :: HTTPRequestHandler -> IO ()
run handler = runTCPServer Nothing "3000" (createTCPHandler $ C.pack . show . requestMediator handler)
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

