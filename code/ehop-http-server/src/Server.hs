{-# LANGUAGE OverloadedStrings #-}
module Server (run) where

import Network.Socket


import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (unless, forever)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack, lines, words)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, Embed, embed, runM)
import Data.Function ( (&) )

import Types.HTTP.Response ( HTTPResponse (HTTPResponse), ResponseHeaders (ResponseHeaders), Status (BadRequest) )
import Types.HTTP.Request ( HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders), parseMethodType )
import Types.HTTP.General (Payload(Empty, Payload), ContentType (TextPlain))

type RawHandler = S.ByteString -> S.ByteString

parseRequest :: S.ByteString -> Either (String, Status) HTTPRequest
parseRequest x = if S.null x
    then Left ("400 Bad Request", BadRequest)
    else
      let ws = C.words (head xs)
      in
        if length ws == 3
        then let
            methodTypeString = head ws
            requestPath = C.unpack $ head $ tail ws
            protocolVersion = C.unpack $ head $ tail $ tail ws
          in
            case parseMethodType methodTypeString of
              (Right methodType) -> Right $ HTTPRequest
                (RequestHeaders methodType requestPath protocolVersion)
                Empty
              (Left t) -> Left t
        else
          Left ("400 Bad Request", BadRequest)
    where
      xs = C.lines x

type HTTPRequestHandler = HTTPRequest -> HTTPResponse

requestMediator :: HTTPRequestHandler -> Either (String, Status) HTTPRequest -> HTTPResponse
requestMediator handler res = case res of
  (Left (err, code))  -> HTTPResponse (ResponseHeaders "HTTP/1.0" code) $ Payload (length err) TextPlain err
  (Right req) -> handler req


createTCPHandler :: RawHandler -> (Socket -> IO ())
createTCPHandler handle = listener
    where
      listener s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s $ handle msg
          listener s

run :: HTTPRequestHandler -> IO ()
run handler = runTCPServer Nothing "3000" (createTCPHandler $ C.pack . show . requestMediator handler . parseRequest)
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

