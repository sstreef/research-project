{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Server ( runWith, HTTPServer ) where

import Prelude hiding (log)

import Network.Socket
import Control.Monad (forever, void)

import Polysemy (Sem, runM, Members)
import Control.Concurrent (forkFinally)

import HTTP.Request ( HTTPRequest )
import HTTP.Response ( HTTPResponse )
import qualified HTTP.Response

import Data.Function ((&))
import Data.Maybe ( fromMaybe )
import Polysemy.KVStore ( runKVStorePurely )
import Polysemy.State ( evalState )
import Effects.Buffering ( SocketBuffer, evalSocketBuffering )
import Effects.FileReading ( runFileReadingIO )
import Effects.Logging (runConsoleLogger)
import Effects.RequestHandling
    ( RequestHandling,
      resolveRequest,
      resolveFileRequest,
      runRequestHandling,
      evalRequestHandling )

import qualified Effects.Buffering as SocketBuffer
import qualified Control.Exception as E

type HTTPServer = Sem '[RequestHandling] ()

type Port = String

runWith :: Maybe Port -> HTTPServer -> IO ()
runWith port serverSetup = runTCPServer Nothing (fromMaybe "3000" port) (evalSocketBuffering socketBufferer 256)
  where
    socketBufferer :: Members '[SocketBuffer] r => Sem r ()
    socketBufferer = do
      endOfStream <- SocketBuffer.readFromSocket
      if endOfStream
      then SocketBuffer.handle resolve
      else socketBufferer
    
    resolve :: HTTPRequest -> IO HTTPResponse
    resolve req = do
      (store, (state, _)) <- evalRequestHandling serverSetup
      (_, resp) <- chain req [resolveRequest, resolveFileRequest]
        & runRequestHandling
        & evalState state
        & runKVStorePurely store
        & runFileReadingIO
        & runConsoleLogger
        & evalState ""
        & runM
      resp
      where
        chain ::  HTTPRequest -> [HTTPRequest -> Sem r (Maybe (IO HTTPResponse))] -> Sem r (IO HTTPResponse)
        chain _ [] = return $ return HTTP.Response.notFoundResponse
        chain request (f:fs) = f request >>= \case
          Nothing       -> chain request fs
          Just response -> return response

{- Boilerplate code from the Network library -}

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    putStrLn $ "Server started listening at " ++ fromMaybe "127.0.0.1" mhost ++ ':' : port
    E.bracket (open addr) close loop
  where
    resolve :: IO AddrInfo
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)

    open :: AddrInfo -> IO Socket
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock

    loop :: Socket -> IO ()
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

{-
Functions that parse or format logging messages
-}

-- format :: HTTPRequest -> HTTPResponse -> String
-- format request response = let
--     (RequestHeaders reqMethod reqPath reqVersion) = HTTP.Request.headersFromRequest request
--     (ResponseHeaders resVersion resStatus) = HTTP.Response.headersFromResponse response
--   in
--     "REQ: " ++ show (unwords [ show reqMethod, padRight 18 reqPath, reqVersion ]) ++ " ==> " ++
--     "RES: " ++ show (unwords [ show resStatus, resVersion ])

-- showSocketIP :: Socket -> IO String
-- showSocketIP sock = getPeerName sock <&> \case
--     (SockAddrInet6 _ _ host _)  -> showIP host
--     _                           -> "Unknown IP"
--   where
--     showIP :: (Word32, Word32, Word32, Word32) -> String
--     showIP (_, _, _, ip) = intercalate "." $ map (padLeft 3 . show) [d, c, b, a]
--       where
--         (a, b, c, d) = hostAddressToTuple ip

-- padLeft :: Int -> [Char] -> [Char]
-- padLeft i s = replicate (i - length s) ' ' ++ s

-- padRight :: Int -> [Char] -> [Char]
-- padRight i s = if length result > i
--   then take (i-3) result ++ "..."
--   else result
--   where
--     result = s ++ replicate (i - length s) ' '
