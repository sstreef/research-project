module Server ( runWith, ServerSetup ) where

import Effects.RequestHandling (RequestHandling, resolveRequest, resolveFileRequest, runRequestHandling, HTTPHandlerStore, HTTPStaticFilePathState)

import Prelude hiding (log)

import Effects.Logging (Logging, runConsoleLogger, logIO, log, flushLog)

import Network.Socket
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, runM, Embed)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E

import Parsers.HTTP as P ( parseRequest )
import Types.HTTP.Response ( HTTPResponse )
import qualified Types.HTTP.Response as HTTP.Response
import Types.HTTP.Request ( HTTPRequest )
import qualified Types.HTTP.Request as HTTP.Request


import qualified Data.Map.Strict as Map
import Polysemy.KVStore (runKVStorePurely)
import Data.Function ((&))
import Data.Maybe ( fromMaybe )
import Polysemy.State (runState, State)
import Effects.FileReading (runFileReadingIO, FileReading)
import Data.Word (Word32)
import Data.List (intercalate)

type ServerSetup = Sem '[RequestHandling, HTTPStaticFilePathState, HTTPHandlerStore, FileReading, Logging, State String, Embed IO]  ()

type Port = String

runWith :: Maybe Port -> ServerSetup -> IO ()
runWith port serverSetup = runTCPServer Nothing (fromMaybe "3000" port) socketListener
  where
    socketListener :: Socket -> IO ()
    socketListener s = do
      msg <- recv s 1024 -- What happens if user sends > 1024 bytes?

      unless (S.null msg) $ do
        resp <- tcpPipe s msg
        sendAll s resp
        socketListener s

    tcpPipe :: Socket -> S.ByteString -> IO S.ByteString
    tcpPipe sock s = do
      effectsResult <- do { serverSetup; resolveTCPRequest sock s }
                        & runRequestHandling
                        & runState Nothing
                        & runKVStorePurely Map.empty
                        & runFileReadingIO
                        & runConsoleLogger
                        & runState ""
                        & runM
      return . snd . snd $ snd effectsResult

resolveTCPRequest :: Member Logging r =>
                    Member RequestHandling r =>
                    Socket -> S.ByteString -> Sem r S.ByteString
resolveTCPRequest sock msg = do
        log "From: "
        logIO $ parseAddrInfo $ getPeerName sock
        log " | "

        case parseRequest $ C.unpack msg of
            Left res   -> do
              log $ "Request: Invalid Headers ==> Response: " ++ show (HTTP.Response.status $ HTTP.Response.headersFromResponse res)
              wrap res
            Right req   -> chain req [resolveRequest, resolveFileRequest]
        where
            wrap :: HTTPResponse -> Sem r S.ByteString
            wrap = pure . C.pack . show

            chain :: Member Logging r => HTTPRequest -> [HTTPRequest -> Sem r (Maybe HTTPResponse)] -> Sem r S.ByteString
            chain _ [] = wrap HTTP.Response.badRequestResponse
            chain req (f:fs) = do
              x <- f req
              case x of
                Nothing -> chain req fs
                Just res  -> do
                  log $ format req res
                  flushLog
                  wrap res

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve'
    putStrLn $ "Server started listening at " ++ fromMaybe "127.0.0.1" mhost ++ ':' : port
    E.bracket (open addr) close loop
  where
    -- 
    resolve' :: IO AddrInfo
    resolve' = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    -- 
    open :: AddrInfo -> IO Socket
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    --
    loop :: Socket -> IO ()
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

{-

Functions that parse or format logging messages

-}

format :: HTTPRequest -> HTTPResponse -> String
format req res = let
    reqHeaders = HTTP.Request.headersFromRequest req
    resHeaders = HTTP.Response.headersFromResponse res
    reqS = show (HTTP.Request.method reqHeaders)
      ++ ' ' : padRight 18 (HTTP.Request.path reqHeaders)
      ++ ' ' : HTTP.Request.version reqHeaders
    resS = show (HTTP.Response.status resHeaders)
      ++ ' ' : HTTP.Response.version resHeaders
  in
    "Request: " ++ show reqS ++ "  ==> Response: " ++ show resS

parseAddrInfo :: IO SockAddr -> IO String
parseAddrInfo info = do
  x <- info
  return $ case x of
    (SockAddrInet6 _ _ host _) -> showIP host
    _                       -> "Unknown IP"
  where
    showIP :: (Word32, Word32, Word32, Word32) -> String
    showIP (_, _, _, ip) = intercalate "." $ map (padLeft 3 . show) [d, c, b, a]
      where
        (a, b, c, d) = hostAddressToTuple ip
        

padLeft :: Int -> [Char] -> [Char]
padLeft i s = replicate (i - length s) ' ' ++ s

padRight :: Int -> [Char] -> [Char]
padRight i s = if length result > i 
  then take (i-3) result ++ "..."
  else result
  where
    result = s ++ replicate (i - length s) ' '

