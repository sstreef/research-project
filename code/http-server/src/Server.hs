module Server ( runWith ) where

import Prelude hiding (log)

import Network.Socket
import Control.Monad (forever, void, unless)

import Control.Concurrent (forkFinally)

import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import HTTP.Request (HTTPRequest (Request), MethodType, HTTPHeaders (Headers), Meta (Meta, None), getHeaders, getMeta)
import HTTP.Response (HTTPResponse (HTTPResponse), createPlainResponse, Status (OK, NotFound), createStatusResponse, badRequestResponse, notFoundResponse)
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Debug.Trace
import Parsers.HTTP
import Data.Functor ((<&>))
import Data.Either (fromLeft)

type Handler = HTTPRequest -> HTTPResponse

type Port = String

runWith :: Maybe String -> [((MethodType, String), Handler)] -> Maybe String -> IO ()
-- runWith port handlers staticFilePath = runTCPServer Nothing (fromMaybe "3000" port) handleSocket
runWith port handlers staticFilePath = runTCPServer Nothing (fromMaybe "3000" port) createSocketHandler
  where
    bytesToRead = 128

    logRequest :: HTTPRequest -> IO ()
    logRequest (Request (Headers (Meta m p v) _) _) = putStrLn (show m ++ " " ++ p ++ " " ++ v ++ "1")
    logRequest _ = putStrLn "Invalid request"

    lookupRequestHandler :: (MethodType, String) -> Maybe Handler
    lookupRequestHandler key = lookup key handlers 

    handleRequest :: String -> IO HTTPResponse
    handleRequest s = return $ fromMaybe notFoundResponse $ 
      case parseRequest s of
        Left  res -> Just res
        Right req -> (getMeta req >>= lookupRequestHandler) <*> Just req

    createSocketHandler :: Socket -> IO ()
    createSocketHandler = bufferedSocketHandle ""

    bufferedSocketHandle :: String -> Socket -> IO ()
    bufferedSocketHandle buffer sock = do
          msg <- recv sock bytesToRead <&> C.unpack

          if null msg || length msg < bytesToRead
            then handleRequest buffer >>= sendAll sock . C.pack . show
            else bufferedSocketHandle (buffer ++ msg) sock



runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO ()) -> IO ()
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
