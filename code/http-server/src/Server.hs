{-# LANGUAGE ScopedTypeVariables #-}
module Server ( runWith ) where

import Prelude hiding (log)

import Network.Socket
import Control.Monad (forever, void, unless)

import Control.Concurrent (forkFinally)

import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import HTTP.Request (HTTPRequest (Request), MethodType (GET), HTTPHeaders (Headers), Meta (Meta, None), getHeaders, getMeta)
import HTTP.Response (HTTPResponse (HTTPResponse), createPlainResponse, Status (OK, NotFound), createStatusResponse, badRequestResponse, notFoundResponse, createContentResponse)
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Parsers.HTTP
import Data.Functor ((<&>))
import Data.Either (fromLeft)
import HTTP.General (Payload(Empty))
import Control.Monad.State (execState)
import Text.Read (Lexeme(String))
import Parsers.File

type Handler = HTTPRequest -> IO HTTPResponse

type Port = String

runWith :: Maybe String -> [((MethodType, String), Handler)] -> Maybe String -> IO ()
runWith port handlers staticFilePath = runTCPServer Nothing (fromMaybe "3000" port) createSocketHandler
  where
    bytesToRead = 512

    createSocketHandler :: Socket -> IO ()
    createSocketHandler  = bufferedSocketHandle (return emptyRequest)

    handleFileRequest :: (MethodType, String) -> Handler
    handleFileRequest (GET, path) request = case staticFilePath of
        Just rootPath -> readFromFile rootPath path
        Nothing       -> return HTTP.Response.notFoundResponse
    handleFileRequest (_, _) _ = return HTTP.Response.badRequestResponse

    handleRequest :: BufferState -> IO HTTPResponse
    handleRequest state =
      let (_, mode, request) = execState state emptyState
      in do
        logRequest request
        case mode of
          ParsingFinished -> fromMaybe
            (return badRequestResponse) $ do
              key <- getMeta request
              return (fromMaybe (handleFileRequest key) (lookup key handlers) request)
          _             -> return badRequestResponse

    bufferedSocketHandle :: BufferState -> Socket ->  IO ()
    bufferedSocketHandle oldState sock = do
      bytes <- recv sock bytesToRead

      let newState = do { oldState; addBytes bytes; consume }

      if S.null bytes || S.length bytes < bytesToRead
            then handleRequest newState >>= sendAll sock . C.pack . show
            else bufferedSocketHandle newState sock

{- Helpers for server function -}

readFromFile :: String -> String -> IO HTTPResponse
readFromFile rootPath path = do
  (eitherErrorOrString :: Either IOError String) <- E.try $ readFile (rootPath ++ path)
  return $ case eitherErrorOrString of
    Right content -> createContentResponse (getFileContentType path) (Just OK) content
    Left _        -> HTTP.Response.notFoundResponse

logRequest :: HTTPRequest -> IO ()
logRequest (Request (Headers (Meta m p v) _) _) = print (show m ++ " " ++ p ++ " " ++ v)
logRequest _ = putStrLn "Invalid request"

{- Boilerplate code from the Network library -}

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
