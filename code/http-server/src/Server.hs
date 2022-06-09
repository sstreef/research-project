module Server ( runWith ) where

import Prelude hiding (log)

import Network.Socket
import Control.Monad (forever, void, unless)

import Control.Concurrent (forkFinally)

import qualified Control.Exception as E
import Data.Maybe (fromMaybe)
import HTTP.Request (HTTPRequest, MethodType)
import HTTP.Response (HTTPResponse, createPlainResponse, Status (OK))
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Debug.Trace

type Handler = HTTPRequest -> HTTPResponse

type Port = String

runWith :: Maybe String -> [((MethodType, String), Handler)] -> IO ()
runWith port serverSetup = runTCPServer Nothing (fromMaybe "3000" port) handleSocket
  where
    handleSocket :: Socket -> IO ()
    handleSocket sock = stateRec sock ""
      where
        bytesToRead = 16

        stateRec :: Socket -> String -> IO ()
        stateRec sock s = do
          msg <- recv sock bytesToRead
          unless (S.null msg || C.length msg < bytesToRead) $ do
            stateRec sock (s ++ C.unpack msg)
          print s
          sendAll sock $ C.pack $ show $ createPlainResponse (Just OK) "Hello World!"




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