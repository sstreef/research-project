module Server ( runWith, ServerSetup ) where

import Effects.RequestHandling (RequestHandling, resolveRequest, resolveFileRequest, runRequestHandling, HTTPHandlerStore, HTTPStaticFileState)

import Network.Socket
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, runM, Embed)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E

import Parsers.Parser as P ( parseRequest )
import Types.HTTP.Response ( HTTPResponse )
import qualified Types.HTTP.Response as Types.Response
import Types.HTTP.Request ( HTTPRequest )


import qualified Data.Map.Strict as Map
import Polysemy.KVStore (runKVStorePurely)
import Data.Function ((&))
import Data.Maybe ( fromMaybe )
import Polysemy.State (runState)
import Effects.FileReading (runFileReadingIO, FileReading)

type ServerSetup = Sem '[RequestHandling, HTTPStaticFileState, HTTPHandlerStore, FileReading, Embed IO]  ()

type Port = String

runWith :: Maybe Port -> ServerSetup -> IO ()
runWith port serverSetup = runTCPServer Nothing (fromMaybe "3000" port) socketListener
  where
    socketListener :: Socket -> IO ()
    socketListener s = do
      msg <- recv s 1024
      unless (S.null msg) $ do
        resp <- tcpPipe msg
        sendAll s resp
        socketListener s
      
    tcpPipe :: S.ByteString -> IO S.ByteString
    tcpPipe s = do
      effectsResult <- do { serverSetup; resolveTCPRequest s }
                        & runRequestHandling
                        & runState Nothing
                        & runKVStorePurely Map.empty
                        & runFileReadingIO
                        & runM
      return . snd $ snd effectsResult

resolveTCPRequest :: Member RequestHandling r => S.ByteString -> Sem r S.ByteString
resolveTCPRequest msg = do
        case parseRequest $ C.unpack msg of
            Left response   -> wrap response
            Right request   -> chain request [resolveRequest, resolveFileRequest]
        where
            wrap :: HTTPResponse -> Sem r S.ByteString
            wrap = pure . C.pack . show

            chain :: HTTPRequest -> [HTTPRequest -> Sem r (Maybe HTTPResponse)] -> Sem r S.ByteString
            chain _ [] = wrap Types.Response.badRequestResponse
            chain req (f:fs) = do
              x <- f req
              case x of
                Nothing -> chain req fs
                Just r  -> wrap r

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
