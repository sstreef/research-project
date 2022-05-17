module Server ( runWith, ServerSetup ) where

import Effects.RequestHandling (RequestHandling, resolve, runRequestHandling, HTTPHandlerStore, HTTPStaticFileState)

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


import qualified Data.Map.Strict as Map
import Polysemy.KVStore (runKVStorePurely)
import Data.Function ((&))
import Data.Maybe ( fromMaybe )
import Polysemy.State (runState)

type ServerSetup = Sem '[RequestHandling, HTTPStaticFileState, HTTPHandlerStore, Embed IO]  ()

type Port = String

runWith :: Maybe Port -> ServerSetup -> IO ()
runWith port server = runTCPServer Nothing (fromMaybe "3000" port) $ createTCPHandler $ normalize pipe
  where
    pipe s = (do
                server                -- Registers handlers by user in effect
                resolveTCPRequest s)  -- Resolves request by using user registered requests
            & runRequestHandling
            & runState Nothing
            & runKVStorePurely Map.empty
            & runM
    normalize p s = do
      x <- p s
      return $ snd $ snd x

resolveTCPRequest :: Member RequestHandling r => S.ByteString -> Sem r S.ByteString
resolveTCPRequest msg = do
        case parseRequest $ C.unpack msg of
            Left response   -> wrap response
            Right request   -> do
              response <- resolve request
              wrap response
        where
            wrap :: HTTPResponse -> Sem r S.ByteString
            wrap = pure . C.pack . show

createTCPHandler :: (S.ByteString -> IO S.ByteString) -> Socket -> IO ()
createTCPHandler handle = listener
    where
      listener s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          resp <- handle msg
          sendAll s resp
          listener s

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
