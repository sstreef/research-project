module Server (runWith) where

import Effects.RequestHandling (RequestHandling, resolve, runRequestHandling, HTTPHandlerStore)

import Network.Socket
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, run)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E

import Parsers.Parser as P ( parseRequest )
import Types.HTTP.Response ( HTTPResponse )
import Types.HTTP.Request (RequestHeaders (path, method), getHeaders)


import qualified Data.Map.Strict as Map
import Polysemy.KVStore (runKVStorePurely)
import Data.Function ((&))

runWith :: Sem [RequestHandling, HTTPHandlerStore] () -> IO ()
runWith server = runTCPServer Nothing "3000" $ createTCPHandler pipe
  where
    pipe :: S.ByteString -> S.ByteString
    pipe s = do
                server              -- Registers handlers by user as effect
                resolveTCPRequest s -- Resolves request by using user registered requests
              & runRequestHandling
              & runKVStorePurely Map.empty
              & run
              & snd

resolveTCPRequest ::   Member RequestHandling r => S.ByteString -> Sem r S.ByteString
resolveTCPRequest msg = do
        case parseRequest $ C.unpack msg of 
            Left response   -> wrap response
            Right request -> let
                headers = getHeaders request
              in do
                storeEntry <- resolve (method headers) (path headers)
                case storeEntry of
                  Left response -> wrap response
                  Right handler -> wrap $ handler request
        where
            wrap :: HTTPResponse -> Sem r S.ByteString
            wrap = pure . C.pack . show

createTCPHandler :: (S.ByteString -> S.ByteString) -> Socket -> IO ()
createTCPHandler handle = listener
    where
      listener s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s $ handle msg
          listener s

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO ()
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve'
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
