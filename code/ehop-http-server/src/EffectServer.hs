module EffectServer (runWithEffects) where

import Effects.RequestHandling (RequestHandling, resolve, runRequestHandling, HTTPHandler, HTTPHandlerStore)

import Network.Socket
import Control.Monad (unless, forever, void)
import qualified Data.ByteString as S
import Data.ByteString.Char8 as C (unpack, pack)
import Network.Socket.ByteString (recv, sendAll)

import Polysemy (Member, Sem, run, Members)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E

import Parsers.Parser as P ( parseRequest )
import Types.HTTP.Request (HTTPRequest (HTTPRequest), RequestHeaders (path, method), MethodType)


import qualified Data.Map.Strict as Map
import Polysemy.KVStore (runKVStorePurely, KVStore)
import Data.Function ((&))

runWithEffects :: Members [RequestHandling, KVStore (MethodType, String) HTTPHandler] r =>
                  Sem r () -> IO ()
runWithEffects server = undefined
  -- runTCPServer Nothing "3000" (createTCPHandler messageHandler)
  -- where
  --   messageHandler :: S.ByteString -> S.ByteString
  --   messageHandler s = pipeRequest server s
  --     & runRequestHandling
  --     & runKVStorePurely Map.empty
  --     & run
      -- & snd



-- combineEffects :: Member RequestHandling r =>
--                   Sem r () ->
--                   S.ByteString -> (Map.Map (MethodType, String) HTTPHandler, S.ByteString)
-- combineEffects server s = pipeRequest server s
--     & runRequestHandling
--     & runKVStorePurely Map.empty
--     & run

pipeRequest ::  Member RequestHandling r =>
                Member (HTTPHandlerStore) r =>
                Sem r () -> S.ByteString -> Sem r S.ByteString
pipeRequest server msg = do
    server
    case P.parseRequest (C.unpack msg) of
      (Right (HTTPRequest headers payload)) -> do
          result <- resolve (method headers) (path headers)
          case result of
            Left resp       -> toByteStringSem resp
            Right handler   -> toByteStringSem $ handler $ HTTPRequest headers payload
      (Left resp) -> toByteStringSem resp
      where
        toByteStringSem = pure . C.pack . show

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
    addr <- resolve
    E.bracket (open addr) close loop
  where
    -- 
    resolve :: IO AddrInfo
    resolve = do
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
