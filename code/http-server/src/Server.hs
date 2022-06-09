module Server ( runWith ) where

import Prelude hiding (log)

import Network.Socket
import Control.Monad (forever, void)

import Control.Concurrent (forkFinally)

import qualified Control.Exception as E
import Data.Maybe (fromMaybe)


type Port = String

-- runWith port serverSetup = runTCPServer Nothing (fromMaybe "3000" port)
runWith port serverSetup = undefined 


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