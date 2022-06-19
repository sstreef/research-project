{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
module Effects.Buffering where

import Network.Socket (Socket)

import Polysemy (makeSem, Sem, interpret, Member, Embed, embed, runM, raiseUnder2)
import Network.Socket.ByteString (recv, sendAll)
import Polysemy.State (get, runState)
import HTTP.Request (HTTPRequest (Request), HTTPHeaders (Headers), Meta (None))
import HTTP.General (Payload (Empty))
import Parsers.HTTP (BufferState, addBytes, consume, BufferStateValues, BufferMode (ParsingFinished, ParsingMeta))
import qualified Data.ByteString.Char8 as C
import Control.Monad (void)
import qualified HTTP.Response as HTTPResponse
import HTTP.Response ( HTTPResponse )
import Data.Function ((&))
import qualified Data.ByteString as S

data SocketBuffer m a where
    -- Appends a ByteString to the buffer
    ReadFromSocket  :: SocketBuffer m Bool
    Handle          :: (HTTPRequest -> IO HTTPResponse) -> SocketBuffer m ()

makeSem ''SocketBuffer

runSocketBuffering ::   Member (Embed IO) r =>
                        Member BufferState r =>
                        Int -> Socket -> Sem (SocketBuffer : r) a -> Sem r a
runSocketBuffering bytesToRead sock = interpret $ \case
    ReadFromSocket  -> do
        bytes <- embed (recv sock bytesToRead)
        addBytes bytes
        consume
        return $ S.null bytes || S.length bytes < bytesToRead
    Handle f       -> do
        ((_, mode, req) :: BufferStateValues) <- get
        resp <- case mode of
            ParsingFinished -> embed $ f req
            _       -> return HTTPResponse.badRequestResponse
        embed $ sendAll sock $ C.pack $ show resp

{-
    Raises the SocketBuffer effect with all required effects
-}
raiseSocketBufferEffect :: Sem (SocketBuffer : r) a -> Sem (SocketBuffer : BufferState : Embed IO : r) a
raiseSocketBufferEffect = raiseUnder2 @BufferState @(Embed IO) @SocketBuffer

evalSocketBuffering :: Sem '[SocketBuffer] a -> Int -> Socket -> IO ()
evalSocketBuffering f i sock = raiseSocketBufferEffect f
      & runSocketBuffering i sock
      & runState ("", ParsingMeta, Request (Headers None []) Empty)
      & runM
      & void
