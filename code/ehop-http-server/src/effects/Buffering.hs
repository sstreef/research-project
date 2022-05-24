module Effects.Buffering where

import Network.Socket (Socket)

import Polysemy (makeSem, Sem, interpret, Member, Embed, embed)
import Network.Socket.ByteString (recv, sendAll)
import Polysemy.State (State, get, put)
import Types.HTTP.Request (MethodType)
import Types.HTTP.General (Payload ())
import Parsers.HTTP (parseRequestHeader, headerEndToken, requestMethod, requestPath, requestProtocolVersion)
import qualified Data.ByteString.Char8 as C
import Parsers.Parsing as P (many, Parser, string, parse)
import Control.Monad (void)
import Data.Maybe ( isNothing )
import Types.HTTP.Response (HTTPResponse (HTTPResponse))
import qualified Types.HTTP.Response as Types.HTTPResponse


data Meta =  Meta {
        method      :: MethodType,
        path        :: String,
        version     :: String
    } | None
    deriving Show

type Header = (String, String)

data HTTPHeaders = Headers Meta [Header]
    deriving Show

data HTTPRequest = Request HTTPHeaders Payload
    deriving Show

data BufferMode = MetaBM | HeadersBM | BodyBM | ErrorBM | FinalBM
    deriving Show

-- data HTTPBuffer = Buffer BufferMode (String, Int) (Maybe RequestHeaders) (Maybe Payload)
data HTTPBuffer = Buffer BufferMode (String, Int) HTTPHeaders Payload
    deriving Show



type BufferState = State HTTPBuffer

data SocketBuffer m a where
    -- Appends a ByteString to the buffer
    ReadFromSocket  :: SocketBuffer m Bool
    Handle          :: (HTTPRequest -> HTTPResponse) -> SocketBuffer m ()

makeSem ''SocketBuffer

wrapRemainder :: Foldable t => t a -> (t a, Int)
wrapRemainder s = (s, length s)

runSocketBuffering ::   Member (Embed IO) r =>
                        Member BufferState r =>
                        Int -> Socket -> Sem (SocketBuffer : r) a -> Sem r a
runSocketBuffering bytesToRead sock = interpret $ \case
    ReadFromSocket  -> do
        newBytes <-  embed $ C.unpack <$> recv sock bytesToRead
        (Buffer mode (oldBytes, i) httpHeaders httpPayload) <- get
        let bufferString = oldBytes ++ newBytes
            in case mode of
                MetaBM        ->
                    let
                        (httpHeaders', remainder) = consumeMeta bufferString
                        nextState = case httpHeaders' of
                            (Headers None _)    -> MetaBM
                            _                   -> HeadersBM
                    in do
                        put $ Buffer nextState (wrapRemainder remainder) httpHeaders' httpPayload
                        return $ null remainder

                HeadersBM     ->
                    let
                        (headers', remainder, isFinished) = consumeHeaders bufferString
                        (Headers meta headers) = httpHeaders
                        hs = (headers ++ headers')
                        httpHeaders' = Headers meta hs
                    in do
                        if isFinished then do
                            put $ Buffer FinalBM (wrapRemainder remainder) httpHeaders' httpPayload
                            return $ isNothing $ lookup "Content-Length" hs
                        else do
                            put $ Buffer HeadersBM (wrapRemainder remainder) httpHeaders' httpPayload
                            return False
                _ -> return True

    Handle f       -> do
        (Buffer mode (oldBytes, _) httpHeaders httpPayload) <- get
        embed $ sendAll sock $ case mode of
            FinalBM | null oldBytes -> wrap $ f (Request httpHeaders httpPayload)
            _       -> wrap Types.HTTPResponse.badRequestResponse

        where
            wrap = C.pack . show

{-
    General Description: Updates the buffer
    
    Input:
    1. a buffer
    2. A function that changes the buffer
    Returns
    1. A new buffer
    2. A boolean indicating if we should move to the next state
-}




-- >>> S.append (C.pack "hello ") (C.pack "world")
-- "hello world"

-- >>> consumeMeta "GET / HTTP/1.0\r\n"
-- (Just (GET,"/","HTTP/1.0"),"")

headerParser :: Parser [(String, String)]
headerParser = do P.many parseRequestHeader

consumeCRLF :: Parser ()
consumeCRLF = void $ P.string "\r\n"

parseMeta :: Parser HTTPHeaders
parseMeta = do
    headerEndToken $ do
        a <- requestMethod
        b <- requestPath
        c <- requestProtocolVersion
        return $ Headers (Meta a b c) []

consumeMeta :: String -> (HTTPHeaders, String)
consumeMeta s = case P.parse parseMeta s of
    [(headers, remainder)]  -> (headers, remainder)
    _                       -> (Headers None [], s)

consumeHeaders :: String -> ([(String, String)], String, Bool)
consumeHeaders s = case P.parse headerParser s of
    [(hs, remainder)]   -> case P.parse consumeCRLF remainder of
        [(_, remainder')]   -> (hs, remainder', True)
        _                   -> (hs, remainder, False)
    _                   -> ([], s, False)



