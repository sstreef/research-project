module Effects.Buffering where

import Network.Socket (Socket)
import qualified Data.ByteString as S

import Polysemy (makeSem, Sem, interpret, Member, Embed, embed)
import Network.Socket.ByteString (recv)
import Polysemy.State (State, get, put)
import Types.HTTP.Request (RequestHeaders (RequestHeaders))
import Types.HTTP.General (Payload)
import Parsers.HTTP (parseRequestHeader, headerEndToken, requestMethod, requestPath, requestProtocolVersion)
import qualified Data.ByteString.Char8 as C
import Parsers.Parsing (many, Parser, string, parse)
import Control.Monad (void)
import Data.Maybe (isNothing)



data SocketBuffer m a where
    -- Appends a ByteString to the buffer
    ReadFromSocket  :: SocketBuffer m (Maybe S.ByteString)
    ParseBuffer     :: SocketBuffer m S.ByteString

makeSem ''SocketBuffer

data BufferMode = Meta | Headers | Body | Error

data HTTPBuffer = Buffer BufferMode (String, Int) (Maybe RequestHeaders) (Maybe Payload)

type BufferState = State HTTPBuffer

wrapRemainder :: Foldable t => t a -> (t a, Int)
wrapRemainder s = (s, length s)

runSocketBuffering ::   Member (Embed IO) r =>
                        Member BufferState r =>
                        Int -> Socket -> Sem (SocketBuffer : r) a -> Sem r a
runSocketBuffering bytesToRead sock = interpret $ \case
    ReadFromSocket  -> do
        newBytes <-  embed $ C.unpack <$> recv sock bytesToRead
        (Buffer mode (oldBytes, l) maybeHeaders maybePayload) <- get
        let bufferString = oldBytes ++ newBytes
            in case mode of
                Meta        ->
                    let (maybeHeaders', remainder) = consumeMeta bufferString
                    in do
                        put $ Buffer
                            (if isNothing maybeHeaders' then Meta else Headers)
                            (wrapRemainder remainder)
                            maybeHeaders'
                            maybePayload
                        return Nothing
                _ -> undefined


    ParseBuffer     -> undefined

-- lookupHeaders :: [(String, String)] -> RequestHeaders
-- lookupHeaders xs =

--     where
--         method = lookup ""


-- >>> S.append (C.pack "hello ") (C.pack "world")
-- "hello world"

-- >>> consumeMeta "GET / HTTP/1.0\r\n"
-- (Just (GET,"/","HTTP/1.0"),"")

headerParser :: Parser [(String, String)]
headerParser = do many parseRequestHeader

consumeCRLF :: Parser ()
consumeCRLF = void $ string "\r\n"

parseMeta :: Parser RequestHeaders
parseMeta = do
    headerEndToken $ do
        a <- requestMethod
        b <- requestPath
        RequestHeaders a b <$> requestProtocolVersion

consumeMeta :: String -> (Maybe RequestHeaders, String)
consumeMeta s = case parse parseMeta s of
    [(t, remainder)]    -> (Just t, remainder)
    _                   -> (Nothing, s)

consumeHeaders :: String -> ([(String, String)], String, Bool)
consumeHeaders s = case parse headerParser s of
    [(hs, remainder)]   -> case parse consumeCRLF remainder of
        [(_, remainder')]   -> (hs, remainder', True)
        _                   -> (hs, remainder, False)
    _                   -> ([], s, False)



