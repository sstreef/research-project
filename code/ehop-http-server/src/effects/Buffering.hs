module Effects.Buffering where

import Network.Socket (Socket)

import Polysemy (makeSem, Sem, interpret, Member, Embed, embed, runM, raiseUnder2)
import Network.Socket.ByteString (recv, sendAll)
import Polysemy.State (State, get, put, runState)
import HTTP.Request (HTTPRequest (Request), HTTPHeaders (Headers), Meta (None, Meta))
import HTTP.General (Payload (Empty, Payload), parseContentType)
import Parsers.HTTP (parseRequestHeader, headerEndToken, requestMethod, requestPath, requestProtocolVersion)
import qualified Data.ByteString.Char8 as C
import Parsers.Parsing as P (many, Parser, string, parse, sat)
import Control.Monad (void)
import qualified HTTP.Response as HTTPResponse
import HTTP.Response ( HTTPResponse )
import Data.Function ((&))

data BufferMode = MetaBM | HeadersBM | BodyBM | ErrorBM | FinalBM
    deriving (Show, Eq)

data HTTPBuffer = HTTPBuffer {
    bufferMode :: BufferMode,
    bufferedBytes :: String,
    bufferedRequest :: HTTPRequest
} deriving Show

appendBytesToBuffer :: String -> HTTPBuffer -> HTTPBuffer
appendBytesToBuffer b' (HTTPBuffer m b req) = HTTPBuffer m (b ++ b') req

data SocketBuffer m a where
    -- Appends a ByteString to the buffer
    ReadFromSocket  :: SocketBuffer m Bool
    Handle          :: (HTTPRequest -> IO HTTPResponse) -> SocketBuffer m ()

makeSem ''SocketBuffer

runSocketBuffering ::   Member (Embed IO) r =>
                        Member (State HTTPBuffer) r =>
                        Int -> Socket -> Sem (SocketBuffer : r) a -> Sem r a
runSocketBuffering bytesToRead sock = interpret $ \case
    ReadFromSocket  -> do
        newBytes        <- embed (C.unpack <$> recv sock bytesToRead)
        buffer          <- appendBytesToBuffer newBytes <$> get
        consumedBuffer  <- consume buffer
        put consumedBuffer
        let mode = bufferMode consumedBuffer
        return (mode == FinalBM || mode == ErrorBM || null newBytes)
    Handle f       -> do
        (HTTPBuffer mode bytes (Request headers payload)) <- get
        resp <- case mode of
            FinalBM | null bytes    -> embed $ f (Request headers payload)
            _                       -> return HTTPResponse.badRequestResponse
        embed . sendAll sock . C.pack . show $ resp

{-
    Raises the SocketBuffer effect with all required effects
-}
raiseSocketBufferEffect :: Sem (SocketBuffer : r) a -> Sem (SocketBuffer : State HTTPBuffer : Embed IO : r) a
raiseSocketBufferEffect = raiseUnder2 @(State HTTPBuffer) @(Embed IO) @SocketBuffer

evalSocketBuffering :: Sem '[SocketBuffer] a -> Int -> Socket -> IO ()
evalSocketBuffering f i sock = raiseSocketBufferEffect f
      & runSocketBuffering i sock
      & runState (HTTPBuffer MetaBM "" (Request (Headers None []) Empty))
      & runM
      & void

-- Effect helpers

type BufferTransformer = HTTPBuffer -> HTTPBuffer

consume :: Member (Embed IO) r => HTTPBuffer -> Sem r HTTPBuffer
consume buffer = do
    if mode == bufferMode buffer'
        then return buffer'
        else consume buffer'
    where
        mode = bufferMode buffer
        buffer' = (case mode of
            MetaBM      -> consumeMeta
            HeadersBM   -> consumeHeaders
            BodyBM      -> consumeBody
            _           -> id) buffer

consumeMeta :: BufferTransformer
consumeMeta (HTTPBuffer _ bytes (Request _ payload)) =
    case P.parse parseMeta bytes of
        [(headers', bytes')]    -> HTTPBuffer HeadersBM bytes' (Request headers' payload)
        _                       -> HTTPBuffer MetaBM bytes (Request (Headers None []) payload)

consumeHeaders :: BufferTransformer
consumeHeaders (HTTPBuffer _ bytes (Request (Headers meta oldHs) payload)) = case P.parse headerParser bytes of
    [(hs, remainder)]   -> case P.parse consumeCRLF remainder of
        [(_, remainder')]   -> HTTPBuffer BodyBM remainder' (Request (Headers meta (oldHs++hs)) payload)
        _                   -> HTTPBuffer HeadersBM remainder (Request (Headers meta (oldHs++hs)) payload)
    _                   -> HTTPBuffer HeadersBM bytes (Request (Headers meta oldHs) payload)

consumeBody :: BufferTransformer
consumeBody (HTTPBuffer _ bytes (Request (Headers meta hs) payload)) =
    case payload of
        Payload l' t' c'    -> case P.parse bodyParser bytes of
            [(c, bytes')]   -> case c' ++ c of
                s | length s > l'   -> errorBuffer
                s | length s == l'  -> HTTPBuffer FinalBM bytes' (Request (Headers meta hs) (Payload l' t' s))
                s                   -> HTTPBuffer BodyBM bytes' (Request (Headers meta hs) (Payload l' t' s))
            _               -> errorBuffer
        Empty               -> case (lookup "Content-Length" hs, lookup "Content-Type" hs) of
            (Just l', t')  -> case P.parse bodyParser bytes of
                [(c, bytes')]       -> let l = read l' :: Int
                    in if (length c < l) || (length c == l)
                        then HTTPBuffer BodyBM bytes' (Request (Headers meta hs) (Payload l (parseContentType t') c))
                        else errorBuffer
                _                   -> errorBuffer
            _                   -> HTTPBuffer FinalBM bytes (Request (Headers meta hs) Empty)

    where
        errorBuffer = HTTPBuffer ErrorBM bytes (Request (Headers meta hs) payload)

bodyParser :: Parser String
bodyParser =  many (sat (const True))

headerParser :: Parser [(String, String)]
headerParser = P.many parseRequestHeader

consumeCRLF :: Parser ()
consumeCRLF = void (P.string "\r\n")

parseMeta :: Parser HTTPHeaders
parseMeta = headerEndToken $ do
        a <- requestMethod
        b <- requestPath
        c <- requestProtocolVersion
        return (Headers (Meta a b c) [])
