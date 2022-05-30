module Effects.Buffering where

import Network.Socket (Socket)

import Polysemy (makeSem, Sem, interpret, Member, Embed, embed)
import Network.Socket.ByteString (recv, sendAll)
import Polysemy.State (State, get, put)
import Types.HTTP.Request (MethodType)
import Types.HTTP.General (Payload (Empty, Payload), parseContentType)
import Parsers.HTTP (parseRequestHeader, headerEndToken, requestMethod, requestPath, requestProtocolVersion)
import qualified Data.ByteString.Char8 as C
import Parsers.Parsing as P (many, Parser, string, parse, sat)
import Control.Monad (void)
import qualified Types.HTTP.Response as Types.HTTPResponse
import Types.HTTP.Response ( HTTPResponse )


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
    deriving (Show, Eq)

data HTTPBuffer = HTTPBuffer {
    bufferMode :: BufferMode,
    bufferedBytes :: String,
    bufferedRequest :: HTTPRequest
} deriving Show

getContentInfo :: HTTPHeaders -> [Header]
getContentInfo (Headers _ xs) = xs

setBufferMode :: BufferMode -> HTTPBuffer -> HTTPBuffer
setBufferMode m (HTTPBuffer _ b req) = HTTPBuffer m b req

appendBytesToBuffer :: String -> HTTPBuffer -> HTTPBuffer
appendBytesToBuffer b' (HTTPBuffer m b req) = HTTPBuffer m (b ++ b') req

type BufferState = State HTTPBuffer

data SocketBuffer m a where
    -- Appends a ByteString to the buffer
    ReadFromSocket  :: SocketBuffer m Bool
    Handle          :: (HTTPRequest -> IO HTTPResponse) -> SocketBuffer m ()

makeSem ''SocketBuffer

wrapRemainder :: Foldable t => t a -> (t a, Int)
wrapRemainder s = (s, length s)

runSocketBuffering ::   Member (Embed IO) r =>
                        Member BufferState r =>
                        Int -> Socket -> Sem (SocketBuffer : r) a -> Sem r a
runSocketBuffering bytesToRead sock = interpret $ \case
    ReadFromSocket  -> do
        newBytes        <- embed (C.unpack <$> recv sock bytesToRead)
        embed $ putStrLn $ "NewBytes: " ++ show (length newBytes)
        buffer          <- appendBytesToBuffer newBytes <$> get
        consumedBuffer  <- consume buffer
        put consumedBuffer
        -- This check is currently bugging (suspecting to be coming from parsing ByteString to String but not to UTF-8)
        -- return $ length newBytes < bytesToRead
        let mode = bufferMode consumedBuffer
        return $ mode == FinalBM || mode == ErrorBM || null newBytes
    Handle f       -> do
        (HTTPBuffer mode bytes (Request headers payload)) <- get
        embed $ print $ "Handled buffer: Bytes=\"" ++ bytes ++ "\", mode=" ++ show mode
        resp <- case mode of
            FinalBM | null bytes    -> embed $ f (Request headers payload)
            _                       -> return Types.HTTPResponse.badRequestResponse
        embed . sendAll sock . C.pack . show $ resp

-- Effect helpers

type BufferTransformer = HTTPBuffer -> HTTPBuffer


consume :: Member (Embed IO) r => HTTPBuffer -> Sem r HTTPBuffer
consume buffer = do
    embed $ print $ "FSM ::\t" ++ show (bufferMode buffer) ++ " => " ++ show (bufferMode buffer')
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
