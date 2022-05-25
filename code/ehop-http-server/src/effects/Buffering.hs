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
    isFinalChunk :: Bool,
    bufferedRequest :: HTTPRequest
}
    deriving Show

hModeBuffer = HTTPBuffer HeadersBM
mModeBuffer = HTTPBuffer MetaBM
bModeBuffer = HTTPBuffer BodyBM
eModeBuffer = HTTPBuffer ErrorBM
fModeBuffer = HTTPBuffer FinalBM

setBufferMode :: BufferMode -> HTTPBuffer -> HTTPBuffer
setBufferMode m (HTTPBuffer _ b ifc req) = HTTPBuffer m b ifc req

appendBytesToBuffer :: String -> HTTPBuffer -> HTTPBuffer
appendBytesToBuffer b' (HTTPBuffer m b ifc req) = HTTPBuffer m (b ++ b') ifc req

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
        newBytes        <- embed (C.unpack <$> recv sock bytesToRead)
        buffer          <- appendBytesToBuffer newBytes <$> get
        consumedBuffer  <- fsm buffer
        put consumedBuffer
        return $ length newBytes < bytesToRead
        where
            fsm :: Member (Embed IO) r => HTTPBuffer -> Sem r HTTPBuffer
            fsm buffer = do
                embed $ print $ bufferMode buffer
                let
                    handlers = [(MetaBM, consumeMeta), (HeadersBM, consumeHeaders), (BodyBM, consumeBody)]
                    mode = bufferMode buffer
                    in case lookup (bufferMode buffer) handlers of
                        Nothing         -> return buffer
                        Just consume    ->
                            let buffer' = consume buffer
                            in if bufferMode buffer' == mode
                                then return buffer'
                                else fsm buffer'

    Handle f       -> do
        (HTTPBuffer mode bytes _ (Request headers payload)) <- get
        embed $ sendAll sock $ case mode of
            BodyBM                  -> wrap $ f (Request headers payload)
            FinalBM | null bytes    -> wrap $ f (Request headers payload)
            _       -> wrap Types.HTTPResponse.badRequestResponse

        where
            wrap = C.pack . show

-- Effect helpers

type BufferTransformer = HTTPBuffer -> HTTPBuffer

consumeMeta :: BufferTransformer
consumeMeta (HTTPBuffer _ bytes ifc (Request _ payload)) =
    case P.parse parseMeta bytes of
        [(headers', bytes')]    -> hModeBuffer bytes' ifc (Request headers' payload)
        _                       -> mModeBuffer bytes ifc (Request (Headers None []) payload)

consumeHeaders :: BufferTransformer
consumeHeaders (HTTPBuffer _ bytes ifc (Request (Headers meta oldHs) payload)) = case P.parse headerParser bytes of
    [(hs, remainder)]   -> case P.parse consumeCRLF remainder of
        [(_, remainder')]   -> bModeBuffer remainder' ifc (Request (Headers meta (oldHs++hs)) payload)
        _                   -> hModeBuffer remainder ifc (Request (Headers meta (oldHs++hs)) payload)
    _                   -> hModeBuffer bytes ifc (Request (Headers meta oldHs) payload)

consumeBody :: BufferTransformer
consumeBody (HTTPBuffer _ bytes ifc (Request (Headers meta hs) payload)) =
    case payload of
        Payload l' t' c'    -> case P.parse bodyParser bytes of
            [(c, bytes')]   -> case combineContents c' c l' of
                Left _          -> errorBuffer
                Right s         -> bModeBuffer bytes' ifc (Request (Headers meta hs) (Payload l' t' s))
            _               -> errorBuffer
        Empty               -> case (lookup "Content-Length" hs, lookup "Content-Type" hs) of
            (Just l', t')  -> case P.parse bodyParser bytes of
                [(c, bytes')]       -> let l = read l' :: Int
                    in if (length c < l && not ifc) || (length c == l && ifc)
                        then bModeBuffer bytes' ifc (Request (Headers meta hs) (Payload l (parseContentType t') c))
                        else errorBuffer
                _                   -> errorBuffer
            _                   -> fModeBuffer bytes ifc (Request (Headers meta hs) Empty)

    where
        combineContents :: String -> String -> Int -> Either String String
        combineContents a b l = let c = a ++ b
            in if (length c < l && not ifc) || (length c == l && ifc) then Right c else Left c

        errorBuffer = eModeBuffer bytes ifc (Request (Headers meta hs) payload)

bodyParser :: Parser String
bodyParser =  many (sat (const True))

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
