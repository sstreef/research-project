module Parsers.HTTP where

import Parsers.Parsing
import HTTP.Request (MethodType (GET, POST), HTTPRequest (Request), HTTPHeaders (Headers), Meta (Meta, None))
import HTTP.General (Payload(Empty, Payload), parseContentType)
import HTTP.Response (HTTPResponse)
import qualified HTTP.Response

import Parsers.Parser as P (extendCharParser, apply)

import Control.Monad.State
import qualified Control.Applicative as P
import qualified Parsers.Parsing as P

{- Custom -}

urlSymbol :: Parser Char
urlSymbol = P.extendCharParser alphanum "-._~:/?#[]@!$&'()*+,;%="

requestHeaderSymbol :: Parser Char
requestHeaderSymbol = P.extendCharParser urlSymbol " :/;=."

requestMethod :: Parser MethodType
requestMethod = methodParser GET <|> methodParser POST
    where
        methodParser :: MethodType -> Parser MethodType
        methodParser t = do { _ <- symbol $ show t; return t }

requestPath :: Parser String
requestPath = do
    x   <- char '/'
    xs  <- many urlSymbol
    ys  <- some $ do
            z <- char '/'
            zs <- some urlSymbol
            return (z : zs)
        <|> do
            z <- char '/'
            return [z]
    return (x : xs ++ concat ys)
    <|> do
    x <- char '/'
    xs <- many urlSymbol
    return (x : xs)

requestProtocolVersion :: Parser String
requestProtocolVersion = do
    x <- symbol "HTTP/"
    a <- nat
    b <- char '.'
    c <- nat
    return $ x ++ (show a ++ b : show c)

parseRequestHeader :: Parser (String, String)
parseRequestHeader = headerEndToken $ do
    x <- upper
    xs <- many letter
    xss <- many (do
        _ <- char '-'
        y <- upper
        ys <- many letter
        return $ '-' : (y : ys))
    _ <- char ':'
    _ <- many $ char ' '
    val <- some requestHeaderSymbol
    return (x : xs ++ concat xss, val)

headerEndToken :: Parser a -> Parser a
headerEndToken p = do
    v <- p
    _ <- char '\r'
    _ <- char '\n'
    return v

requestHeaders :: Parser ((MethodType, String, String), [(String, String)])
requestHeaders = do
    meta <- headerEndToken $ do
        a <- requestMethod
        b <- requestPath
        c <- requestProtocolVersion
        return (a, b, c)
    headers <- many parseRequestHeader
    return (meta, headers)

request :: Parser HTTPRequest
request = do
    ((m, x, y), xs) <- requestHeaders
    contents <- many (sat (const True))
    pl <- let
            cLength = lookup "Content-Length" xs
            cType = lookup "Content-Type" xs
        in
            return $ case cLength of
                Just i  | m == POST ->
                    Payload (read i) (parseContentType cType) contents
                _ -> Empty

    return $ Request (Headers (Meta m x y) []) pl

parseRequest :: String -> Either HTTPResponse HTTPRequest
parseRequest s = case P.apply request s of
    (Just req)  -> Right req
    _           -> Left HTTP.Response.badRequestResponse



-- Custom added for buffering
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


data BufferMode = ParsingMeta | ParsingHeaders | ParsingBody | ParsingError | ParsingFinished
    deriving (Show, Eq)


type BufferValue = HTTPRequest
type BufferState' = (String, BufferMode, HTTPRequest)

type BufferState = State BufferState' BufferValue

addBytes :: String -> BufferState
addBytes bytes' = do
    (bytes, mode, request) <- get
    put (bytes ++ bytes', mode, request)
    return request


emptyRequest :: HTTPRequest
emptyRequest = Request (Headers None []) Empty

emptyState :: BufferState'
emptyState = ("POST / HTTP/1.0\r\nContent-Length: 12\r\nContent-Type: text/plain\r\n\r\nHello World!", ParsingMeta, emptyRequest)

consume :: BufferState
consume = do
    (_, mode, request) <- get
    case mode of
        ParsingMeta     -> consumeMeta
        ParsingHeaders  -> consumeHeaders
        ParsingBody     -> consumeBody
        _           -> return request
    (_, mode', request') <- get
    if mode == mode' then return request' else consume

res = execState consume emptyState

-- >>> show res
-- "(\"\",ParsingBody,Request (Headers (Meta {method = POST, path = \"/\", version = \"HTTP/1.0\"}) [(\"Content-Length\",\"12\"),(\"Content-Type\",\"text/plain\")]) Content-Length: 12\nContent-Type: text/plain\n\nHello World!\n)"

getBufferStateValue :: BufferState
getBufferStateValue = do
    (_, _, request) <- get
    return request

consumeMeta :: BufferState
consumeMeta = do
    (bytes, _, Request _ payload) <- get
    put $ case P.parse parseMeta bytes of
        [(headers', bytes')]    -> (bytes', ParsingHeaders, Request headers' payload)
        _                       -> (bytes, ParsingMeta, Request (Headers None []) payload)
    getBufferStateValue

consumeHeaders :: BufferState
consumeHeaders = do
    (bytes, mode, Request (Headers meta oldHeaders) payload) <- get
    put $ case P.parse headerParser bytes of
        [(headers, remainder)]  -> case P.parse consumeCRLF remainder of
            [(_, remainder')]       -> (remainder', ParsingBody, Request (Headers meta (oldHeaders++headers)) payload)
            _                       -> (remainder, ParsingHeaders, Request (Headers meta (oldHeaders++headers)) payload)
        _                       -> (bytes, ParsingHeaders, Request (Headers meta oldHeaders) payload)
    getBufferStateValue

consumeBody :: BufferState
consumeBody = do
    (bytes, mode, Request (Headers meta headers) payload) <- get
    let errorState = (bytes, ParsingError, Request (Headers meta headers) payload)
    put $ case payload of
        Payload l' t' c'    -> case P.parse bodyParser bytes of
            [(c, bytes')]   -> case c' ++ c of
                s | length s > l'   -> errorState
                s | length s == l'  -> (bytes', ParsingFinished, Request (Headers meta headers) (Payload l' t' s))
                s                   -> (bytes', ParsingBody, Request (Headers meta headers) (Payload l' t' s))
            _               -> errorState
        Empty               -> case (lookup "Content-Length" headers, lookup "Content-Type" headers) of
            (Just l', t')  -> case P.parse bodyParser bytes of
                [(c, bytes')]       -> let l = read l' :: Int
                    in if (length c < l) || (length c == l)
                        then (bytes', ParsingBody, Request (Headers meta headers) (Payload l (parseContentType t') c))
                        else errorState
                _                   -> errorState
            _                   -> (bytes, ParsingFinished, Request (Headers meta headers) Empty)
    getBufferStateValue

