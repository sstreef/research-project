module Parsers.HTTP where

import Parsers.Parsing
import HTTP.Request (MethodType (GET, POST), HTTPRequest (Request), HTTPHeaders (Headers), Meta (Meta))
import HTTP.General (Payload(Empty, Payload), parseContentType)
import HTTP.Response (HTTPResponse)
import qualified HTTP.Response

import Parsers.Parser as P (extendCharParser, apply)


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