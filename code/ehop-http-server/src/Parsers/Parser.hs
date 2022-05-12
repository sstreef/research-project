module Parsers.Parser where

import Parsers.Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders))
import Data.List (intercalate)
import Types.HTTP.General (Payload(Empty))
import Data.Char

-- Custom Parsers
urlPathSymbol :: Parser Char
urlPathSymbol = char '$'
    <|> char '-'
    <|> char '_'
    <|> char '.'
    <|> char '+'
    <|> char '!'
    <|> char '*'
    <|> char '‘'
    <|> char '('
    <|> char ')'
    <|> char ','
    <|> intToDigit <$> nat
    <|> lower
    <|> upper

requestSymbol :: Parser Char
requestSymbol = urlPathSymbol <|> char '\n' <|> char '\r' <|> char ' ' <|> char ':' <|> char '/'

applyParser :: Parser a -> String -> Maybe a
applyParser p s = case parse p s of
  [(v, "")] -> Just v
  _ -> Nothing

parseMethod :: Parser MethodType
parseMethod = do
        _ <- symbol $ show GET
        return GET 
    <|> do
        _ <- symbol $ show POST
        return POST

parsePath :: Parser String
parsePath =do
    x   <- char '/'
    xs  <- many urlPathSymbol
    ys  <- some $ do
            z <- char '/'
            zs <- some urlPathSymbol
            pure (z : zs)
        <|> do
            z <- char '/'
            pure [z]
    pure (x : xs ++ intercalate "" ys)
    <|> do
    x <- char '/'
    xs <- many urlPathSymbol
    pure (x : xs)

parseVersion :: Parser String
parseVersion = do
    x <- symbol "HTTP/"
    a <- nat
    b <- char '.'
    c <- nat
    pure $ x ++ (show a ++ b : show c)


parseRequest' :: Parser HTTPRequest
parseRequest' = do
    m <- parseMethod
    p <- parsePath
    v <- parseVersion
    _ <- many requestSymbol
    pure $ HTTPRequest (RequestHeaders m p v) Empty

-- >>> parseRequest "GET /hello HTTP/1.1\r\nUser-Agent: PostmanRuntime/7.29.0\r\nAccept: */*\r\nPostman-Token: 63d250f7-ce47-4699-a3f4-fa66d4ff3d4e\r\nHost: localhost:3000\r\n\r\n"
-- Just GET /hello HTTP/1.1

parseRequest :: String -> Maybe HTTPRequest
parseRequest = applyParser parseRequest'
