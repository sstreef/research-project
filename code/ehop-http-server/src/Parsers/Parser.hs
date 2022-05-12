module Parsers.Parser where

import Parsers.Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders))
import Data.List (intercalate)
import Types.HTTP.General (Payload(Empty, Payload), ContentType (TextPlain))
import Data.Char
import Types.HTTP.Response (Status (BadRequest), createResponse, HTTPResponse (HTTPResponse))

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

parseRequest :: String -> Either HTTPResponse HTTPRequest
parseRequest s = case applyParser parseRequest' s of
    Nothing     -> Left $ createResponse "HTTP/1.0" BadRequest
    (Just x)    -> Right x