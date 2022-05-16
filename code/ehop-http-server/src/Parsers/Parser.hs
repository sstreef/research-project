module Parsers.Parser where

import Parsers.Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders, method))
import Data.List (intercalate)
import Types.HTTP.General (Payload(Empty, Payload, content, contentLength), ContentType (TextPlain, ApplicationJson, TextHtml))
import Data.Char
import Types.HTTP.Response (Status (BadRequest), HTTPResponse, createStatusResponse)

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

parseContent :: Parser Payload
parseContent = do
    -- Parse content length
    _ <- symbol "Content-Length:"
    l <- nat
    _ <- some (sat (\c -> c == '\r' || c == '\n'))
    -- Parse content type
    _ <- symbol "Content-Type:"
    t <-    do { _ <- string "text/plain"; pure TextPlain }
        <|> do { _ <- string "text/html"; pure TextHtml }
        <|> do { _ <- string "application/json"; pure ApplicationJson }
    _ <- some (sat (\c -> c == '\r' || c == '\n'))
    -- Parse content
    c <- many (sat (const True))

    if length c == l
    then pure $ Payload l t c
    else pure Empty


-- >>> show $ parseRequest req
-- "Right GET / HTTP/1.0\n\n"


parseRequest' :: Parser HTTPRequest
parseRequest' = do
    m <- parseMethod
    p <- parsePath
    v <- parseVersion
    _ <- many (sat (const True))
    pure $ HTTPRequest (RequestHeaders m p v) Empty

parseRequest :: String -> Either HTTPResponse HTTPRequest
parseRequest s = case applyParser parseRequest' s of
    (Just (HTTPRequest h pl)) -> Right $ HTTPRequest h pl
    _   -> Left $ createStatusResponse BadRequest
