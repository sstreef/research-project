module Parsers.Parser where

import Parsers.Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders))
import Data.List (intercalate)
import Types.HTTP.General (Payload(Empty, Payload), ContentType (TextPlain, ApplicationJson, TextHtml))
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

fileSymbol :: Parser Char
fileSymbol =  char '-'
    <|> char '_'
    <|> alphanum

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


data FileDescription = FileDescription {
    path :: String,
    name :: String,
    extension :: Maybe String,
    contentType :: ContentType }
    deriving (Show)


parsePathLevel :: Parser String
parsePathLevel = do
    xs  <- some urlPathSymbol
    x   <- char '/'
    pure $ xs ++ [x]

parseFilePath :: Parser String
parseFilePath = do
    x  <- char '/'
    xs  <- many parsePathLevel
    pure $ x : intercalate "" xs

parseFile' :: Parser FileDescription
parseFile' = do
        p <- parseFilePath
        n <- some fileSymbol
        extensions <- parseFileExtensions

        let fileDescription = \e t ->  pure $ FileDescription p n e t
        if null extensions
            then fileDescription Nothing TextPlain
            else
                let ext = last extensions
                in fileDescription (Just ext) (parseExtension ext)

getFileContentType :: String -> Maybe ContentType
getFileContentType p = case applyParser parseFile' p of
    Nothing -> Nothing
    Just fd -> Just $ contentType fd


parseFileExtensions :: Parser [String]
parseFileExtensions = do
    many (do
        _ <- char '.'
        many alphanum)

parseExtension :: String -> ContentType
parseExtension = \case
    "html" -> TextHtml
    "json" -> ApplicationJson
    _      -> TextPlain
