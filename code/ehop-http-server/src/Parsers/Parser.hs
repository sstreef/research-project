module Parsers.Parser where

import Parsers.Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders))
import Types.HTTP.General (Payload(Empty, Payload), ContentType (TextPlain, ApplicationJson, TextHtml), parseContentType)
import Types.HTTP.Response (Status (BadRequest), HTTPResponse, createStatusResponse)

applyParser :: Parser a -> String -> Maybe a
applyParser p s = case parse p s of
  [(v, "")] -> Just v
  _ -> Nothing

{-
    Custom Symbol Parsers
-}

extendCharParser :: Parser Char -> [Char] -> Parser Char
extendCharParser p s = foldr (\x acc -> char x <|> acc) p (s :: [Char])

-- Note: not all symbols are valid at every place in the url
urlSymbol :: Parser Char
urlSymbol = extendCharParser alphanum "-._~:/?#[]@!$&'()*+,;%="

fileNameSymbol :: Parser Char
fileNameSymbol = extendCharParser alphanum "-_"

requestHeaderSymbol :: Parser Char
requestHeaderSymbol = extendCharParser urlSymbol " :/;=."

{-
    Request Component Parsers
-}

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

parseRequest' :: Parser HTTPRequest
parseRequest' = do
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

    return $ HTTPRequest (RequestHeaders m x y) pl

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


pathLevel :: Parser String
pathLevel = do
    xs  <- some fileNameSymbol
    x   <- char '/'
    return $ xs ++ [x]

filePath :: Parser String
filePath = do
    x  <- char '/'
    xs  <- many pathLevel
    return (x : concat xs)

fileExtensions :: Parser [String]
fileExtensions = many $ do { _ <- char '.'; many alphanum }

file :: Parser FileDescription
file = do
        p <- filePath
        n <- some fileNameSymbol
        extensions <- fileExtensions
        let fileDescription = \e t ->  return $ FileDescription p n e t
        if null extensions
            then fileDescription Nothing TextPlain
            else
                let ext = last extensions
                in fileDescription (Just ext) (parseExtension ext)

getFileContentType :: String -> Maybe ContentType
getFileContentType p = case applyParser file p of
    Nothing -> Nothing
    Just fd -> Just $ contentType fd

parseExtension :: String -> ContentType
parseExtension = \case
    "html" -> TextHtml
    "json" -> ApplicationJson
    _      -> TextPlain
