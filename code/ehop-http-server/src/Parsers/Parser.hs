module Parser where

import Parsing
import Types.HTTP.Request (MethodType (GET, POST), HTTPRequest (HTTPRequest), RequestHeaders (RequestHeaders))
import Data.List (intercalate)
import Types.HTTP.General (Payload(Empty))

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

applyParser :: Parser a -> String -> Maybe a
applyParser p s = case parse p s of
  [(v, "")] -> Just v
  _ -> Nothing

parseMethod :: Parser MethodType
parseMethod = do
    _ <- symbol $ show GET
    return GET <|> do
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
    

parseRequest :: Parser HTTPRequest
parseRequest = do
    m <- parseMethod
    p <- parsePath
    v <- parseVersion
    pure $ HTTPRequest (RequestHeaders m p v) Empty
