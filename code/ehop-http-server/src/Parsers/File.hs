module Parsers.File where

import Parsers.Parser (fileNameSymbol, apply)
import Parsers.Parsing
import HTTP.General (ContentType (TextPlain, TextHtml, ApplicationJson))

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
getFileContentType p = case apply file p of
    Nothing -> Nothing
    Just fd -> Just $ contentType fd

parseExtension :: String -> ContentType
parseExtension = \case
    "html" -> TextHtml
    "json" -> ApplicationJson
    _      -> TextPlain