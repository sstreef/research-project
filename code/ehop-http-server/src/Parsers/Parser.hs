module Parsers.Parser where

import Parsers.Parsing

{-
    Parser Helpers
-}

apply :: Parser a -> String -> Maybe a
apply parser str = case parse parser str of
  [(v, "")] -> Just v
  _         -> Nothing

extendCharParser :: Parser Char -> [Char] -> Parser Char
extendCharParser p s = foldr (\x acc -> char x <|> acc) p (s :: [Char])

{-
    Custom Symbol Parsers
-}

-- Note: not all symbols are valid at every place in a URL.
urlSymbol :: Parser Char    
urlSymbol = extendCharParser alphanum "-._~:/?#[]@!$&'()*+,;%="

fileNameSymbol :: Parser Char
fileNameSymbol = extendCharParser alphanum "-_"

requestHeaderSymbol :: Parser Char
requestHeaderSymbol = extendCharParser urlSymbol " :/;=."


