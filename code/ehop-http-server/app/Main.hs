module Main where

import Server as HTTPServer ( run )
import HTTP (Headers(Response), Status(OK, BadRequest), ContentType(TextPlain))
import Data.ByteString.Char8 (pack, lines, words)
import qualified Data.ByteString as S

import Prelude hiding (lines, words)

type Handler = S.ByteString -> S.ByteString

customHandler :: Handler
customHandler x = 
    pack $ show $ case head $ words firstHeader  of
        "GET"   -> Response "HTTP/1.0" OK 11 TextPlain "Hello World"
        _       -> Response "HTTP/1.0" BadRequest 5 TextPlain "Error"
    where
        xs = lines x
        firstHeader = if not (null xs) then head xs else S.empty



main :: IO ()
main = run customHandler
