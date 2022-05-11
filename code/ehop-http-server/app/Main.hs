module Main where

import Server as HTTPServer ( run )
import Data.ByteString.Char8 (words, lines)
import qualified Data.ByteString as S

import Prelude hiding (lines, words)
import HTTP.Types.General (Payload(Payload), ContentType (TextPlain))
import HTTP.Types.Response
    ( HTTPResponse(..), Status(BadRequest, OK), ResponseHeaders (ResponseHeaders) )


customHandler :: S.ByteString -> HTTPResponse
customHandler x = 
    case head $ words firstHeader  of
        "GET"   -> HTTPResponse (ResponseHeaders "HTTP/1.0" OK) $ Payload 11 TextPlain "Hello World"
        _       -> HTTPResponse (ResponseHeaders "HTTP/1.0" BadRequest) $ Payload 5 TextPlain "Error"
    where
        xs = lines x
        firstHeader = if not (null xs) then head xs else S.empty

main :: IO ()
main = run customHandler
