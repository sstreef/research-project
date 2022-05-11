module Main where

import Server as HTTPServer ( run )

import Prelude hiding (lines, words)
import Types.HTTP.General (Payload(Payload), ContentType (TextPlain))
import Types.HTTP.Response
    ( HTTPResponse(..), Status(BadRequest, OK), ResponseHeaders (ResponseHeaders) )
import Types.HTTP.Request (HTTPRequest (HTTPRequest), MethodType (GET), RequestHeaders (method))


handler :: HTTPRequest -> HTTPResponse
handler (HTTPRequest headers _) = case method headers of
    GET ->  HTTPResponse (ResponseHeaders "HTTP/1.0" OK) (Payload 11 TextPlain "Hello World")
    _   -> HTTPResponse (ResponseHeaders "HTTP/1.0" BadRequest) (Payload 5 TextPlain "Error")

main :: IO ()
main = run handler
