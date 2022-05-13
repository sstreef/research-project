module Main where

import Server as HTTPServer ( run )

import Prelude hiding (lines, words)
import Types.HTTP.General (Payload(Payload), ContentType (TextPlain))
import Types.HTTP.Response
    ( HTTPResponse(..), Status(BadRequest, OK, InternalServerError), ResponseHeaders (ResponseHeaders), createResponse )
import Types.HTTP.Request (HTTPRequest (HTTPRequest), MethodType (GET), RequestHeaders (method, version))
import Polysemy (Sem, Member)
import Effects.RequestHandling (RequestHandling, register)

import EffectServer (runWithEffects)


handler :: HTTPRequest -> HTTPResponse
handler (HTTPRequest headers _) = case method headers of
    GET ->  HTTPResponse (ResponseHeaders "HTTP/1.0" OK) (Payload 11 TextPlain "Hello World")
    _   ->  HTTPResponse (ResponseHeaders "HTTP/1.0" BadRequest) (Payload 5 TextPlain "Error")

customHandler :: HTTPRequest -> HTTPResponse
customHandler (HTTPRequest headers _) = if not $ null $ version headers
            then createResponse "HTTP/1.0" OK
            else createResponse "HTTP/1.0" InternalServerError


-- main :: IO ()
-- main = run handler

main :: IO ()
main = runWithEffects server

server ::   Member RequestHandling r => 
            Sem r ()
server = do
    register GET "/test" customHandler
    register GET "/hello" customHandler