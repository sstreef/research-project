module Main where

import Effects.RequestHandling ( register, setStaticFilePath )
import Server ( runWith, HTTPServer )
import Types.HTTP.Response ( createPlainResponse, createJSONResponse, createContentResponse, Status(OK), badRequestResponse )
import Types.HTTP.Request ( MethodType (GET, POST) )
import Types.HTTP.General ( Payload(Payload, Empty) )
import Effects.Buffering (HTTPRequest (Request))


main :: IO ()
main = runWith (Just "80") server
    where
        server :: HTTPServer
        server = do
            setStaticFilePath "resources"
            
            register GET "/person" (\_ ->
                createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }")
                
            register GET "/hello" (\_ -> 
                createPlainResponse (Just OK) "The Quick Brown Fox Jumps Over The Lazy Dog.")
            
            register POST "/mirror" (\(Request _ payload) -> 
                case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) c
                    Empty         -> badRequestResponse)
                