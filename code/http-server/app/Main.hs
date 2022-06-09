module Main where

import Server (runWith)
import HTTP.Request (MethodType(GET, POST), HTTPRequest (Request))
import HTTP.Response ( Status(OK), createJSONResponse, createPlainResponse, createContentResponse, badRequestResponse )
import HTTP.General (Payload(..))

main :: IO ()
main = runWith Nothing [
    ((GET, "/person"), \_ -> 
        createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }"),

    ((GET, "/hello"), \_ -> 
        createPlainResponse (Just OK) "The Quick Brown Fox Jumps Over The Lazy Dog."),

    ((POST, "/mirror"), \(Request _ payload) -> 
                case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) c
                    Empty         -> badRequestResponse)
    ] Nothing
