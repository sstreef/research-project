module Main where

import Server ( runWith )
import HTTP.Request ( HTTPRequest (Request), MethodType (GET, POST) )
import HTTP.Response ( createPlainResponse, createJSONResponse, createContentResponse, Status(OK), badRequestResponse )
import HTTP.General ( Payload(Payload, Empty) )
import Data.Char ( toUpper )

main :: IO ()
main = runWith Nothing [
    ((GET, "/api/person"), \_ ->
        return $ createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }"),

    ((GET, "/api/fox"), \_ ->
        return $ createPlainResponse (Just OK) "The Quick Brown Fox Jumps Over The Lazy Dog."),

    ((POST, "/api/caps"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (map toUpper c)
                    Empty         -> badRequestResponse),
    
    ((POST, "/api/add"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (show (sum $ map read (words c)))
                    Empty         -> badRequestResponse),

    ((POST, "/api/reverse"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (reverse c)
                    Empty         -> badRequestResponse)
    ] (Just "./resources/")
