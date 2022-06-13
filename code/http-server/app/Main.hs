{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Server (runWith)
import HTTP.Request (MethodType(GET, POST), HTTPRequest (Request))
import HTTP.Response ( Status(OK), createJSONResponse, createPlainResponse, createContentResponse, badRequestResponse )
import HTTP.General (Payload(..))
import Data.Char (toUpper)

main :: IO ()
main = runWith Nothing [
    ((GET, "/person"), \_ ->
        return $ createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }"),

    ((GET, "/fox"), \_ ->
        return $ createPlainResponse (Just OK) "The Quick Brown Fox Jumps Over The Lazy Dog."),

    ((POST, "/caps"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (map toUpper (content payload))
                    Empty         -> badRequestResponse),
    
    ((POST, "/add"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (show (sum $ map read (words (content payload))))
                    Empty         -> badRequestResponse),

    ((POST, "/reverse"), \(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (reverse c)
                    Empty         -> badRequestResponse)
    ] (Just "./resources/")
