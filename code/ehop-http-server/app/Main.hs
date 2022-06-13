module Main where

import Server ( runWith, HTTPServer )
import HTTP.Request ( HTTPRequest (Request), MethodType (GET, POST) )
import HTTP.Response ( createPlainResponse, createJSONResponse, createContentResponse, Status(OK), badRequestResponse )
import HTTP.General ( Payload(Payload, Empty) )
import Data.Char ( toUpper )
import Effects.RequestHandling ( register, setStaticFilePath )

main :: IO ()
main = runWith (Just "80") server
    where
        server :: HTTPServer
        server = do
            setStaticFilePath "./resources/"
            
            register GET "/api/person" (\_ -> do
                return $ createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }")
                
            register GET "/api/fox" (\_ -> do
                return $ createPlainResponse (Just OK) "The Quick Brown Fox Jumps Over The Lazy Dog.")
            
            register POST "/api/caps" (\(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (map toUpper c)
                    Empty         -> badRequestResponse)
            
            register POST "/api/add" (\(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (show (sum $ map read (words c)))
                    Empty         -> badRequestResponse)
            
            register POST "/api/reverse" (\(Request _ payload) -> do
                return $ case payload of
                    Payload _ t c -> createContentResponse (Just t) (Just OK) (reverse c)
                    Empty         -> badRequestResponse)