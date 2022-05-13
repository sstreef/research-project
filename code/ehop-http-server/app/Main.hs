module Main where

import Prelude hiding (lines, words)
import Types.HTTP.Response
import Types.HTTP.Request (MethodType (GET))
import Polysemy (Sem, Member)
import Effects.RequestHandling (RequestHandling, register)

import Server (runWith)

main :: IO ()
main = runWith server
    where
        server ::   Member RequestHandling r => Sem r ()
        server = do
            register GET "/person" (\_ ->
                createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }")
                
            register GET "/hello" (\_ -> 
                createHTMLResponse (Just OK) "<h2>Hello World! :-)</h2>")