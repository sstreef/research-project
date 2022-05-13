module Main where

import Effects.RequestHandling ( register )
import Prelude hiding (lines, words)
import Server ( runWith, ServerSetup )
import Types.HTTP.Response ( createHTMLResponse, createJSONResponse, Status(OK) )
import Types.HTTP.Request ( MethodType (GET) )


main :: IO ()
main = runWith server
    where
        server :: ServerSetup
        server = do
            register GET "/person" (\_ ->
                createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }")
                
            register GET "/hello" (\_ -> 
                createHTMLResponse (Just OK) "<h2>Hello World! :-)</h2>")
                