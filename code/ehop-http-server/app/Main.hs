module Main where

import Effects.RequestHandling ( register, setStaticFilePath )
import Server ( runWith, ServerSetup )
import Types.HTTP.Response ( createHTMLResponse, createJSONResponse, Status(OK) )
import Types.HTTP.Request ( MethodType (GET) )


main :: IO ()
main = runWith (Just "80") server
    where
        server :: ServerSetup
        server = do
            setStaticFilePath "resources"
            
            register GET "/person" (\_ ->
                createJSONResponse (Just OK) "{ \"Name\": \"Bob\", \"Age\": 27, \"Gender\": \"Male\" }")
                
            register GET "/hello" (\_ -> 
                createHTMLResponse (Just OK) "<h2>Hello World! :-)</h2>")
                