module Effects.RequestHandling where

import Types.HTTP.Request (HTTPRequest (HTTPRequest), MethodType (GET, POST), RequestHeaders (version))
import Types.HTTP.Response (HTTPResponse, createResponse, Status(NotFound, OK, InternalServerError))

import Polysemy ( makeSem, Sem, interpret, Member, Members, Embed, runM, embed )
import Polysemy.KVStore as S (KVStore, writeKV, runKVStorePurely, lookupKV)

import qualified Data.Map.Strict as Map

import Data.Function ((&))

type HTTPHandler = HTTPRequest -> HTTPResponse

{-
    Should register http handlers and allow for resolving methods with paths
-}
data RequestHandling m a where
    Register    :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    Resolve     :: MethodType -> String -> RequestHandling m (Either HTTPResponse HTTPHandler)


makeSem ''RequestHandling


type HTTPHandlerStore = S.KVStore (MethodType, String) HTTPHandler

runRequestHandling :: Member (S.KVStore (MethodType, String) HTTPHandler) r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register m p h  -> S.writeKV (m, p) h
    (Resolve method path)           -> do
        (result :: Maybe HTTPHandler) <- S.lookupKV (method, path)
        pure $ case result of
           Nothing          -> Left $ createResponse "HTTP/1.0" NotFound
           (Just handler)   -> Right handler



-- program :: Members [Embed IO, RequestHandling, HTTPHandlerStore] r => 
--             Sem r ()
-- program = do
--     register GET "/hello" customHandler
--     register GET "/world" customHandler
--     register POST "/world" customHandler

--     x <- resolve GET "/hello"

--     _ <- embed $ case x of 
--         (Left a) -> print a
--         _ -> print ("Handler not printable":: String)

--     y <- resolve GET "/"
--     _ <- embed $ case y of 
--         (Left a) -> print a
--         _ ->  print ("Handler not printable" :: String)
    
--     pure ()

--     where
--         customHandler :: HTTPRequest -> HTTPResponse
--         customHandler (HTTPRequest headers _) = if not $ null $ version headers
--             then createResponse "HTTP/1.0" OK
--             else createResponse "HTTP/1.0" InternalServerError

-- main :: IO (Map.Map (MethodType, String) HTTPHandler, ())
-- main = execute
--     where
--         execute = program
--             & runRequestHandling
--             & runKVStorePurely Map.empty
--             & runM