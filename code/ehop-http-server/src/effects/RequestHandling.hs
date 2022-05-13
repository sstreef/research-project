module Effects.RequestHandling where

import Types.HTTP.Request (HTTPRequest (HTTPRequest), MethodType (GET, POST), RequestHeaders (version, method, path))
import Types.HTTP.Response (HTTPResponse (HTTPResponse), createResponse, Status(NotFound, OK, InternalServerError))

import Polysemy ( makeSem, Sem, interpret, Member, runM, run )
import Polysemy.KVStore as S (KVStore, writeKV, runKVStorePurely, lookupKV)

import qualified Data.Map.Strict as Map

import Data.Function ((&))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Parsers.Parser (parseRequest)

type HTTPHandler = HTTPRequest -> HTTPResponse

{-
    Should register http handlers and allow for resolving methods with paths
-}
data RequestHandling m a where
    Register    :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    Resolve     :: MethodType -> String -> RequestHandling m (Either HTTPResponse HTTPHandler)


makeSem ''RequestHandling


type HTTPHandlerStore = S.KVStore (MethodType, String) HTTPHandler

runRequestHandling :: Member HTTPHandlerStore r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register m p h  -> S.writeKV (m, p) h
    (Resolve m p)   -> do
        (result :: Maybe HTTPHandler) <- S.lookupKV (m, p)
        pure $ case result of
           Nothing          -> Left $ createResponse "HTTP/1.0" NotFound
           (Just handler)   -> Right handler

-- TESTING

customHandler :: HTTPRequest -> HTTPResponse
customHandler (HTTPRequest headers _) = if not $ null $ version headers
            then createResponse "HTTP/1.0" OK
            else createResponse "HTTP/1.0" InternalServerError

requestRegistration ::  Member RequestHandling r => 
                        Sem r ()
requestRegistration = do
    register GET "/hello" customHandler
    register GET "/world" customHandler
    register POST "/world" customHandler

requestResolve ::   Member RequestHandling r => 
                    S.ByteString -> Sem r S.ByteString
requestResolve s = do
        case parseRequest $ C.unpack s of 
            Left response   -> wrap response
            Right (HTTPRequest h p) -> do
                x <- resolve (method h) (path h)
                case x of
                  Left response -> wrap response
                  Right handler -> wrap $ handler (HTTPRequest h p)
        where
            wrap :: HTTPResponse -> Sem r S.ByteString
            wrap = pure . C.pack . show


combine :: Member RequestHandling r => S.ByteString -> Sem r S.ByteString
combine s = do
    requestRegistration
    requestResolve s

main' :: IO ()
main' = print $ snd execute
    where
        execute = combine "GET /hello HTTP/1.0"
            & runRequestHandling
            & runKVStorePurely Map.empty
            & run
-- program :: Members [Embed IO, RequestHandling] r => 
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

--     return ()
    

type HandlerStore = (Map.Map (MethodType, String) HTTPHandler, ())

main :: IO HandlerStore
main = execute
    where
        execute = requestRegistration
            & runRequestHandling
            & runKVStorePurely Map.empty
            & runM
            & fst
