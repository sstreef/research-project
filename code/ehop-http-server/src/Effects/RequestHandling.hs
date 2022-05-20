module Effects.RequestHandling where

import Types.HTTP.Request (MethodType (GET), HTTPRequest, RequestHeaders (RequestHeaders))
import Types.HTTP.Response (HTTPResponse, Status(OK), createContentResponse)
import qualified Types.HTTP.Response as HTTP.Response
import qualified Types.HTTP.Request as HTTP.Request


import Polysemy ( makeSem, Sem, interpret, Members, Embed, raiseUnder, raiseUnder3, runM )
import Polysemy.KVStore as Store (KVStore, writeKV, lookupKV, runKVStorePurely)

import Polysemy.State as State ( State, get, put, runState )
import Prelude as P

import Parsers.File (getFileContentType)
import Data.Functor ((<&>))
import Effects.FileReading (FileReading, readFromFile, runFileReadingIO)
import qualified Data.Map as Map
import Data.Function ( (&) )


type HTTPHandler = HTTPRequest -> HTTPResponse

data RequestHandling m a where
    Register            :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    ResolveRequest      :: HTTPRequest -> RequestHandling m (Maybe HTTPResponse)
    ResolveFileRequest  :: HTTPRequest -> RequestHandling m (Maybe HTTPResponse)
    SetStaticFilePath   :: String -> RequestHandling m ()
    GetStaticFilePath   :: RequestHandling m (Maybe String)

makeSem ''RequestHandling

type HTTPHandlerMap = Map.Map (HTTP.Request.MethodType, String) HTTPHandler

type HTTPHandlerStore = Store.KVStore (MethodType, String) HTTPHandler

type HTTPStaticFilePathState = State.State (Maybe String)

runRequestHandling ::   Members [HTTPHandlerStore, HTTPStaticFilePathState, FileReading] r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register method path handler  -> Store.writeKV (method, path) handler
    ResolveRequest request     ->
            let (RequestHeaders method path _) = HTTP.Request.headersFromRequest request
            in do
                maybeHandler <- Store.lookupKV (method, path)
                return $ maybeHandler <&> \handler -> handler request
    ResolveFileRequest request ->
            let (RequestHeaders method path _)  = HTTP.Request.headersFromRequest request
            in case method of
                GET -> do
                    response <- State.get <&> \case
                        Nothing             -> return HTTP.Response.notFoundResponse
                        Just resourcePath   -> readFromFile (resourcePath ++ path) <&> \case
                            Nothing             -> HTTP.Response.notFoundResponse
                            Just content        -> createContentResponse (getFileContentType path) (Just OK) content
                    Just <$> response
                _ -> return $ Just HTTP.Response.notFoundResponse
    SetStaticFilePath s -> put $ Just s
    GetStaticFilePath   -> get

-- Helpers for this effect

{-
    Raises the RequestHandling effect with all required effects
-}
raising :: Sem (RequestHandling : r) a
  -> Sem (RequestHandling : HTTPStaticFilePathState : HTTPHandlerStore : FileReading : Embed IO : r) a
raising = raiseUnder3 @HTTPStaticFilePathState @HTTPHandlerStore @FileReading @RequestHandling
  . raiseUnder @(Embed IO) @RequestHandling

{-
    Runs the request handling effect with automatically raising the required effects to it.
-}
runRequestHandling' :: Sem '[RequestHandling] () -> IO (HTTPHandlerMap, (Maybe String, ()))
runRequestHandling' setup = raising setup
  & runRequestHandling
  & runState Nothing
  & runKVStorePurely Map.empty
  & runFileReadingIO
  & runM