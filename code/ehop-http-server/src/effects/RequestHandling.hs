module Effects.RequestHandling where

import HTTP.Request (MethodType (GET), HTTPRequest, HTTPHeaders (Headers), Meta (Meta), getHeaders)
import HTTP.Response (HTTPResponse, Status(OK), createContentResponse)
import qualified HTTP.Response


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
    ResolveRequest request      -> case HTTP.Request.getHeaders request of
            (Headers (Meta method path _) _)
                -> do
                    maybeHandler <- Store.lookupKV (method, path)
                    return (maybeHandler <&> \handler -> handler request)
            _   ->  return (Just HTTP.Response.badRequestResponse)
    ResolveFileRequest request  -> case HTTP.Request.getHeaders request of
            (Headers (Meta method path _) _) ->
                case method of 
                    GET -> do
                        response <- State.get <&> \case
                            Nothing         -> return HTTP.Response.notFoundResponse
                            Just rootPath   -> readFromFile (rootPath ++ path) <&> \case
                                Nothing         -> HTTP.Response.notFoundResponse
                                Just content    -> createContentResponse (getFileContentType path) (Just OK) content
                        Just <$> response
                    _   -> return (Just HTTP.Response.notFoundResponse)
            _   ->  return (Just HTTP.Response.badRequestResponse)
    SetStaticFilePath filePath  -> put (Just filePath)
    GetStaticFilePath           -> get

-- Helpers for this effect

{-
    Raises the RequestHandling effect with all required effects
-}
raiseRequestHandlingEffect :: Sem (RequestHandling : r) a
  -> Sem (RequestHandling : HTTPStaticFilePathState : HTTPHandlerStore : FileReading : Embed IO : r) a
raiseRequestHandlingEffect = raiseUnder3 @HTTPStaticFilePathState @HTTPHandlerStore @FileReading @RequestHandling
  . raiseUnder @(Embed IO) @RequestHandling

{-
    Runs the request handling effect with automatically raising the required effects to it.
-}
evalRequestHandling :: Sem '[RequestHandling] () -> IO (HTTPHandlerMap, (Maybe String, ()))
evalRequestHandling setup = raiseRequestHandlingEffect setup
  & runRequestHandling
  & runState Nothing
  & runKVStorePurely Map.empty
  & runFileReadingIO
  & runM