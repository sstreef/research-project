module Effects.RequestHandling where

import Types.HTTP.Request (MethodType, HTTPRequest)
import Types.HTTP.Response (HTTPResponse, Status(OK), createContentResponse)
import qualified Types.HTTP.Response as HTTP.Response
import qualified Types.HTTP.Request as HTTP.Request


import Polysemy ( makeSem, Sem, interpret, Members )
import Polysemy.KVStore as Store (KVStore, writeKV, lookupKV)

import Polysemy.State as State ( State, get, put )
import Prelude as P

import Parsers.File (getFileContentType)
import Data.Functor ((<&>))
import Effects.FileReading (FileReading, readFromFile)


type HTTPHandler = HTTPRequest -> HTTPResponse

data RequestHandling m a where
    Register            :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    ResolveRequest      :: HTTPRequest -> RequestHandling m (Maybe HTTPResponse)
    ResolveFileRequest  :: HTTPRequest -> RequestHandling m (Maybe HTTPResponse)
    SetStaticFilePath   :: String -> RequestHandling m ()
    GetStaticFilePath   :: RequestHandling m (Maybe String)

makeSem ''RequestHandling

type HTTPHandlerStore = Store.KVStore (MethodType, String) HTTPHandler

type HTTPStaticFilePathState = State.State (Maybe String)

runRequestHandling ::   Members [HTTPHandlerStore, HTTPStaticFilePathState, FileReading] r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register method path handler  -> Store.writeKV (method, path) handler
    ResolveRequest request     -> 
            let headers = HTTP.Request.headersFromRequest request 
            in do
                result <- Store.lookupKV (HTTP.Request.method headers, HTTP.Request.path headers)
                pure (result <&> \handler -> handler request)
    ResolveFileRequest request -> 
            let headers = HTTP.Request.headersFromRequest request
            in do
                filePath <- State.get
                case filePath of
                    Nothing -> pure $ Just HTTP.Response.notFoundResponse
                    Just p -> do
                        maybeContent <-  readFromFile $ p ++ HTTP.Request.path headers
                        pure $ Just $ case maybeContent of
                            Nothing         -> HTTP.Response.notFoundResponse
                            Just content    -> createContentResponse (getFileContentType (HTTP.Request.path headers)) (Just OK) content


    SetStaticFilePath s -> put $ Just s
    GetStaticFilePath   -> get

