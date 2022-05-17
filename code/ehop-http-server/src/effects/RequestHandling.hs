module Effects.RequestHandling where

import Types.HTTP.Request (MethodType, getHeaders, RequestHeaders (method, path), HTTPRequest)
import Types.HTTP.Response (HTTPResponse, Status(OK), createContentResponse)
import qualified Types.HTTP.Response as Types.Response


import Polysemy ( makeSem, Sem, interpret, Members )
import Polysemy.KVStore as Store (KVStore, writeKV, lookupKV)

import qualified Data.Map.Strict as Map
import Polysemy.State as State ( State, get, put )
import Prelude as P

import Parsers.Parser (getFileContentType)
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

type HandlerStore = (Map.Map (MethodType, String) HTTPHandler, ())

type HTTPHandlerStore = Store.KVStore (MethodType, String) HTTPHandler

type HTTPStaticFileState = State.State (Maybe String)

runRequestHandling ::   Members [HTTPHandlerStore, HTTPStaticFileState, FileReading] r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register m p h  -> Store.writeKV (m, p) h
    ResolveRequest request     -> let
                headers = getHeaders request
            in do
                result <- Store.lookupKV (method headers, path headers)
                pure (result <&> \f -> f request)
    ResolveFileRequest request -> let
                headers = getHeaders request
                requestPath = path headers
            in do
                (filePath :: Maybe String) <- State.get
                case filePath of
                    Nothing -> pure $ Just Types.Response.notFoundResponse
                    Just p -> do
                        x <-  readFromFile $ p ++ requestPath
                        pure $ Just $ case x of
                            Nothing    -> Types.Response.notFoundResponse
                            Just c   -> createContentResponse (getFileContentType requestPath) (Just OK) c


    SetStaticFilePath s -> put $ Just s
    GetStaticFilePath   -> get

