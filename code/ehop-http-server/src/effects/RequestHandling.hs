{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Effects.RequestHandling where

import HTTP.Request (MethodType (GET), HTTPRequest, getMeta)
import HTTP.Response (HTTPResponse)
import qualified HTTP.Response


import Polysemy ( makeSem, Sem, interpret, Members, Embed, runM, raiseUnder, raiseUnder3 )
import Polysemy.KVStore as Store (KVStore, writeKV, lookupKV, runKVStorePurely)

import Polysemy.State as State ( State, get, put, runState )
import Prelude as P

import Data.Functor ((<&>))
import Effects.FileReading (FileReading, readFromFile, runFileReadingIO)
import qualified Data.Map as Map
import Data.Function ( (&) )


type HTTPHandler = HTTPRequest -> IO HTTPResponse

data RequestHandling m a where
    Register            :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    ResolveRequest      :: HTTPRequest -> RequestHandling m (Maybe (IO HTTPResponse))
    ResolveFileRequest  :: HTTPRequest -> RequestHandling m (Maybe (IO HTTPResponse))
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
    ResolveRequest request ->
        case getMeta request of
            Just key    -> do
                            handler <- Store.lookupKV key
                            return (handler <*> Just request)
            Nothing     ->  return $ Just $ pure HTTP.Response.badRequestResponse
    ResolveFileRequest request  -> 
        case getMeta request of
            Just (GET, path)    -> do
                        (staticFilePath :: Maybe String) <- State.get
                        case staticFilePath of
                            Just rootPath -> readFromFile rootPath path <&> Just
                            Nothing       -> return $ Just $ return HTTP.Response.notFoundResponse
            Nothing     ->  return $ Just $ pure HTTP.Response.badRequestResponse
            _           -> return Nothing
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
  & runKVStorePurely (Map.empty :: HTTPHandlerMap)
  & runFileReadingIO
  & runM