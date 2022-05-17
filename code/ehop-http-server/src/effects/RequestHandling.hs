module Effects.RequestHandling where

import Types.HTTP.Request (MethodType, getHeaders, RequestHeaders (method, path), HTTPRequest)
import Types.HTTP.Response (HTTPResponse, Status(NotFound, OK), createStatusResponse, createContentResponse)

import Polysemy ( makeSem, Sem, interpret, Members, embed, Embed )
import Polysemy.KVStore as S (KVStore, writeKV, lookupKV)

import qualified Data.Map.Strict as Map
import Polysemy.State as State ( State, get, put )
import Prelude as P

import qualified Control.Exception as E
import Parsers.Parser (getFileContentType)


type HTTPHandler = HTTPRequest -> HTTPResponse

data RequestHandling m a where
    Register            :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    Resolve             :: HTTPRequest -> RequestHandling m HTTPResponse
    SetStaticFilePath   :: String -> RequestHandling m ()
    GetStaticFilePath   :: RequestHandling m (Maybe String)


makeSem ''RequestHandling

type HandlerStore = (Map.Map (MethodType, String) HTTPHandler, ())

type HTTPHandlerStore = S.KVStore (MethodType, String) HTTPHandler

type HTTPStaticFileState = State.State (Maybe String)

runRequestHandling ::   Members [Embed IO, HTTPHandlerStore, HTTPStaticFileState] r =>
                        Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register m p h  -> S.writeKV (m, p) h
    Resolve req     -> do
            (result :: Maybe HTTPHandler) <- S.lookupKV (method headers, requestPath)
            case result of
                Just handler -> pure $ handler req
                Nothing -> do
                    (filePath :: Maybe String) <- State.get
                    case filePath of
                        Nothing -> pure $ createStatusResponse NotFound
                        Just p -> do
                            x <-  embed (E.try $ P.readFile $ p ++ requestPath :: IO (Either IOError String))
                            pure $ case x of
                              Left _    -> createStatusResponse NotFound
                              Right c   -> createContentResponse (getFileContentType requestPath) (Just OK) c
        where
            headers = getHeaders req
            requestPath = path headers
    SetStaticFilePath s -> put $ Just s
    GetStaticFilePath   -> get

