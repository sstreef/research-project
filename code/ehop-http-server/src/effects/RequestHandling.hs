module Effects.RequestHandling where

import Types.HTTP.Request (HTTPRequest, MethodType)
import Types.HTTP.Response (HTTPResponse, Status(NotFound), createStatusResponse)

import Polysemy ( makeSem, Sem, interpret, Member )
import Polysemy.KVStore as S (KVStore, writeKV, lookupKV)

import qualified Data.Map.Strict as Map


type HTTPHandler = HTTPRequest -> HTTPResponse

{-
    Should register http handlers and allow for resolving methods with paths
-}
data RequestHandling m a where
    Register    :: MethodType -> String -> HTTPHandler -> RequestHandling m ()
    Resolve     :: MethodType -> String -> RequestHandling m (Either HTTPResponse HTTPHandler)


makeSem ''RequestHandling

type HandlerStore = (Map.Map (MethodType, String) HTTPHandler, ())

type HTTPHandlerStore = S.KVStore (MethodType, String) HTTPHandler

runRequestHandling :: Member HTTPHandlerStore r => Sem (RequestHandling : r) a -> Sem r a
runRequestHandling = interpret $ \case
    Register m p h  -> S.writeKV (m, p) h
    (Resolve m p)   -> do
        (result :: Maybe HTTPHandler) <- S.lookupKV (m, p)
        pure $ case result of
           Nothing          -> Left $ createStatusResponse NotFound
           (Just handler)   -> Right handler
