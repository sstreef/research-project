module HTTP.Request where

import HTTP.General (Payload)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import HTTP.Response (Status (MethodNotAllowed))

data MethodType = GET | POST
    deriving (Show, Eq, Ord)

parseMethodType :: ByteString -> Either (String, Status) MethodType
parseMethodType s = case unpack s of
    "GET"   -> Right GET
    "POST"  -> Right POST
    _       -> Left ("405 Method Not Allowed", MethodNotAllowed)

data Meta =  Meta {
        method      :: MethodType,
        path        :: String,
        version     :: String
    } | None
    deriving Show

type Header = (String, String)

data HTTPHeaders = Headers Meta [Header]
    deriving Show

data HTTPRequest = Request HTTPHeaders Payload
    deriving Show

getHeaders :: HTTPRequest -> HTTPHeaders
getHeaders (Request headers _) = headers

getPayload :: HTTPRequest -> Payload
getPayload (Request _ payload) = payload

getContentInfo :: HTTPHeaders -> [Header]
getContentInfo (Headers _ xs) = xs


getMeta :: HTTPRequest -> Maybe (MethodType, String)
getMeta (Request (Headers (Meta m p v) _) _) = Just (m, p)
getMeta _ = Nothing