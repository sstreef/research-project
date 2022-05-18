module Types.HTTP.Request where

import Types.HTTP.General (Payload)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Types.HTTP.Response (Status (MethodNotAllowed))

data MethodType = GET | POST
    deriving (Show, Eq, Ord)

parseMethodType :: ByteString -> Either (String, Status) MethodType
parseMethodType s = case unpack s of
    "GET"   -> Right GET
    "POST"  -> Right POST
    _       -> Left ("405 Method Not Allowed", MethodNotAllowed)

data RequestHeaders = RequestHeaders {
                    -- | The HTTP method used; either GET or POST
                    method :: MethodType,
                    -- | The request path
                    path :: String,
                    -- | The HTTP version used
                    version :: String
                }

instance Show RequestHeaders where
    show h = unwords [show $ method h, path h, version h] ++ "\n"

data HTTPRequest = HTTPRequest RequestHeaders Payload

instance Show HTTPRequest where
    show (HTTPRequest headers payload) = show headers ++ show payload

headersFromRequest :: HTTPRequest -> RequestHeaders
headersFromRequest (HTTPRequest headers _) = headers

getPayload :: HTTPRequest -> Payload
getPayload (HTTPRequest _ payload) = payload