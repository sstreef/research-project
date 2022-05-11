module HTTP.Types.Request where

import HTTP.Types.General (Payload)

data MethodType = GET | POST
    deriving (Show, Eq)

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

data HTTPResponse = HTTPRequest RequestHeaders Payload


