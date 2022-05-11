module HTTP where

data Status = OK | BadRequest | NotFound | InternalServerError
    deriving Eq

instance Show Status where
  show OK = "200 OK"
  show BadRequest = "400 Bad Request"
  show NotFound = "404 Not Found"
  show InternalServerError = "500 Internal Server Error"


data MethodType = GET | POST
    deriving (Show, Eq)


data ContentType = TextPlain | TextHtml | ApplicationJson
    deriving Eq

instance Show ContentType where
    show TextPlain = "text/plain"
    show TextHtml = "text/html"
    show ApplicationJson = "application/json"

-- supportedHTTPVersions = [ "HTTP/1.0" ]

data Headers =  Response {
                    -- | The HTTP version
                    version :: String,
                    -- | The status code indicating the request status.
                    status :: Status,
                    -- | The length of the content in bytes
                    contentLength :: Int,
                    -- | The type of content
                    contentType :: ContentType,
                    -- | The content of the response
                    content :: String } |
                Request {
                    -- | The HTTP method used; either GET or POST
                    method :: MethodType,
                    -- | The request path
                    path :: String,
                    -- | The HTTP version
                    version :: String }

instance Show Headers where
    show (Response _version _status _length _type _content) = unlines [
            _version ++ ' ' : show _status,
            "Content-Length: " ++ show _length,
            "Content-Type: " ++ show _type,
            _content
        ]
    show (Request _method _path _version) = unwords [
            show _method, _path, _version
        ]

