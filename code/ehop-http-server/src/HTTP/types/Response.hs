module HTTP.Types.Response (Status, ResponseHeaders) where
    
import HTTP.Types.General (Payload)
    
data Status = OK | BadRequest | NotFound | InternalServerError
    deriving Eq

instance Show Status where
  show OK = "200 OK"
  show BadRequest = "400 Bad Request"
  show NotFound = "404 Not Found"
  show InternalServerError = "500 Internal Server Error"

data ResponseHeaders = ResponseHeaders {
                    version :: String,
                    status :: Status
                  }

instance Show ResponseHeaders where
    show h = unlines [version h, show $ status h]

data HTTPResponse = HTTPResponse ResponseHeaders Payload






