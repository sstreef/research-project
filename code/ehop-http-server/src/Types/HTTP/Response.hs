module Types.HTTP.Response where
    
import Types.HTTP.General (Payload)
    
data Status = OK | BadRequest | NotFound | InternalServerError | MethodNotAllowed
    deriving Eq

instance Show Status where
  show OK = "200 OK"
  show BadRequest = "400 Bad Request"
  show NotFound = "404 Not Found"
  show InternalServerError = "500 Internal Server Error"
  show MethodNotAllowed = "405 Method Not Allowed"

data ResponseHeaders = ResponseHeaders {
                    version :: String,
                    status :: Status
                  }

instance Show ResponseHeaders where
    show h = unwords [version h, show $ status h] ++ "\n"

data HTTPResponse = HTTPResponse ResponseHeaders Payload

instance Show HTTPResponse where
  show (HTTPResponse headers payload) = show headers ++ show payload



