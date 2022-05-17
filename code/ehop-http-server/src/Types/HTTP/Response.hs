module Types.HTTP.Response where
    
import Types.HTTP.General (Payload (Payload), ContentType (TextPlain, TextHtml, ApplicationJson))
import Data.Maybe (fromMaybe)
    
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


createResponse :: ContentType -> String -> Status -> String -> HTTPResponse
createResponse type' version' status' content' = HTTPResponse (ResponseHeaders version' status') (Payload (length content') type' content')

createContentResponse :: Maybe ContentType -> Maybe Status -> String -> HTTPResponse
createContentResponse type' status' = createResponse (fromMaybe TextPlain type') "HTTP/1.0" (fromMaybe OK status')

createStatusResponse :: Status -> HTTPResponse
createStatusResponse st = createResponse TextPlain "HTTP/1.0" st (show st)

createPlainResponse :: Maybe Status -> String -> HTTPResponse
createPlainResponse = createContentResponse (Just TextPlain)

createHTMLResponse :: Maybe Status -> String -> HTTPResponse
createHTMLResponse = createContentResponse (Just TextHtml)

createJSONResponse :: Maybe Status -> String -> HTTPResponse
createJSONResponse = createContentResponse (Just ApplicationJson)

notFoundResponse :: HTTPResponse
notFoundResponse = createStatusResponse NotFound

badRequestResponse :: HTTPResponse
badRequestResponse = createStatusResponse BadRequest