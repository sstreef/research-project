module Types.HTTP.General where

data ContentType = TextPlain | TextHtml | ApplicationJson
    deriving Eq

instance Show ContentType where
    show TextPlain = "text/plain"
    show TextHtml = "text/html"
    show ApplicationJson = "application/json"

data Payload = Payload {
                    -- | The length of the content in bytes
                    contentLength :: Int,
                    -- | The type of content
                    contentType :: ContentType,
                    -- | The content of the response
                    content :: String
                } | Empty

instance Show Payload where
    show Empty = "\n"
    show p = unlines [
            "Content-Length: " ++ show (contentLength p),
            "Content-Type: " ++ show (contentType p),
            '\n' : content p
        ]

