{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Test.Framework ( testGroup, defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Types.HTTP.General (ContentType(TextPlain), Payload(Payload))
import qualified Types.HTTP.Response as B
import qualified Types.HTTP.Request as A


main :: IO ()
main = defaultMain tests

payload' = Payload 2 TextPlain "Hi"
requestHeaders' = A.RequestHeaders A.GET "/" "HTTP/1.0"
responseHeaders' = B.ResponseHeaders "HTTP/1.0" B.OK

prop_show_payload = show payload' == "Content-Length: 2\nContent-Type: text/plain\n\nHi\n"
prop_show_request_headers = show requestHeaders' == "GET / HTTP/1.0\n"
prop_show_response_headers = show responseHeaders'== "HTTP/1.0 200 OK\n"
prop_show_http_response = show (B.HTTPResponse responseHeaders' payload')
    == "HTTP/1.0 200 OK\nContent-Length: 2\nContent-Type: text/plain\n\nHi\n"

tests :: [Test]
tests = [
    testGroup "HTTP messages" [
        testProperty "Show HTTP message payload" prop_show_payload,
        testProperty "Show HTTP request headers" prop_show_request_headers,
        testProperty "Show HTTP response headers" prop_show_response_headers,
        testProperty "Show HTTP response" prop_show_http_response
        ]
    ]

