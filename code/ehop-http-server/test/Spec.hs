{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Test.Framework ( testGroup, defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import HTTP

main :: IO ()
main = defaultMain tests

prop_show_http_response_headers = show (Response "HTTP/1.0" OK 11 TextPlain "Hello World") 
    == "HTTP/1.0 200 OK\nContent-Length: 11\nContent-Type: text/plain\n\nHello World\n"
prop_show_http_request_headers = show (Request GET "/" "HTTP/1.0") 
    == "GET / HTTP/1.0"


tests :: [Test]
tests = [
    testGroup "HTTP headers" [
        testProperty "Show HTTP response headers" prop_show_http_response_headers,
        testProperty "Show HTTP request headers" prop_show_http_request_headers
        ]
    ]
