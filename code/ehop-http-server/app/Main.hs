module Main where

import Server as HTTP ( run )

main :: IO ()
main = HTTP.run
