module Logging where

import Polysemy (makeSem)

data Logger m a where
    Log     :: String -> Logger m () -- Logs a string
    LogLine :: String -> Logger m () -- Logs a string with line ending
    Flush   :: Logger m () -- Flushes log to file

makeSem ''Logger

-- Generates:
-- log      :: Member Logger r => String -> Sem r ()
-- logLine  :: Member Logger r => String -> Sem r ()
-- flush    :: Member Logger r => Sem r ()

{-
TODO:
    Add a runLogger function that implements the Logger effect.
    Two possible implementations:
    1.  Logger that flushes logs to the console.
    2.  Logger that flushes logs to file.
-}