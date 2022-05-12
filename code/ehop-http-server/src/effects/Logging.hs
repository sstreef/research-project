module Effects.Logging where

import Polysemy (makeSem, Sem, interpret, Embed, embed, Members)
import Polysemy.State (State, get, put)

data Logger m a where
    Log     :: String -> Logger m () -- Logs a string
    LogLine :: String -> Logger m () -- Logs a string with line ending
    FlushLog   :: Logger m () -- Flushes log to output and clears buffer

makeSem ''Logger

-- Generates:
-- log      :: Member Logger r => String -> Sem r ()
-- logLine  :: Member Logger r => String -> Sem r ()
-- flush    :: Member Logger r => Sem r ()


{-
    Run with:
        & runConsoleLogger
        & runState ""
        & runM
-}
runConsoleLogger :: Members [Embed IO, State String] r =>
                    Sem (Logger : r) a -> Sem r a
runConsoleLogger = interpret $ \case
    Log s       -> do { acc <- get; put $ acc ++ s }
    LogLine s   -> do { acc <- get; put $ acc ++ s ++ "\n" }
    FlushLog    -> do { acc <- get; put ""; embed $ print acc }

{-
    Run with:
        & runFileLogger
        & runState ("", "./logs/log.txt")
        & runM
-}
runFileLogger :: Members [Embed IO, State (String, String)] r =>
                 Sem (Logger : r) a -> Sem r a
runFileLogger = interpret $ \case
    Log s       -> do { (acc, file) <- get; put (acc ++ s, file) }
    LogLine s   -> do { (acc, file) <- get; put (acc ++ s ++ "\n", file) }
    FlushLog    -> do { (acc, file) <- get; put ("", file); embed $ appendFile file acc }
        
