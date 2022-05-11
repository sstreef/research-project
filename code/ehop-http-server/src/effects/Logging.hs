module Effects.Logging where

import Polysemy (makeSem, Sem, interpret, Embed, embed, Member, runM, Members)
import Polysemy.State (State, runState, get, put)
import Prelude hiding (log)

import Data.Function ((&))

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
TODO:
    Add a runLogger function that implements the Logger effect.
    Two possible implementations:
    1.  Logger that flushes logs to the console.
    2.  Logger that flushes logs to file.
-}

runConsoleLogger :: Members [Embed IO, State String] r =>
                    Sem (Logger : r) a -> Sem r a
runConsoleLogger = interpret $ \case
    Log s       -> do { acc <- get; put $ acc ++ s }
    LogLine s   -> do { acc <- get; put $ acc ++ s ++ "\n" }
    FlushLog    -> do { acc <- get; put ""; embed $ print acc }


runFileLogger :: Members [Embed IO, State (String, String)] r =>
                 Sem (Logger : r) a -> Sem r a
runFileLogger = interpret $ \case
    Log s       -> do { (acc, file) <- get; put (acc ++ s, file) }
    LogLine s   -> do { (acc, file) <- get; put (acc ++ s ++ "\n", file) }
    FlushLog    -> do { (acc, file) <- get; put ("", file); embed $ appendFile file acc }
        
        
-- program:: Member Logger r => Sem r ()
-- program = do
--     log "Hello  "
--     logLine "World!"
--     flushLog
--     logLine "Lorem ipsum"
--     log "1"
--     log "+"
--     log "1"
--     logLine "=2"
--     flushLog


-- log2console :: IO (String, ())
-- log2console = execute
--     where
--         execute = program
--             & runConsoleLogger
--             & runState ""
--             & runM

-- log2file :: IO ()
-- log2file = execute >>= print
--     where
--         execute = program
--             & runFileLogger
--             & runState ("", "./logs/log.txt")
--             & runM