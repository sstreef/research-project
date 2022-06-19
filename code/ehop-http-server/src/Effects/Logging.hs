{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Effects.Logging where

import Polysemy (makeSem, Sem, interpret, Embed, embed, Members, Member)
import Polysemy.State (State, get)

data Logging m a where
    Log     :: String -> Logging m () -- Logs a string
    LogLine :: String -> Logging m () -- Logs a string with line ending

makeSem ''Logging

-- Generates:
-- log      :: Member Logger r => String -> Sem r ()
-- logLine  :: Member Logger r => String -> Sem r ()


{-
    Run with:
        & runConsoleLogger
        & runM
-}
runConsoleLogger :: Member (Embed IO) r =>
                    Sem (Logging : r) a -> Sem r a
runConsoleLogger = interpret $ \case
    Log s       -> embed $ print s
    LogLine s   -> embed $ print s

{-
    Run with:
        & runFileLogger
        & runState "./logs/log.txt"
        & runM
-}
runFileLogger :: Members [Embed IO, State String] r =>
                 Sem (Logging : r) a -> Sem r a
runFileLogger = interpret $ \case
    Log s       -> do { file <- get; embed $ appendFile file s }
    LogLine s   -> do { file <- get; embed $ appendFile file (s ++ "\n") }
