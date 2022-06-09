{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Effects.FileReading where

import Polysemy (makeSem, interpret)
import Polysemy.Internal ( embed, Member, Embed, Sem )
import Prelude as P

import qualified Control.Exception as E (try)


data FileReading m a where
    ReadFromFile :: String -> FileReading m (Maybe String)

makeSem ''FileReading

runFileReadingIO :: Member (Embed IO) r =>
                Sem (FileReading : r) a -> Sem r a
runFileReadingIO = interpret $ \case
    ReadFromFile path -> do
        (x :: Either IOError String) <- embed $ E.try $ P.readFile path
        return $ case x of
            Left _ -> Nothing
            Right s -> Just s


runFileReadConstant :: Sem (FileReading : r) a -> Sem r a
runFileReadConstant = interpret $ \case
    ReadFromFile _ -> return (Just "<html><h3 style=\"color:blue;\">Programming with effects</h3></html>")
