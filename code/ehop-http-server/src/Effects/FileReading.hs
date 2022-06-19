{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Effects.FileReading where

import Polysemy (makeSem, interpret)
import Polysemy.Internal (Sem )
import Prelude as P

import qualified Control.Exception as E (try)
import HTTP.Response (HTTPResponse, notFoundResponse, createContentResponse, Status (OK))
import Parsers.File (getFileContentType)
import HTTP.General (ContentType(TextHtml))


data FileReading m a where
    ReadFromFile :: String -> String -> FileReading m (IO HTTPResponse)

makeSem ''FileReading

runFileReadingIO :: Sem (FileReading : r) a -> Sem r a
runFileReadingIO = interpret $ \case
    ReadFromFile rootPath path -> return $ do
        (eitherErrorOrString :: Either IOError String) <- E.try $ readFile (rootPath ++ path)
        case eitherErrorOrString of
            Right content -> return $ createContentResponse (getFileContentType path) (Just OK) content
            Left _        -> return HTTP.Response.notFoundResponse


runFileReadConstant :: Sem (FileReading : r) a -> Sem r a
runFileReadConstant = interpret $ \case
    ReadFromFile _ _ -> return $ return $ createContentResponse (Just TextHtml) (Just OK) "<html><h3 style=\"color:blue;\">Programming with effects</h3></html>"
