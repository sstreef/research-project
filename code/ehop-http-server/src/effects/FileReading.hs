module Effects.FileReading where

import Polysemy (makeSem, interpret)
import Polysemy.Internal ( embed, Member, Embed, Sem )
import Prelude as P

import qualified Control.Exception as E (try)


data FileReading m a where
    ReadFromFile :: String -> FileReading m (Maybe String)

makeSem ''FileReading

-- readFile :: Member FileReader r => String -> Sem r String

runFileReadingIO :: Member (Embed IO) r =>
                Sem (FileReading : r) a -> Sem r a
runFileReadingIO = interpret $ \case
    ReadFromFile path -> do
        (x :: Either IOError String) <- embed $ E.try $ P.readFile path
        pure $ case x of
            Left _ -> Nothing
            Right s -> Just s


-- runFileReadConstant :: Sem (FileReader : r) a -> Sem r a
-- runFileReadConstant = interpret $ \case
--     ReadFromFile _ -> pure $ C.pack "<html><h3 style=\"color:blue;\">Programming with effects</h3></html>"


-- program :: Member FileReading r => Sem r (Maybe String)
-- program = do readFromFile "resources/index.html2"


-- main :: IO ()
-- main = execute >>= putStrLn . show
--     where
--         execute = program
--             & runFileReadIO
--             & runM