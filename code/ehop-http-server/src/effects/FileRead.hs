module Effects.FileRead where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 as C (pack)
import Polysemy (makeSem, interpret)
import Polysemy.Internal ( embed, Member, Embed, Sem )
import Data.Functor ( (<&>) )
import Prelude as P


data FileReader m a where
    ReadFileContents :: String -> FileReader m ByteString

makeSem ''FileReader

-- readFile :: Member FileReader r => String -> Sem r String

runFileReadIO :: Member (Embed IO) r =>
                Sem (FileReader : r) a -> Sem r a
runFileReadIO = interpret $ \case
    ReadFileContents path -> embed $ P.readFile path <&> C.pack



