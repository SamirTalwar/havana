module Havana.Compiler where

import qualified Data.String as S
import qualified System.FilePath as Path
import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Havana.Parser as Parser
import qualified Havana.Serializer as Serializer

compile :: String -> IO ()
compile inputPath = do
    ast <- Parsec.parseFromFile Parser.parser inputPath
    Serializer.serializeToFile ast outputPath
    return ()
    where
    outputPath = Path.replaceExtension inputPath "class"
