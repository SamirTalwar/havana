module Havana.Compiler where

import Data.Either (either)
import qualified System.FilePath as Path

import qualified Havana.Parser as Parser
import qualified Havana.Serializer as Serializer

compile :: FilePath -> IO ()
compile inputPath = do
    result <- Parser.parse inputPath
    let ast = either (error . Parser.parseErrors) id result
    Serializer.serializeToFile ast outputPath
        where
        outputPath = Path.replaceExtension inputPath "class"
