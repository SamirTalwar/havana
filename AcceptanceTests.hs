{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as Dir
default (T.Text)

data TestCase = TestCase {
    directory :: Shelly.FilePath,
    inputFiles :: [Shelly.FilePath]
}

acceptanceTestPath = absPath $ fromText "acceptance"

acceptanceTestCases = do
    acceptanceTestDir <- acceptanceTestPath
    testDirectories <- ls acceptanceTestDir
    forM testDirectories $ \dir -> do
        inputFiles <- ls dir
        return $ TestCase dir inputFiles

main = do
    shelly $ silently $ do
        checkJavaVersion "1.8"
        compileHavana

        tests <- acceptanceTestCases
        forM_ tests $ \testCase -> do
            javacOutput <- compile testCase javac
            havanaOutput <- compile testCase havana
            diff javacOutput havanaOutput

checkJavaVersion version = do
    cmd "javac" "-version"
    javacVersion <- lastStderr
    when (not $ ("javac " `T.append` version) `T.isPrefixOf` (T.strip javacVersion))
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

compileHavana = fake ["cabal", "build"]

compile testCase compiler = do
    cd (directory testCase)
    forM_ (inputFiles testCase) (\file -> fake [compiler, toTextIgnore file])

diff a b = when (a /= b) (echo "Nope.")

javac = "javac"
havana = "./bin/havana"

fake command = echo (T.append "$ " (T.intercalate " " command))

pathFrom = fromText . T.pack
