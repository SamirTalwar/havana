{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as Dir

import qualified Havana

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

main = shelly $ silently $ do
    checkJavaVersion "1.8"

    tests <- acceptanceTestCases
    forM_ tests $ \testCase -> do
        compile testCase (\t -> cmd "javac" (toTextIgnore t))
        compile testCase (\t -> return $ Havana.compile (show t))

checkJavaVersion version = do
    cmd "javac" "-version"
    javacVersion <- lastStderr
    unless (("javac " `T.append` version) `T.isPrefixOf` T.strip javacVersion)
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

compile testCase compiler = do
    cd (directory testCase)
    forM_ (inputFiles testCase) compiler
