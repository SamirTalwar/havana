{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.String as S
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.FilePath as Path

import qualified Havana

default (T.Text)

data TestCase = TestCase {
    directory :: Shelly.FilePath,
    files :: [TestFile]
}

data TestFile = TestFile {
    inputFile :: Shelly.FilePath,
    outputFile :: Shelly.FilePath
}

testFile inputFile = TestFile inputFile outputFile
    where
    outputFile = S.fromString $ Path.replaceExtension (show inputFile) "class"

acceptanceTestDir = fromText "acceptance"
tmpDir = fromText "tmp"

acceptanceTestCases = do
    testDirectories <- ls acceptanceTestDir
    forM testDirectories $ \dir -> do
        inputFiles <- findWhen (\f -> return $ Path.takeExtension (show f) == "java") dir
        let testFiles = map testFile inputFiles
        return $ TestCase dir testFiles

main = shelly $ silently $ do
    checkJavaVersion "1.8"
    mkdir_p tmpDir

    tests <- acceptanceTestCases
    forM tests $ \testCase -> do
        cd (directory testCase)
        forM (files testCase) $ \file -> do
            javacOutputFile <- compile "javac" file (cmd "javac")
            havanaOutputFile <- compile "havana" file (\t -> return $ Havana.compile (show t))
            cmd "diff" javacOutputFile havanaOutputFile

checkJavaVersion version = do
    cmd "javac" "-version"
    javacVersion <- lastStderr
    unless (("javac " `T.append` version) `T.isPrefixOf` T.strip javacVersion)
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

compile prefix file compiler = do
    let destination = tmpDir </> (prefix ++ "-" ++ (show $ outputFile file))
    compiler (inputFile file)
    cp (outputFile file) destination
    return destination
