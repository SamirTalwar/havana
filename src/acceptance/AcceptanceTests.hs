{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.String as S
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.FilePath as Path

import qualified Havana.Compiler

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
    outputFile = S.fromString $ Path.replaceExtension (fromPath inputFile) "class"

acceptanceTestDir = fromText "acceptance"
tmpDir = fromText "tmp"
havanacRelativePath = fromText "dist/build/havanac/havanac"

acceptanceTestCases = do
    return [TestCase (acceptanceTestDir </> "001-empty-class") [testFile "Alpha.java"]]

main = shelly $ silently $ do
    checkJavaVersion "1.8"
    havanac <- absPath havanacRelativePath
    tmpPath <- absPath tmpDir
    mkdir_p tmpPath

    tests <- acceptanceTestCases
    forM tests $ \testCase -> do
        cd (directory testCase)
        forM (files testCase) $ \file -> do
            javacOutputFile <- compile "javac" file tmpPath (cmd "javac")
            havanaOutputFile <- compile "havana" file tmpPath (cmd havanac)
            cmd "diff" javacOutputFile havanaOutputFile

checkJavaVersion version = do
    cmd "javac" "-version"
    javacVersion <- lastStderr
    unless (("javac " `T.append` version) `T.isPrefixOf` T.strip javacVersion)
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

compile prefix file outputPath compiler = do
    let destination = outputPath </> (prefix ++ "-" ++ (fromPath $ outputFile file))
    compiler (inputFile file)
    cp (outputFile file) destination
    rm (outputFile file)
    return destination

fromPath = T.unpack . toTextIgnore
