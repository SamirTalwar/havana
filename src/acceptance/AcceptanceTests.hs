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

data TestCase =
        TestCase { testName :: T.Text, directory :: Shelly.FilePath, files :: [TestFile] }
      | DisabledTestCase { disabledTestName :: T.Text }

data TestFile = TestFile { inputFile :: Shelly.FilePath, outputFile :: Shelly.FilePath }

data TestContext = TestContext { javac :: Shelly.FilePath, havanac :: Shelly.FilePath, tmpPath :: Shelly.FilePath }

testFile inputFile = TestFile inputFile outputFile
    where
    outputFile = S.fromString $ Path.replaceExtension (fromPath inputFile) "class"

disabled (TestCase testName _ _) = DisabledTestCase testName

acceptanceTestDir = fromText "acceptance"
tmpDir = fromText "tmp"
havanacRelativePath = fromText "dist/build/havanac/havanac"

acceptanceTestCases = return [
    TestCase "001: Empty Class" (acceptanceTestDir </> "001-empty-class") [testFile "Alpha.java"]]

main = shelly $ do
    checkJavaVersion "1.8"
    havanac <- absPath havanacRelativePath
    tmpPath <- absPath tmpDir
    mkdir_p tmpPath

    let context = TestContext { javac = fromText "javac", havanac = havanac, tmpPath = tmpPath }
    tests <- acceptanceTestCases
    forM tests $ \testCase ->
        execute testCase context

checkJavaVersion version = do
    silently $ cmd "javac" "-version"
    javacVersion <- lastStderr
    unless (("javac " `T.append` version) `T.isPrefixOf` T.strip javacVersion)
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

execute :: TestCase -> TestContext -> Sh ()
execute (TestCase testName directory files) context = do
    echo testName
    cd directory
    forM_ files $ \file -> do
        javacOutputFile <- compile "javac" file (tmpPath context) (cmd (javac context))
        havanaOutputFile <- compile "havana" file (tmpPath context) (cmd (havanac context))
        highlightOutput $ cmd "cmp" javacOutputFile havanaOutputFile
execute (DisabledTestCase testName) context = coloredAs yellow $ do
    echo_n "DISABLED: "
    echo testName

compile prefix file outputPath compiler = do
    let destination = outputPath </> (prefix ++ "-" ++ fromPath (outputFile file))
    compiler (inputFile file)
    cp (outputFile file) destination
    rm (outputFile file)
    return destination

fromPath = T.unpack . toTextIgnore

highlightOutput command = do
    output <- errExit False command
    exitCode <- lastExitCode
    let color = if exitCode > 0 then red else green
    coloredAs color $ echo_n output
    when (exitCode > 0) (exit exitCode)

coloredAs color action = do
    echo_n color
    result <- action
    echo_n reset
    return result

red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
reset = "\x1b[0m"
