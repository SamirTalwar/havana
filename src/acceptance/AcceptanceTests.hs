{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Word as W
import qualified Numeric as N
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import Text.Printf (printf)

import qualified Havana.Compiler

default (T.Text)

acceptanceTestCases = [
    TestCase "001: Empty Class" (acceptanceTestDir </> "001-empty-class")
            [testFile "Alpha.java"],
    disabled $ TestCase "002: Empty Methods" (acceptanceTestDir </> "002-empty-methods")
            [testFile "EmptyMethods.java"]]

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

main = shelly $ do
    checkJavaVersion "1.8"
    havanac <- absPath havanacRelativePath
    tmpPath <- absPath tmpDir
    mkdir_p tmpPath

    let context = TestContext { javac = fromText "javac", havanac = havanac, tmpPath = tmpPath }
    let tests = acceptanceTestCases
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
    chdir directory $ forM_ files $ \file -> do
        javacOutputFile <- compile "javac" file (tmpPath context) (cmd (javac context))
        havanaOutputFile <- compile "havana" file (tmpPath context) (cmd (havanac context))
        comparisonFailure <- liftIO $ compareFiles javacOutputFile havanaOutputFile
        case comparisonFailure of
            Just (byteIndex, byteA, byteB) -> do
                coloredAs red $ echo $ T.pack $ printf (
                    "At byte %04d (0x%04x), the values differ.\n"
                    ++ "  In file %s, it is: %02x\n"
                    ++ "  In file %s, it is: %02x\n"
                    ) byteIndex byteIndex (fromPath javacOutputFile) byteA (fromPath havanaOutputFile) byteB
                quietExit 1
            Nothing -> return ()
execute (DisabledTestCase testName) context = coloredAs yellow $ do
    echo_n "DISABLED: "
    echo testName

compile prefix file outputPath compiler = do
    let destination = outputPath </> (prefix ++ "-" ++ fromPath (outputFile file))
    compiler (inputFile file)
    cp (outputFile file) destination
    rm (outputFile file)
    return destination

compareFiles :: Shelly.FilePath -> Shelly.FilePath -> IO (Maybe (Int, W.Word8, W.Word8))
compareFiles fileA fileB = do
    a <- BS.readFile $ fromPath fileA
    b <- BS.readFile $ fromPath fileB
    return $ L.find (\(_, byteA, byteB) -> byteA /= byteB)
            (zip3 [0..] (pad 0 (BS.length b) (BS.unpack a)) (pad 0 (BS.length a) (BS.unpack b)))

pad :: a -> Int -> [a] -> [a]
pad value minimumSize list
    | listSize < minimumSize
        = replicate (listSize - minimumSize) value ++ list
    | otherwise
        = list
    where
    listSize = length list

coloredAs color action = do
    echo_n color
    result <- action
    echo_n reset
    return result

fromPath = T.unpack . toTextIgnore

red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
reset = "\x1b[0m"
