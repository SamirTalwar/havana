{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import Control.Monad
import qualified Data.ByteString as BS
import Data.Monoid (Monoid, mappend, mconcat, mempty)
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
    TestCase "002: Empty Methods" (acceptanceTestDir </> "002-empty-methods")
            [testFile "EmptyMethods.java",
             testFile "BadlyFormattedMethods.java",
             testFile "MethodsWithArguments.java"],
    TestCase "003: Class Modifiers" (acceptanceTestDir </> "003-class-modifiers")
            [testFile "PublicClass.java",
             testFile "DefaultAccessClass.java",
             testFile "PublicAbstractClass.java",
             testFile "DefaultAccessAbstractClass.java",
             testFile "PublicFinalClass.java",
             testFile "DefaultAccessFinalClass.java"],
    TestCase "999: Comments" (acceptanceTestDir </> "999-comments")
            [testFile "Comments.java"]]

data TestCase =
        TestCase { testName :: T.Text, directory :: Shelly.FilePath, files :: [TestFile] }
      | DisabledTestCase { disabledTestName :: T.Text }

data TestFile =
        TestFile { inputFile :: Shelly.FilePath, outputFile :: Shelly.FilePath }
      | DisabledTestFile { file :: Shelly.FilePath }

data TestContext = TestContext { javac :: Shelly.FilePath, havanac :: Shelly.FilePath, tmpPath :: Shelly.FilePath }

data TestResult = Success | Failure | Disabled
    deriving (Eq, Show)
instance Monoid TestResult where
    mempty = Success
    Success `mappend` x = x
    Failure `mappend` _ = Failure
    Disabled `mappend` x = x

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
    rm_rf tmpPath
    mkdir_p tmpPath

    let context = TestContext { javac = fromText "javac", havanac = havanac, tmpPath = tmpPath }
    let tests = acceptanceTestCases
    results <- forM tests $ \testCase ->
        execute testCase context
    when (mconcat results == Failure) $
        quietExit 1

checkJavaVersion version = do
    silently $ cmd "javac" "-version"
    javacVersion <- lastStderr
    unless (("javac " `T.append` version) `T.isPrefixOf` T.strip javacVersion)
        (errorExit (T.concat ["Running the acceptance tests requires Java ", version, " or higher."]))

execute :: TestCase -> TestContext -> Sh TestResult
execute (TestCase testName directory files) context = do
    echo testName
    results <- chdir directory $ forM files $ test context
    return $ mconcat results
execute (DisabledTestCase testName) context = do
    coloredAs yellow $ do
        echo_n "DISABLED: "
        echo testName
    return Disabled

test :: TestContext -> TestFile -> Sh TestResult
test (TestContext javac havanac tmpPath) (TestFile inputFile outputFile) = do
    echo_n "  "
    echo $ toTextIgnore inputFile
    javacOutputFile <- compile "javac" (cmd javac inputFile) outputFile tmpPath
    havanaOutputFile <- compile "havana" (cmd havanac inputFile) outputFile tmpPath
    comparisonFailure <- liftIO $ compareFiles javacOutputFile havanaOutputFile
    case comparisonFailure of
        Just (byteIndex, byteA, byteB) -> do
            coloredAs red $ echo $ T.pack $ printf (
                "    At byte %04d (0x%04x), the values differ.\n"
                ++ "      In file %s, it is: 0x%02x\n"
                ++ "      In file %s, it is: 0x%02x\n"
                ) byteIndex byteIndex (fromPath javacOutputFile) byteA (fromPath havanaOutputFile) byteB
            return Failure
        Nothing ->
            return Success
test _ (DisabledTestFile file) = do
    coloredAs yellow $ do
        echo_n "  DISABLED: "
        echo $ toTextIgnore file
    return Disabled

compile :: String -> Sh () -> Shelly.FilePath -> Shelly.FilePath -> Sh Shelly.FilePath
compile prefix compilation outputFile outputPath = do
    let destination = outputPath </> (prefix ++ "-" ++ fromPath outputFile)
    compilation
    mv outputFile destination
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
