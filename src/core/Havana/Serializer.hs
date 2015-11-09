{-# LANGUAGE FlexibleInstances #-}

module Havana.Serializer where

import Havana.AST

import Control.Monad.State.Lazy
import Data.Bits
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Word (Word8)

type MethodSignature = undefined
type ClassText = [String]
data Context = Context ClassText

modifyClassText :: (ClassText -> ClassText) -> State Context ()
modifyClassText m = modify (\(Context classText) -> Context (m classText))

serializeToFile :: AST -> FilePath -> IO ()
serializeToFile ast outputPath =
        ByteString.writeFile outputPath (ByteString.pack $ evalState (serialize ast) (Context []))

class Serializable a where
    serialize :: a -> State Context [Word8]

instance Serializable AST where
    serialize (JavaClass filePath className modifiers methods lineNumber) = do
        modifyClassText (++ [
            "<init>",
            methodTypeSignature (JavaMethod "<init>" (JavaModifiers Public NoHierarchy False) [] Void 0),
            "Code",
            "LineNumberTable"])

        theModifiers <- serialize (exceptHierarchy modifiers)
        theMethods <- serialize methods

        modifyClassText (++ ["SourceFile", filePath])

        (Context allStrings) <- get

        return $ concat [
            [0xca, 0xfe, 0xba, 0xbe, 0x00, 0x00, 0x00, 0x34],
            int16 (13 + count),
            [0x0a, 0x00, 0x03],
            int16 (10 + count),
            [0x07],
            int16 (11 + count),
            [0x07],
            int16 (12 + count),

            concatMap text allStrings,

            [0x0c, 0x00, 0x04, 0x00, 0x05],

            text className, text "java/lang/Object",

            int16 (bit 5 .|. modifiersAsBits modifiers),

            [0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00],

            int16 (1 + methodCount),

            theModifiers,

            [0x00, 0x04, 0x00, 0x05, 0x00, 0x01, 0x00, 0x06,
             0x00, 0x00, 0x00, 0x1d, 0x00, 0x01, 0x00, 0x01,
             0x00, 0x00, 0x00, 0x05, 0x2a, 0xb7, 0x00, 0x01,
             0xb1, 0x00, 0x00, 0x00, 0x01, 0x00, 0x07, 0x00,
             0x00, 0x00, 0x06, 0x00, 0x01, 0x00, 0x00],

            int16 lineNumber,

            theMethods,

            [0x00, 0x01],
            int16 (8 + count),
            [0x00, 0x00, 0x00, 0x02],
            int16 (9 + count)]

        where
        methodCount = length methods
        count = methodCount + sum (map (length . List.nub . methodParameters) methods)

instance Serializable [JavaMethod] where
    serialize methods = concat <$> mapM serialize (zip [0..] methods :: [(Int, JavaMethod)])

instance Serializable (Int, JavaMethod) where
    serialize (index, method@(JavaMethod name modifiers parameters _ lineNumber)) = do
        modifyClassText (++ case length parameters of
                0 -> [name]
                n -> [name, methodTypeSignature method])

        (Context allStrings) <- get

        theModifiers <- serialize modifiers
        return $ concat [
            theModifiers,
            int16 (8 + index),
            int16 (4 + Maybe.fromJust (List.elemIndex (methodTypeSignature method) allStrings)),
            [0x00, 0x01, 0x00, 0x06, 0x00, 0x00, 0x00, 0x19,
             0x00, 0x00],
            int16 (length parameters + if staticModifier modifiers then 0 else 1),
            [0x00, 0x00, 0x00, 0x01, 0xb1, 0x00, 0x00, 0x00,
             0x01, 0x00, 0x07, 0x00, 0x00, 0x00, 0x06, 0x00,
             0x01, 0x00, 0x00],
            int16 lineNumber]

instance Serializable JavaModifiers where
    serialize = return . int16 . modifiersAsBits

modifiersAsBits (JavaModifiers visibilityModifier hierarchyModifier staticModifier) =
    zeroBits
        .|. (case visibilityModifier of
                 DefaultAccess -> zeroBits
                 Public -> bit 0
                 Private -> bit 1
                 Protected -> bit 2)
        .|. (case hierarchyModifier of
                 NoHierarchy -> zeroBits
                 Abstract -> bit 10
                 Final -> bit 4)
        .|. (if staticModifier then bit 3 else zeroBits)

methodTypeSignature :: JavaMethod -> String
methodTypeSignature (JavaMethod methodName _ methodParameters _ _) =
    "(" ++ replicate (length methodParameters) 'I' ++ ")V"

text :: String -> [Word8]
text string = map fromIntegral $ [0x01, 0x00, length string] ++ map Char.ord string

int16 :: Int -> [Word8]
int16 integer = [high8, low8]
    where
    low8 = fromIntegral $ integer `mod` 0x100
    high8 = fromIntegral $ integer `div` 0x100
