module Havana.Serializer where

import Havana.AST

import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Word as Word

serializeToFile :: AST -> FilePath -> IO ()
serializeToFile ast outputPath = ByteString.writeFile outputPath (ByteString.pack $ serialize ast)

serialize :: AST -> [Word.Word8]
serialize (JavaClass filePath className methods) = map fromIntegral $ concat [
    [0xca, 0xfe, 0xba, 0xbe, 0x00, 0x00, 0x00, 0x34],

    [0x00, 0x0d, 0x0a, 0x00, 0x03, 0x00, 0x0a, 0x07,
     0x00, 0x0b, 0x07, 0x00, 0x0c],

    text "<init>", text "()V", text "Code", text "LineNumberTable", text "SourceFile", text filePath,

    [0x0c, 0x00, 0x04, 0x00, 0x05],

    text className, text "java/lang/Object",

    [0x00, 0x20, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00,
     0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04,
     0x00, 0x05, 0x00, 0x01, 0x00, 0x06, 0x00, 0x00,
     0x00, 0x1d, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00,
     0x00, 0x05, 0x2a, 0xb7, 0x00, 0x01, 0xb1, 0x00,
     0x00, 0x00, 0x01, 0x00, 0x07, 0x00, 0x00, 0x00,
     0x06, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00,
     0x01, 0x00, 0x08, 0x00, 0x00, 0x00, 0x02, 0x00,
     0x09]]

text string = [0x01, 0x00, length string] ++ map Char.ord string
