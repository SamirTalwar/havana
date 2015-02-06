{-# LANGUAGE FlexibleContexts #-}

module Havana.Parser where

import Havana.AST

import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Error as Error

parse :: FilePath -> IO (Either ParseError AST)
parse inputPath = parseFromFile (parser inputPath) inputPath

parseErrors :: ParseError -> String
parseErrors = error . concatenateWith "\n" . map Error.messageString . Error.errorMessages
    where
    concatenateWith :: Monoid.Monoid a => a -> [a] -> a
    concatenateWith separator = Monoid.mconcat . List.intersperse separator

parser :: Stream s m Char => FilePath -> ParsecT s u m AST
parser = javaClass

javaClass :: Stream s m Char => FilePath -> ParsecT s u m AST
javaClass filePath = do
    string "class"
    whitespace
    className <- javaToken
    optionalWhitespace
    methods <- between (char '{') (char '}') javaMethods
    return JavaClass { filePath = filePath, className = className, methods = methods }

javaMethods :: Stream s m Char => ParsecT s u m [JavaMethod]
javaMethods = do
    optionalWhitespace
    many $ do
        visibilityModifier <- javaVisibilityModifier
        optionalWhitespace
        returnType <- javaType
        whitespace
        methodName <- javaToken
        argumentList
        optionalWhitespace
        between (char '{') (char '}') optionalWhitespace
        optionalWhitespace
        return JavaMethod {
            methodName = methodName,
            returnType = returnType,
            visibilityModifier = visibilityModifier }

javaVisibilityModifier :: Stream s m Char => ParsecT s u m JavaVisibilityModifier
javaVisibilityModifier = choice [
    try (string "public ") >> return Public,
    try (string "protected ") >> return Protected,
    try (string "private ") >> return Private,
    return DefaultAccess]

javaType :: Stream s m Char => ParsecT s u m JavaType
javaType = string "void" >> return Void

argumentList :: Stream s m Char => ParsecT s u m String
argumentList = string "()"

javaToken :: Stream s m Char => ParsecT s u m JavaToken
javaToken = (:) <$> letter <*> many alphaNum

whitespace :: Stream s m Char => ParsecT s u m String
whitespace = many1 space

optionalWhitespace :: Stream s m Char => ParsecT s u m String
optionalWhitespace = many space
