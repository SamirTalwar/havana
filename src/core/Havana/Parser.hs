{-# LANGUAGE FlexibleContexts #-}

module Havana.Parser where

import Havana.AST

import Control.Applicative ((<*), (<*>))
import Control.Monad (when)
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
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
    modifiers <- javaClassModifiers
    string "class"
    whitespace
    className <- javaToken
    optionalWhitespace
    methods <- between (char '{') (char '}') javaMethods
    return JavaClass { filePath = filePath,
                       className = className,
                       classModifiers = modifiers,
                       methods = methods }

javaMethods :: Stream s m Char => ParsecT s u m [JavaMethod]
javaMethods = do
    optionalWhitespace
    many $ do
        modifiers <- javaMethodModifiers
        returnType <- javaType
        whitespace
        methodName <- javaToken
        argumentList
        optionalWhitespace
        between (char '{') (char '}') optionalWhitespace
        optionalWhitespace
        return JavaMethod {
            methodName = methodName,
            methodModifiers = modifiers,
            returnType = returnType }


javaVisibilityModifierStrings = ["public", "protected", "private"]
javaHierarchyModifierStrings = ["abstract", "final"]
javaStaticModifierString = "static"

javaClassModifiers :: Stream s m Char => ParsecT s u m JavaModifiers
javaClassModifiers = javaModifiers $ "public" : javaHierarchyModifierStrings

javaMethodModifiers :: Stream s m Char => ParsecT s u m JavaModifiers
javaMethodModifiers = javaModifiers $ javaStaticModifierString : javaVisibilityModifierStrings

javaModifiers :: Stream s m Char => [String] -> ParsecT s u m JavaModifiers
javaModifiers validModifiers = do
    modifiers <- many $ choice (map (try . string) validModifiers) <* whitespace
    let visibilityModifier = case List.find (`elem` javaVisibilityModifierStrings) modifiers of
                                 Just "public" -> Public
                                 Just "protected" -> Protected
                                 Just "private" -> Private
                                 Nothing -> DefaultAccess
    let hierarchyModifier = case List.find (`elem` javaHierarchyModifierStrings) modifiers of
                                Just "abstract" -> Abstract
                                Just "final" -> Final
                                Nothing -> NoHierarchy
    let staticModifier = Maybe.isJust $ List.find (== javaStaticModifierString) modifiers
    return JavaModifiers { visibilityModifier = visibilityModifier,
                           hierarchyModifier = hierarchyModifier,
                           staticModifier = staticModifier }

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
