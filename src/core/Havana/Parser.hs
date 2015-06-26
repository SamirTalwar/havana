{-# LANGUAGE FlexibleContexts #-}

module Havana.Parser where

import Havana.AST

import Control.Applicative ((<*), (<*>))
import Control.Monad (void, when)
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.String
import qualified Text.Parsec.Error as Error

parse :: FilePath -> IO (Either ParseError AST)
parse inputPath = parseFromFile (parser inputPath) inputPath

parseErrors :: ParseError -> String
parseErrors = show

parser :: Stream s m Char => FilePath -> ParsecT s u m AST
parser = javaClass

javaClass :: Stream s m Char => FilePath -> ParsecT s u m AST
javaClass filePath = do
    optionalWhitespace
    lineNumber <- currentLineNumber
    modifiers <- javaClassModifiers
    reservedWord "class"
    className <- javaToken
    methods <- between (symbol "{") (symbol "}") javaMethods
    return JavaClass { filePath = filePath,
                       className = className,
                       classModifiers = modifiers,
                       methods = methods,
                       classLineNumber = lineNumber }

javaMethods :: Stream s m Char => ParsecT s u m [JavaMethod]
javaMethods = many $ do
        modifiers <- javaMethodModifiers
        returnType <- javaType
        methodName <- javaToken
        parameters <- parameterList
        between (symbol "{") (char '}') optionalWhitespace
        lineNumber <- currentLineNumber
        optionalWhitespace
        return JavaMethod {
            methodName = methodName,
            methodModifiers = modifiers,
            returnType = returnType,
            methodParameters = parameters,
            methodLineNumber = lineNumber }


javaVisibilityModifierStrings = ["public", "protected", "private"]
javaHierarchyModifierStrings = ["abstract", "final"]
javaStaticModifierString = "static"

javaClassModifiers :: Stream s m Char => ParsecT s u m JavaModifiers
javaClassModifiers = javaModifiers $ "public" : javaHierarchyModifierStrings

javaMethodModifiers :: Stream s m Char => ParsecT s u m JavaModifiers
javaMethodModifiers = javaModifiers $ javaStaticModifierString : javaVisibilityModifierStrings

javaModifiers :: Stream s m Char => [String] -> ParsecT s u m JavaModifiers
javaModifiers validModifiers = do
    modifiers <- many $ choice (map (try . reservedWord) validModifiers)
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
javaType = javaToken >>= typeFromName
    where
    typeFromName "void" = return Void
    typeFromName "int" = return Int
    typeFromName other = unexpected other

javaToken :: Stream s m Char => ParsecT s u m JavaToken
javaToken = lexeme ((:) <$> letter <*> many alphaNum)

parameterList :: Stream s m Char => ParsecT s u m [JavaParameter]
parameterList = between (symbol "(") (symbol ")") (javaParameter `sepBy` symbol ",")
    where
    javaParameter = constructParameter <$> javaType <*> javaToken
    constructParameter parameterType parameterName = JavaParameter {
        parameterName = parameterName,
        parameterType = parameterType }

reservedWord :: Stream s m Char => JavaToken -> ParsecT s u m JavaToken
reservedWord expected = do
    let parser = javaToken
    actual <- parser
    if actual == expected
        then return actual
        else unexpected actual

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol value = lexeme $ string value

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme parser = parser <* optionalWhitespace

whitespace :: Stream s m Char => ParsecT s u m ()
whitespace = skipMany1 (javaComment <|> skipMany1 space)

optionalWhitespace :: Stream s m Char => ParsecT s u m ()
optionalWhitespace = skipMany (javaComment <|> skipMany1 space)

javaComment :: Stream s m Char => ParsecT s u m ()
javaComment = do
    try javaBlockComment <|> try javaLineComment
    return ()

javaBlockComment :: Stream s m Char => ParsecT s u m ()
javaBlockComment = do
    string "/*"
    manyTill anyChar (try $ string "*/")
    return ()

javaLineComment :: Stream s m Char => ParsecT s u m ()
javaLineComment = do
    string "//"
    manyTill (noneOf "\r\n") (try endOfLine)
    return ()

currentLineNumber :: Stream s m Char => ParsecT s u m LineNumber
currentLineNumber = do
    state <- getParserState
    return $ sourceLine $ statePos state
