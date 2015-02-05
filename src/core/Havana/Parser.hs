{-# LANGUAGE FlexibleContexts #-}

module Havana.Parser where

import Havana.AST

import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Text.Parsec
import qualified Text.Parsec.Error as Error

parse :: FilePath -> IO (Either ParseError AST)
parse inputPath = do
    input <- readFile inputPath
    runParserT parser () inputPath input

parseErrors :: ParseError -> String
parseErrors = (error . concatenateWith "\n" . map Error.messageString . Error.errorMessages)
    where
    concatenateWith :: Monoid.Monoid a => a -> [a] -> a
    concatenateWith separator = Monoid.mconcat . List.intersperse separator

parser :: Stream s m Char => ParsecT s u m AST
parser = do
    string "class"
    whitespace
    className <- javaToken
    whitespace
    between (char '{') (char '}') whitespace
    return (Class className)

whitespace :: Stream s m Char => ParsecT s u m String
whitespace = many1 space

javaToken :: Stream s m Char => ParsecT s u m String
javaToken = (:) <$> letter <*> many alphaNum
