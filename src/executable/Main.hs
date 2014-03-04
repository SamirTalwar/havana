module Main where

import qualified System.Environment

import qualified Havana.Compiler

main = do
    args <- System.Environment.getArgs
    Havana.Compiler.compile (args !! 0)
