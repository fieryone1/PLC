module Main (main) where

import System.Environment (getArgs)
import Tokens (alexScanTokens)
import Parser (parse)
import Rdf (load)

-- main :: IO ()  --test parser and lexer
-- main = do
--     [filename] <- getArgs
--     source <- readFile filename
--     let tokens = alexScanTokens source
--     let ast = parse tokens
--     print ast

main :: IO ()  --test turtle parser
main = do
    graph <- load "foo"
    print graph