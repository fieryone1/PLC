module Main (main) where

import System.Environment (getArgs)
import Tokens (alexScanTokens)
import Parser (parse)
import Eval 

-- import Eval (runProg)


-- main :: IO ()  --test parser and lexer
-- main = do
--     [filename] <- getArgs
--     source <- readFile filename
--     let tokens = alexScanTokens source
--     let ast = parse tokens
--     print ast

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    src  <- readFile filename
    let tokens = alexScanTokens src
    let ast    = parse tokens
    runProg ast