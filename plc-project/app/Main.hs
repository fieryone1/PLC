module Main (main) where

import System.Environment (getArgs)
import Tokens (alexScanTokens)
import Parser (parse)
import Part2Eval (runProg)


main :: IO ()
main = do
        args <- getArgs
        case args of
            [filename] -> do
                src  <- readFile filename
                let tokens = alexScanTokens src
                let ast    = parse tokens
                runProg ast
            _ ->
                error "Usage: plc-project-exe <query-file.rql>"