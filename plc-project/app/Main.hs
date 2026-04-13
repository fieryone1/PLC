module Main (main) where

import System.Environment (getArgs)
import Tokens (alexScanTokens)
import Parser (parse)

main :: IO ()
main = do
    [filename] <- getArgs
    source <- readFile filename
    let tokens = alexScanTokens source
    let ast = parse tokens
    print ast