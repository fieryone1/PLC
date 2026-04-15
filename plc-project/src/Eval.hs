module Eval where

import Parser
import Rdf 
import Data.List (nub, sortBy)



type Env = [(String, Graph)]
type Binding = [(String, RDFNode)]

runProg :: [Statement] -> IO ()
runProg = execStatements []

printLines :: [String] -> IO ()
printLines []     = return ()
printLines (x:xs) = do
    putStrLn x
    printLines xs

execStatements :: Env -> [Statement] -> IO ()
execStatements _ [] = return ()
execStatements env (x:xs) = do
  (env', output) <- execStatement env x
  printLines output
  execStatements env' xs


execStatement :: Env -> Statement -> IO (Env, [String])
execStatement e (Assign name expr) = do 
  graph <- evalExpr e expr
  return ((name, graph) : e, [])
execStatement e (Print name) = 
  case lookup name e of 
    Nothing -> error (name ++ "is undefined")
    Just gra -> return (e, renderGraph gra)



-- IO done 

evalExpr :: Env -> Expr -> IO Graph
evalExpr e (Load file ) = loadG file 
evalExpr _ _ = error "undefined"  



--  rendering 
renderGraph :: Graph -> [String]
renderGraph = map renderTriple . sortBy compareTriple . nub

renderTriple :: RDFTriple -> String
renderTriple (s, p, o) = renderNode s ++ " " ++ renderNode p ++ " " ++ renderNode o ++ " ."

renderNode :: RDFNode -> String
renderNode (URI u) = "<" ++ u ++ ">"
renderNode (Str s) = "\"" ++ s ++ "\""
renderNode (Num n) = show n

compareTriple :: RDFTriple -> RDFTriple -> Ordering
compareTriple (s1,p1,o1) (s2,p2,o2) =
    compare (renderNode s1) (renderNode s2) <>
    compare (renderNode p1) (renderNode p2) <>
    compare (renderNode o1) (renderNode o2)