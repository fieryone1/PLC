module Eval where

import Parser
import Rdf
import Data.List (nub, sortBy,intersect)
import Data.Set (fromList, toList)



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
    Nothing -> error (name ++ " is undefined")
    Just gra -> return (e, renderGraph gra)



-- IO done 

evalExpr :: Env -> Expr -> IO Graph
evalExpr e (Load file ) = loadG file
evalExpr e (Union f1 f2) = return $ unionEval e f1 f2
evalExpr e (Intersect f1 f2) = return $ intersectEval e f1 f2
evalExpr e (Minus f1 f2) = return $ minusEval e f1 f2
evalExpr e (Select outTerms cond ) = return $ evalSelect e outTerms cond 
evalExpr _ _ = error "undefined"



evalSelect :: Env -> [OutputTerm] -> Condition -> Graph
evalSelect env outTerms cond = 
  let bindings = evalCond env cond 
  in nub $ map (buildTriple outTerms) bindings

buildTriple :: [OutputTerm] -> Binding -> RDFTriple
buildTriple [s, p, o] b = (resolve s, resolve p, resolve o)
  where
    resolve (OutVar v) = case lookup v b of
                           Just n  -> n
                           Nothing -> error ("Unbound variable: " ++ v)
    resolve (OutURI u) = URI u
    resolve (OutStr s) = Str s
    resolve (OutInt i) = Num i
buildTriple _ _ = error "SELECT needs exactly 3 output terms"

evalCond :: Env -> Condition -> [Binding]
evalCond env (Match s p o graph ) =
  case lookup graph env of 
    Nothing -> error (graph ++ " is undefined")
    Just g -> concatMap (matchTriple s p o ) g
evalCond env (And c1 c2 ) =
  let  b1 = evalCond env c1 
  in case c2 of
    Gte _ _  -> filterBindings b1 c2
    Lte _ _  -> filterBindings b1 c2
    Gt  _ _  -> filterBindings b1 c2
    Lt  _ _  -> filterBindings b1 c2
    Eq  _ _  -> filterBindings b1 c2
    Neq _ _  -> filterBindings b1 c2
    _        -> joinBindings b1 (evalCond env c2)
evalCond env (Or c1 c2) = nub $ evalCond env c1 ++ evalCond env c2
evalCond env (Not _) = error " Not requires bindings"
evalCond _ _ = []

filterBindings :: [Binding] -> Condition -> [Binding]
filterBindings bs (Gte var n) = filter (checkNum var (>=n)) bs 
filterBindings bs (Lte var n) = filter (checkNum var (<=n)) bs 
filterBindings bs (Gt var n) = filter (checkNum var (>n)) bs 
filterBindings bs (Lt var n) = filter (checkNum var (<n)) bs 
filterBindings bs (Eq var n) = filter (checkEq var n) bs 
filterBindings bs (Neq var val) = filter (not . checkEq var val) bs 
filterBindings bs _ = bs 


checkNum :: String -> (Int -> Bool) -> Binding -> Bool 
checkNum var f b = case lookup var b of 
  Just (Num x) -> f x 
  _ -> False 


checkEq :: String -> Value -> Binding -> Bool
checkEq var (ValInt n) b = case lookup var b of
  Just (Num x) -> x == n
  _ -> False
checkEq var (ValStr s) b = case lookup var b of
  Just (Str x) -> x == s
  _ -> False
checkEq var (ValURI u) b = case lookup var b of
  Just (URI x) -> x == u
  _ -> False
checkEq var (ValVar v) b = case (lookup var b, lookup v b) of
  (Just x, Just y) -> x == y
  _ -> False




joinBindings :: [Binding] -> [Binding] -> [Binding] 
joinBindings b1 b2 = [b1' ++ b2' | b1' <- b1, b2' <- b2, compatible b1' b2'] where 
  compatible b1' b2' = all (\(key,value) -> lookup key b1' `elem` [Nothing,Just value]) b2'




matchTriple :: Term->Term->Term -> RDFTriple -> [Binding]
matchTriple s p o (s', p', o') = case (bindTerm s s', bindTerm p p', bindTerm o o' ) of 
  (Just b1, Just b2, Just b3) -> [b1 ++ b2 ++ b3]
  _ -> []

bindTerm :: Term -> RDFNode -> Maybe Binding
bindTerm (TermVar x) node    = Just [(x, node)]
bindTerm (TermURI x) (URI y) = if x == y then Just [] else Nothing
bindTerm (TermStr x) (Str y) = if x == y then Just [] else Nothing
bindTerm (TermInt x) (Num y) = if x == y then Just [] else Nothing
bindTerm _ _                 = Nothing


unionEval :: Env -> String -> String -> Graph
unionEval e f1 f2  =
  case (lookup f1 e,lookup f2 e) of
    (Nothing, _) -> error (f1 ++ " is undefined")
    (_,Nothing) -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> nub (g1 ++ g2)

intersectEval :: Env -> String -> String -> Graph
intersectEval e f1 f2 =
    case (lookup f1 e,lookup f2 e) of
    (Nothing, _) -> error (f1 ++ " is undefined")
    (_,Nothing) -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> filter (`elem` g2) g1


minusEval :: Env -> String -> String -> Graph
minusEval e f1 f2 =
    case (lookup f1 e,lookup f2 e) of
    (Nothing, _) -> error (f1 ++ " is undefined")
    (_,Nothing) -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> filter (`notElem` g2) g1




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