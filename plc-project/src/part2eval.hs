module Part2Eval where

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
    Nothing  -> error (name ++ " is undefined")
    Just gra -> return (e, renderGraph gra)
 
 
 

 
evalExpr :: Env -> Expr -> IO Graph
evalExpr _ (Load file)                     = loadG file
evalExpr e (Union f1 f2)                   = return $ unionEval     e f1 f2
evalExpr e (Intersect f1 f2)               = return $ intersectEval e f1 f2
evalExpr e (Minus f1 f2)                   = return $ minusEval     e f1 f2
evalExpr e (Select outTerms cond)          = return $ evalSelect      e outTerms cond
evalExpr e (SelectGroup outTerms cond gb)  = return $ evalSelectGroup e outTerms cond gb
 
 
 

 
evalSelect :: Env -> [OutputTerm] -> Condition -> Graph
evalSelect env outTerms cond =
  let bindings = evalCond env [[]] cond
  in  nub $ map (buildTriple outTerms) bindings
 
 
evalSelectGroup :: Env -> [OutputTerm] -> Condition -> GroupByClause -> Graph
evalSelectGroup env outTerms cond (GroupBy groupVar) =
  let bindings = evalCond env [[]] cond
      groups   = groupBy' groupVar bindings
  in  nub $ map (buildGroupTriple outTerms) groups
 
groupBy' :: String -> [Binding] -> [(RDFNode, [Binding])]
groupBy' var bs =
  let keys = nub [ v | b <- bs, Just v <- [lookup var b] ]
  in  [ (k, filter (\b -> lookup var b == Just k) bs) | k <- keys ]
 
buildGroupTriple :: [OutputTerm] -> (RDFNode, [Binding]) -> RDFTriple
buildGroupTriple outTerms (_, bs) =
  buildTriple outTerms (applyAgg outTerms bs)
 
applyAgg :: [OutputTerm] -> [Binding] -> Binding
applyAgg outTerms bs = concatMap (aggTerm bs) outTerms
 
aggTerm :: [Binding] -> OutputTerm -> Binding
aggTerm bs (OutAgg Max   var) =
  case [ n | b <- bs, Just (Num n) <- [lookup var b] ] of
    [] -> []
    xs -> [(var, Num (maximum xs))]
aggTerm bs (OutAgg Min   var) =
  case [ n | b <- bs, Just (Num n) <- [lookup var b] ] of
    [] -> []
    xs -> [(var, Num (minimum xs))]
aggTerm bs (OutAgg Sum   var) =
  [(var, Num (sum [ n | b <- bs, Just (Num n) <- [lookup var b] ]))]
aggTerm bs (OutAgg Count var) =
  [(var, Num (length [ () | b <- bs, Just _ <- [lookup var b] ]))]
aggTerm bs (OutVar v) =
  case bs of
    (b:_) -> case lookup v b of
               Just val -> [(v, val)]
               Nothing  -> []
    _     -> []
aggTerm _ _ = []
 
 
buildTriple :: [OutputTerm] -> Binding -> RDFTriple
buildTriple [s, p, o] b = (resolve s, resolve p, resolve o)
  where
    resolve (OutVar   v) = case lookup v b of
                             Just n  -> n
                             Nothing -> error ("Unbound variable: " ++ v)
    resolve (OutAgg _ v) = case lookup v b of
                             Just n  -> n
                             Nothing -> error ("Unbound aggregate variable: " ++ v)
    resolve (OutURI u)   = URI u
    resolve (OutStr s')  = Str s'
    resolve (OutInt i)   = Num i
buildTriple _ _ = error "SELECT needs exactly 3 output terms"
 
 
 

 
evalCond :: Env -> [Binding] -> Condition -> [Binding]
evalCond env bs (Match s p o graph) =
  case lookup graph env of
    Nothing -> error (graph ++ " is undefined")
    Just g  -> nub [ b ++ m | b <- bs, t <- g, m <- matchTriple s p o t
                            , compatible b m ]
evalCond env bs (And c1 c2) =
  evalCond env (evalCond env bs c1) c2
evalCond env bs (Or c1 c2) =
  nub (evalCond env bs c1 ++ evalCond env bs c2)
evalCond env bs (Not c) =
  let inner = evalCond env bs c
  in  filter (\b -> not (any (compatible b) inner)) bs
evalCond _ bs (Gte v n)   = filter (checkNum v (>= n)) bs
evalCond _ bs (Lte v n)   = filter (checkNum v (<= n)) bs
evalCond _ bs (Gt  v n)   = filter (checkNum v (>  n)) bs
evalCond _ bs (Lt  v n)   = filter (checkNum v (<  n)) bs
evalCond _ bs (Eq  v val) = filter (checkEq  v val) bs
evalCond _ bs (Neq v val) = filter (not . checkEq v val) bs
 
 
checkNum :: String -> (Int -> Bool) -> Binding -> Bool
checkNum var f b = case lookup var b of
  Just (Num x) -> f x
  _            -> False
 
 
checkEq :: String -> Value -> Binding -> Bool
checkEq var (ValInt n) b = case lookup var b of
  Just (Num x) -> x == n
  _            -> False
checkEq var (ValStr s) b = case lookup var b of
  Just (Str x) -> x == s
  _            -> False
checkEq var (ValURI u) b = case lookup var b of
  Just (URI x) -> x == u
  _            -> False
checkEq var (ValVar v) b = case (lookup var b, lookup v b) of
  (Just x, Just y) -> x == y
  _                -> False
 
 
 

compatible :: Binding -> Binding -> Bool
compatible b1 b2 =
  all (\(k, v) -> case lookup k b1 of
                    Nothing -> True
                    Just v' -> v == v') b2
 
 
 

 
matchTriple :: Term -> Term -> Term -> RDFTriple -> [Binding]
matchTriple s p o (s', p', o') =
  case (bindTerm s s', bindTerm p p', bindTerm o o') of
    (Just b1, Just b2, Just b3)
      | compatible b1 b2 && compatible (b1 ++ b2) b3 -> [b1 ++ b2 ++ b3]
    _ -> []
 
bindTerm :: Term -> RDFNode -> Maybe Binding
bindTerm (TermVar x) node    = Just [(x, node)]
bindTerm (TermURI x) (URI y) = if x == y then Just [] else Nothing
bindTerm (TermStr x) (Str y) = if x == y then Just [] else Nothing
bindTerm (TermInt x) (Num y) = if x == y then Just [] else Nothing
bindTerm _           _       = Nothing
 
 
 

 
unionEval :: Env -> String -> String -> Graph
unionEval e f1 f2 =
  case (lookup f1 e, lookup f2 e) of
    (Nothing, _)       -> error (f1 ++ " is undefined")
    (_, Nothing)       -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> nub (g1 ++ g2)
 
intersectEval :: Env -> String -> String -> Graph
intersectEval e f1 f2 =
  case (lookup f1 e, lookup f2 e) of
    (Nothing, _)       -> error (f1 ++ " is undefined")
    (_, Nothing)       -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> nub (filter (`elem` g2) g1)
 
minusEval :: Env -> String -> String -> Graph
minusEval e f1 f2 =
  case (lookup f1 e, lookup f2 e) of
    (Nothing, _)       -> error (f1 ++ " is undefined")
    (_, Nothing)       -> error (f2 ++ " is undefined")
    (Just g1, Just g2) -> nub (filter (`notElem` g2) g1)
 
 
 

 
renderGraph :: Graph -> [String]
renderGraph = map renderTriple . sortBy compareTriple . nub
 
renderTriple :: RDFTriple -> String
renderTriple (s, p, o) =
  renderNode s ++ " " ++ renderNode p ++ " " ++ renderNode o ++ " ."
 
renderNode :: RDFNode -> String
renderNode (URI u) = "<" ++ u ++ ">"
renderNode (Str s) = "\"" ++ s ++ "\""
renderNode (Num n) = show n
 
compareTriple :: RDFTriple -> RDFTriple -> Ordering
compareTriple (s1, p1, o1) (s2, p2, o2) =
  compare (renderNode s1) (renderNode s2) <>
  compare (renderNode p1) (renderNode p2) <>
  compare (renderNode o1) (renderNode o2)