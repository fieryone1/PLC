module Main (main) where

import Control.Monad (unless)
import Parser
import Part2Eval
import Rdf

assertTrue :: String -> Bool -> IO ()
assertTrue msg cond =
  unless cond (error ("Assertion failed: " ++ msg))

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual =
  unless (expected == actual) $ error (
    "Assertion failed: " ++ msg
      ++ "\nExpected: " ++ show expected
      ++ "\nActual:   " ++ show actual)

runTest :: String -> IO () -> IO ()
runTest name action = do
  putStrLn ("[TEST] " ++ name)
  action
  putStrLn "  OK"

u :: String -> RDFNode
u = URI

n :: Int -> RDFNode
n = Num

s :: String -> RDFNode
s = Str

gMain :: Graph
gMain =
  [ (u "a", u "p", n 10)
  , (u "a", u "q", s "x")
  , (u "b", u "p", n 20)
  , (u "b", u "q", s "y")
  , (u "b", u "type", u "T")
  , (u "c", u "type", u "U")
  ]

gSub :: Graph
gSub =
  [ (u "T", u "sub", u "Top")
  , (u "U", u "sub", u "Top")
  , (u "U", u "sub", u "Thing")
  ]

gPrice :: Graph
gPrice =
  [ (u "apple", u "price", n 3)
  , (u "apple", u "price", n 7)
  , (u "banana", u "price", n 2)
  ]

env :: Env
env =
  [ ("g", gMain)
  , ("sub", gSub)
  , ("price", gPrice)
  ]

testMatchRepeatedVar :: IO ()
testMatchRepeatedVar = do
  let bad = matchTriple (TermVar "x") (TermURI "p") (TermVar "x") (u "a", u "p", n 10)
  assertEq "repeated variable must unify in a single MATCH" [] bad

testAndMatchBeforeComparison :: IO ()
testAndMatchBeforeComparison = do
  let cond = And (Match (TermVar "x") (TermURI "p") (TermVar "o") "g") (Gte "o" 15)
      out = evalCond env [[]] cond
  assertEq "match-before-comparison should produce one binding" 1 (length out)
  assertTrue "binding should be x=b,o=20" $
    any (\b -> lookup "x" b == Just (u "b") && lookup "o" b == Just (n 20)) out

testAndComparisonBeforeMatchFails :: IO ()
testAndComparisonBeforeMatchFails = do
  let cond = And (Gte "o" 15) (Match (TermVar "x") (TermURI "p") (TermVar "o") "g")
      out = evalCond env [[]] cond
  assertEq "comparison-before-match is not supported and produces no bindings" 0 (length out)
testOrDedup :: IO ()
testOrDedup = do
  let c = Match (TermVar "x") (TermURI "type") (TermURI "T") "g"
      out = evalCond env [[]] (Or c c)
  assertEq "OR should deduplicate equal bindings" 1 (length out)

testNotAsFailure :: IO ()
testNotAsFailure = do
  let cond =
        And
          (Match (TermVar "x") (TermURI "type") (TermVar "t") "g")
          (Not (Match (TermVar "t") (TermURI "sub") (TermURI "Thing") "sub"))
      out = evalCond env [[]] cond
  assertEq "NOT should filter out bindings with a matching extension" 1 (length out)
  assertTrue "remaining solution should be x=b,t=T" $
    any (\b -> lookup "x" b == Just (u "b") && lookup "t" b == Just (u "T")) out

testEqNeq :: IO ()
testEqNeq = do
  let bs = [[("v", n 20)], [("v", n 10)], [("v", n 5)]]
      ge = evalCond env bs (Gte "v" 10)
      ne = evalCond env bs (Neq "v" (ValInt 10))
  assertEq ">= should keep two bindings" 2 (length ge)
  assertEq "!= should remove value-equal binding" 2 (length ne)

testSelectBuild :: IO ()
testSelectBuild = do
  let cond = Match (TermVar "x") (TermURI "p") (TermVar "o") "g"
      out = evalSelect env [OutVar "x", OutURI "projected", OutVar "o"] cond
  assertEq "SELECT should project two triples" 2 (length out)
  assertTrue "selected graph should contain (b,projected,20)" $
    (u "b", u "projected", n 20) `elem` out

testGroupMax :: IO ()
testGroupMax = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "maxPrice", OutAgg Max "p"] cond (GroupBy "x")
  assertEq "GROUP BY + MAX should produce one triple per subject" 2 (length out)
  assertTrue "apple max should be 7" ((u "apple", u "maxPrice", n 7) `elem` out)
  assertTrue "banana max should be 2" ((u "banana", u "maxPrice", n 2) `elem` out)

testGroupMin :: IO ()
testGroupMin = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "minPrice", OutAgg Min "p"] cond (GroupBy "x")
  assertTrue "apple min should be 3" ((u "apple", u "minPrice", n 3) `elem` out)

testGroupSum :: IO ()
testGroupSum = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "sumPrice", OutAgg Sum "p"] cond (GroupBy "x")
  assertTrue "apple sum should be 10" ((u "apple", u "sumPrice", n 10) `elem` out)

testGroupCount :: IO ()
testGroupCount = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "countPrice", OutAgg Count "p"] cond (GroupBy "x")
  assertTrue "apple count should be 2" ((u "apple", u "countPrice", n 2) `elem` out)
  assertTrue "banana count should be 1" ((u "banana", u "countPrice", n 1) `elem` out)

testSetOps :: IO ()
testSetOps = do
  let e = [("a", [(u "s", u "p", n 1), (u "s", u "p", n 1)]), ("b", [(u "s", u "p", n 1), (u "x", u "p", n 2)])]
      uOut = unionEval e "a" "b"
      iOut = intersectEval e "a" "b"
      mOut = minusEval e "b" "a"
  assertEq "union should deduplicate triples" 2 (length uOut)
  assertEq "intersection should keep one common triple" [(u "s", u "p", n 1)] iOut
  assertEq "minus should remove shared triple" [(u "x", u "p", n 2)] mOut

testRenderSort :: IO ()
testRenderSort = do
  let g =
        [ (u "s", u "p", u "uri")
        , (u "s", u "p", n 1)
        , (u "s", u "p", s "txt")
        ]
      out = renderGraph g
  assertEq "render sort should be string then int then URI" 
    ["<s> <p> \"txt\" .", "<s> <p> 1 .", "<s> <p> <uri> ."] out

main :: IO ()
main = do
  runTest "MATCH repeated variable unification" testMatchRepeatedVar
  runTest "AND match before comparison" testAndMatchBeforeComparison
  runTest "AND comparison before match is unsupported" testAndComparisonBeforeMatchFails
  runTest "OR deduplicates bindings" testOrDedup
  runTest "NOT as negation-as-failure" testNotAsFailure
  runTest "Comparison filters" testEqNeq
  runTest "SELECT projection" testSelectBuild
  runTest "GROUP BY MAX" testGroupMax
  runTest "GROUP BY MIN" testGroupMin
  runTest "GROUP BY SUM" testGroupSum
  runTest "GROUP BY COUNT" testGroupCount
  runTest "Graph set operators" testSetOps
  runTest "Rendered output ordering" testRenderSort
