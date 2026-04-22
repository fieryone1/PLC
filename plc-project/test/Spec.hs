module Main (main) where

import Control.Monad (unless)
import Control.Exception (try, evaluate, SomeException)
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

-- | Run an IO action and return True if it throws, False if it completes.
throws :: IO a -> IO Bool
throws act = do
  r <- try (act >>= evaluate)
  return (isLeftE r)
  where
    isLeftE :: Either SomeException b -> Bool
    isLeftE (Left _)  = True
    isLeftE (Right _) = False

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


-- =====================================================================
-- Test graphs
-- =====================================================================

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

gPeople :: Graph
gPeople =
  [ (u "alice", u "age", n 30)
  , (u "alice", u "worksFor", u "acme")
  , (u "bob",   u "age", n 17)
  , (u "bob",   u "worksFor", u "acme")
  , (u "carol", u "age", n 45)
  , (u "carol", u "worksFor", u "beta")
  , (u "dave",  u "age", n 22)
  ]

gA :: Graph
gA = [(u "1", u "p", n 1), (u "2", u "p", n 2), (u "3", u "p", n 3)]

gB :: Graph
gB = [(u "2", u "p", n 2), (u "3", u "p", n 3), (u "4", u "p", n 4)]

gKnows :: Graph
gKnows =
  [ (u "alice", u "knows", u "bob")
  , (u "bob",   u "knows", u "alice")
  , (u "carol", u "knows", u "carol")
  , (u "dave",  u "knows", u "alice")
  ]

gEmpty :: Graph
gEmpty = []

env :: Env
env =
  [ ("g",      gMain)
  , ("sub",    gSub)
  , ("price",  gPrice)
  , ("people", gPeople)
  , ("a",      gA)
  , ("b",      gB)
  , ("knows",  gKnows)
  , ("empty",  gEmpty)
  ]


-- =====================================================================
-- Basic sanity
-- =====================================================================

testMatchRepeatedVar :: IO ()
testMatchRepeatedVar = do
  let bad = matchTriple (TermVar "x") (TermURI "p") (TermVar "x") (u "a", u "p", n 10)
  assertEq "repeated variable must unify in a single MATCH" [] bad

testMatchRepeatedVarSucceeds :: IO ()
testMatchRepeatedVarSucceeds = do
  let cond = Match (TermVar "x") (TermURI "knows") (TermVar "x") "knows"
      out = evalCond env [[]] cond
  assertEq "self-loop match should produce one binding" 1 (length out)
  assertTrue "self-loop should be carol" $
    any (\b -> lookup "x" b == Just (u "carol")) out

testAndMatchBeforeComparison :: IO ()
testAndMatchBeforeComparison = do
  let cond = And (Match (TermVar "x") (TermURI "p") (TermVar "o") "g") (Gte "o" 15)
      out = evalCond env [[]] cond
  assertEq "match-before-comparison should produce one binding" 1 (length out)

testAndComparisonBeforeMatchFails :: IO ()
testAndComparisonBeforeMatchFails = do
  let cond = And (Gte "o" 15) (Match (TermVar "x") (TermURI "p") (TermVar "o") "g")
      out = evalCond env [[]] cond
  assertEq "comparison-before-match produces no bindings (documented)" 0 (length out)

testOrDedup :: IO ()
testOrDedup = do
  let c = Match (TermVar "x") (TermURI "type") (TermURI "T") "g"
      out = evalCond env [[]] (Or c c)
  assertEq "OR should deduplicate equal bindings" 1 (length out)

testEqNeq :: IO ()
testEqNeq = do
  let bs = [[("v", n 20)], [("v", n 10)], [("v", n 5)]]
      ge = evalCond env bs (Gte "v" 10)
      ne = evalCond env bs (Neq "v" (ValInt 10))
  assertEq ">= should keep two bindings" 2 (length ge)
  assertEq "!= should remove value-equal binding" 2 (length ne)


-- =====================================================================
-- NOT scenarios
-- =====================================================================

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

testNotWithComparison :: IO ()
testNotWithComparison = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Not (Lt "a" 18))
      out = evalCond env [[]] cond
  assertEq "NOT (a < 18) should keep 3 adults" 3 (length out)
  assertTrue "should NOT contain bob" $
    not $ any (\b -> lookup "s" b == Just (u "bob")) out

testDoubleNot :: IO ()
testDoubleNot = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Not (Not (Gte "a" 18)))
      out = evalCond env [[]] cond
  assertEq "double NOT should equal the original" 3 (length out)

testNotAtTopLevel :: IO ()
testNotAtTopLevel = do
  let cond = Not (Match (TermVar "x") (TermURI "p") (TermVar "o") "g")
      out = evalCond env [[]] cond
  assertEq "top-level NOT of satisfied match returns []" 0 (length out)

testNotAtTopLevelUnsatisfied :: IO ()
testNotAtTopLevelUnsatisfied = do
  let cond = Not (Match (TermVar "x") (TermURI "nonexistent") (TermVar "o") "g")
      out = evalCond env [[]] cond
  assertEq "top-level NOT of unsatisfied match returns [[]]" [[]] out


-- =====================================================================
-- Chained/nested AND
-- =====================================================================

testChainedComparisonsNested :: IO ()
testChainedComparisonsNested = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (And (Gte "a" 18) (Lte "a" 40))
      out = evalCond env [[]] cond
  assertEq "chained age range should find 2 people" 2 (length out)

testChainedComparisonsFlat :: IO ()
testChainedComparisonsFlat = do
  let cond = And (And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                      (Gte "a" 18))
                 (Lte "a" 40)
      out = evalCond env [[]] cond
  assertEq "flat chained age range should find 2 people" 2 (length out)


-- =====================================================================
-- OR scenarios
-- =====================================================================

testOrOfComparisons :: IO ()
testOrOfComparisons = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Or (Lte "a" 17) (Gte "a" 40))
      out = evalCond env [[]] cond
  assertEq "OR of comparisons finds bob and carol" 2 (length out)

testOrBindingDifferentVars :: IO ()
testOrBindingDifferentVars = do
  let cond = Or (Match (TermVar "x") (TermURI "p") (TermVar "v") "g")
                (Match (TermVar "y") (TermURI "type") (TermVar "t") "g")
      out = evalCond env [[]] cond
  assertEq "OR with disjoint variables unions result sets" 4 (length out)

testOrAtTopLevel :: IO ()
testOrAtTopLevel = do
  let cond = Or (Match (TermVar "x") (TermURI "type") (TermURI "T") "g")
                (Match (TermVar "x") (TermURI "type") (TermURI "U") "g")
      out = evalCond env [[]] cond
  assertEq "OR of two matches should bind x=b and x=c" 2 (length out)


-- =====================================================================
-- Cross-graph and joins
-- =====================================================================

testTwoWayJoin :: IO ()
testTwoWayJoin = do
  let cond = And (Match (TermVar "x") (TermURI "type") (TermVar "y") "g")
                 (Match (TermVar "y") (TermURI "sub") (TermVar "z") "sub")
      out = evalCond env [[]] cond
  assertEq "two-way join should produce 3 bindings" 3 (length out)

testThreeWayJoin :: IO ()
testThreeWayJoin = do
  let cond = And (And (Match (TermVar "x") (TermURI "age") (TermVar "a") "people")
                      (Match (TermVar "x") (TermURI "worksFor") (TermVar "e") "people"))
                 (Gte "a" 18)
      out = evalCond env [[]] cond
  assertEq "three-way join filters bob out" 2 (length out)


-- =====================================================================
-- Set operations
-- =====================================================================

testSetOps :: IO ()
testSetOps = do
  let e = [("x", [(u "s", u "p", n 1), (u "s", u "p", n 1)])
          ,("y", [(u "s", u "p", n 1), (u "x", u "p", n 2)])]
      uOut = unionEval e "x" "y"
      iOut = intersectEval e "x" "y"
      mOut = minusEval e "y" "x"
  assertEq "union should deduplicate triples" 2 (length uOut)
  assertEq "intersection should keep one common triple" [(u "s", u "p", n 1)] iOut
  assertEq "minus should remove shared triple" [(u "x", u "p", n 2)] mOut

testIntersectOfSelects :: IO ()
testIntersectOfSelects = do
  let c1 = Match (TermVar "s") (TermURI "p") (TermVar "o") "a"
      c2 = Match (TermVar "s") (TermURI "p") (TermVar "o") "b"
      out1 = evalSelect env [OutVar "s", OutURI "p", OutVar "o"] c1
      out2 = evalSelect env [OutVar "s", OutURI "p", OutVar "o"] c2
      env' = ("r1", out1) : ("r2", out2) : env
      inter = intersectEval env' "r1" "r2"
  assertEq "intersect of two selects finds common triples" 2 (length inter)


-- =====================================================================
-- Empty/tiny graphs
-- =====================================================================

testEmptyGraphMatch :: IO ()
testEmptyGraphMatch = do
  let cond = Match (TermVar "s") (TermVar "p") (TermVar "o") "empty"
      out = evalCond env [[]] cond
  assertEq "match against empty graph returns []" 0 (length out)

testEmptyGraphSelect :: IO ()
testEmptyGraphSelect = do
  let cond = Match (TermVar "s") (TermVar "p") (TermVar "o") "empty"
      out = evalSelect env [OutVar "s", OutVar "p", OutVar "o"] cond
  assertEq "select from empty graph returns []" [] out

testEmptyGraphGroupBy :: IO ()
testEmptyGraphGroupBy = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "empty"
      out = evalSelectGroup env [OutVar "x", OutURI "max", OutAgg Max "p"] cond (GroupBy "x")
  assertEq "GROUP BY over empty match returns []" [] out


-- =====================================================================
-- Aggregate edge cases
-- =====================================================================

testMaxOverMixedTypes :: IO ()
testMaxOverMixedTypes = do
  let cond = Match (TermVar "x") (TermURI "q") (TermVar "v") "g"
      result = evalSelectGroup env [OutVar "x", OutURI "max", OutAgg Max "v"] cond (GroupBy "x")
  didThrow <- throws (evaluate (length (show result)))
  assertTrue "MAX over non-numeric values should error cleanly" didThrow

testCountOverStrings :: IO ()
testCountOverStrings = do
  let cond = Match (TermVar "x") (TermURI "q") (TermVar "v") "g"
      out = evalSelectGroup env [OutVar "x", OutURI "count", OutAgg Count "v"] cond (GroupBy "x")
  assertEq "count should work over string bindings" 2 (length out)
  assertTrue "a has 1 q" ((u "a", u "count", n 1) `elem` out)
  assertTrue "b has 1 q" ((u "b", u "count", n 1) `elem` out)

testGroupMax :: IO ()
testGroupMax = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "maxPrice", OutAgg Max "p"] cond (GroupBy "x")
  assertEq "GROUP BY + MAX should produce one triple per subject" 2 (length out)
  assertTrue "apple max should be 7" ((u "apple", u "maxPrice", n 7) `elem` out)

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

testGroupWithFilter :: IO ()
testGroupWithFilter = do
  let cond = And (Match (TermVar "x") (TermURI "price") (TermVar "p") "price")
                 (Gte "p" 3)
      out = evalSelectGroup env [OutVar "x", OutURI "max", OutAgg Max "p"] cond (GroupBy "x")
  assertEq "grouped with filter, banana excluded" 1 (length out)
  assertTrue "apple max of filtered is 7" ((u "apple", u "max", n 7) `elem` out)


-- =====================================================================
-- Output construction
-- =====================================================================

testSelectAllConstants :: IO ()
testSelectAllConstants = do
  let cond = Match (TermVar "x") (TermURI "p") (TermVar "o") "g"
      out = evalSelect env [OutURI "A", OutURI "B", OutURI "C"] cond
  assertEq "all-constant output deduplicates to 1" 1 (length out)
  assertEq "should be (A,B,C)" [(u "A", u "B", u "C")] out

testSelectRepeatedVar :: IO ()
testSelectRepeatedVar = do
  let cond = Match (TermVar "x") (TermURI "p") (TermVar "o") "g"
      out = evalSelect env [OutVar "x", OutVar "x", OutVar "x"] cond
  assertEq "repeated var in output should work" 2 (length out)
  assertTrue "contains (a,a,a)" ((u "a", u "a", u "a") `elem` out)
  assertTrue "contains (b,b,b)" ((u "b", u "b", u "b") `elem` out)

testSelectUnboundVarErrors :: IO ()
testSelectUnboundVarErrors = do
  let cond = Match (TermVar "x") (TermURI "p") (TermVar "o") "g"
      result = evalSelect env [OutVar "unused", OutURI "p", OutVar "o"] cond
      action = evaluate (length (renderGraph result))
  didThrow <- throws action
  assertTrue "unbound variable in SELECT output should error" didThrow


-- =====================================================================
-- Comparisons on non-numeric types
-- =====================================================================

testGteOnNonNumeric :: IO ()
testGteOnNonNumeric = do
  let bs = [[("q", s "x")], [("q", s "y")]]
      out = evalCond env bs (Gte "q" 5)
  assertEq ">= on string bindings drops them all" 0 (length out)

testEqStringLiteral :: IO ()
testEqStringLiteral = do
  let cond = And (Match (TermVar "x") (TermURI "q") (TermVar "v") "g")
                 (Eq "v" (ValStr "x"))
      out = evalCond env [[]] cond
  assertEq "Eq on string literal finds one" 1 (length out)
  assertTrue "should be x=a" $ any (\b -> lookup "x" b == Just (u "a")) out

testEqUriLiteral :: IO ()
testEqUriLiteral = do
  let cond = And (Match (TermVar "x") (TermURI "type") (TermVar "t") "g")
                 (Eq "t" (ValURI "T"))
      out = evalCond env [[]] cond
  assertEq "Eq on URI literal finds one" 1 (length out)

testEqVarToVar :: IO ()
testEqVarToVar = do
  let cond = And (And (Match (TermVar "x") (TermURI "p") (TermVar "v") "g")
                      (Match (TermVar "y") (TermURI "p") (TermVar "w") "g"))
                 (Eq "v" (ValVar "w"))
      out = evalCond env [[]] cond
  assertEq "var-to-var equality" 2 (length out)


-- =====================================================================
-- Rendering
-- =====================================================================

testRenderSort :: IO ()
testRenderSort = do
  let g =
        [ (u "s", u "p", u "uri")
        , (u "s", u "p", n 1)
        , (u "s", u "p", s "txt")
        ]
      out = renderGraph g
  assertEq "render sort: string, int, URI"
    ["<s> <p> \"txt\" .", "<s> <p> 1 .", "<s> <p> <uri> ."] out

testRenderDedup :: IO ()
testRenderDedup = do
  let g = [(u "a", u "p", n 1), (u "a", u "p", n 1), (u "a", u "p", n 1)]
      out = renderGraph g
  assertEq "render should dedup" 1 (length out)

testRenderLexicographicSubject :: IO ()
testRenderLexicographicSubject = do
  let g = [(u "b", u "p", n 1), (u "aa", u "p", n 1)]
      out = renderGraph g
  assertEq "lex sort by rendered subject"
    ["<aa> <p> 1 .", "<b> <p> 1 ."] out


-- =====================================================================
-- Likely-failing / semantic gotchas
-- =====================================================================

testOrWithPartialBindings :: IO ()
testOrWithPartialBindings = do
  let cond = And (Or (Match (TermVar "s") (TermURI "worksFor") (TermVar "e") "people")
                     (Match (TermVar "s") (TermURI "age") (TermVar "a") "people"))
                 (Gte "a" 18)
      out = evalCond env [[]] cond
  assertEq "OR with partial bindings: filter drops branch without var" 3 (length out)

testAggInSelectWithoutGroupBy :: IO ()
testAggInSelectWithoutGroupBy = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelect env [OutVar "x", OutURI "max", OutAgg Max "p"] cond
  assertTrue "aggregate in plain SELECT silently degrades to var lookup" (length out >= 1)

testGroupByUnboundVar :: IO ()
testGroupByUnboundVar = do
  let cond = Match (TermVar "x") (TermURI "price") (TermVar "p") "price"
      out = evalSelectGroup env [OutVar "x", OutURI "max", OutAgg Max "p"] cond (GroupBy "nonexistent")
  assertEq "GROUP BY unbound var returns []" [] out

testNotInsideOr :: IO ()
testNotInsideOr = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Or (Not (Gte "a" 18)) (Gte "a" 40))
      out = evalCond env [[]] cond
  assertEq "NOT inside OR: under 18 or 40+" 2 (length out)

testNotWithDisjointVars :: IO ()
testNotWithDisjointVars = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Not (Match (TermVar "y") (TermURI "nonexistent") (TermVar "z") "g"))
      out = evalCond env [[]] cond
  assertEq "NOT of unsatisfiable keeps everything" 4 (length out)

testNotDisjointButSatisfied :: IO ()
testNotDisjointButSatisfied = do
  let cond = And (Match (TermVar "s") (TermURI "age") (TermVar "a") "people")
                 (Not (Match (TermVar "x") (TermURI "type") (TermVar "t") "g"))
      out = evalCond env [[]] cond
  assertEq "DOCUMENTING BUG: NOT with disjoint vars wipes all bindings" 0 (length out)

testOutputVarNotInWhere :: IO ()
testOutputVarNotInWhere = do
  let cond = Match (TermVar "s") (TermURI "p") (TermVar "o") "g"
      result = evalSelect env [OutVar "s", OutVar "ghost", OutVar "o"] cond
      action = evaluate (length (renderGraph result))
  didThrow <- throws action
  assertTrue "output var not in WHERE errors" didThrow


-- =====================================================================
-- Main
-- =====================================================================

main :: IO ()
main = do
  putStrLn "=== Basic sanity ==="
  runTest "MATCH repeated variable unification (rejects mismatch)" testMatchRepeatedVar
  runTest "MATCH repeated variable unification (accepts match)" testMatchRepeatedVarSucceeds
  runTest "AND match before comparison" testAndMatchBeforeComparison
  runTest "AND comparison before match is unsupported" testAndComparisonBeforeMatchFails
  runTest "OR deduplicates bindings" testOrDedup
  runTest "Comparison filters" testEqNeq

  putStrLn "\n=== NOT scenarios ==="
  runTest "NOT as negation-as-failure" testNotAsFailure
  runTest "NOT with comparison" testNotWithComparison
  runTest "Double NOT" testDoubleNot
  runTest "NOT at top level (satisfied)" testNotAtTopLevel
  runTest "NOT at top level (unsatisfied)" testNotAtTopLevelUnsatisfied

  putStrLn "\n=== Chained/nested AND ==="
  runTest "Chained comparisons (nested)" testChainedComparisonsNested
  runTest "Chained comparisons (flat)" testChainedComparisonsFlat

  putStrLn "\n=== OR scenarios ==="
  runTest "OR of comparisons" testOrOfComparisons
  runTest "OR with different variables in each branch" testOrBindingDifferentVars
  runTest "OR at top level" testOrAtTopLevel

  putStrLn "\n=== Cross-graph and joins ==="
  runTest "Two-way join (Task 5 style)" testTwoWayJoin
  runTest "Three-way join with filter" testThreeWayJoin

  putStrLn "\n=== Set operations ==="
  runTest "Basic set operators" testSetOps
  runTest "Intersect of two SELECT results" testIntersectOfSelects

  putStrLn "\n=== Empty/tiny graphs ==="
  runTest "Match against empty graph" testEmptyGraphMatch
  runTest "Select from empty graph" testEmptyGraphSelect
  runTest "GROUP BY over empty graph" testEmptyGraphGroupBy

  putStrLn "\n=== Aggregate edge cases ==="
  runTest "MAX over non-numeric values errors cleanly" testMaxOverMixedTypes
  runTest "COUNT over string bindings" testCountOverStrings
  runTest "GROUP BY MAX" testGroupMax
  runTest "GROUP BY MIN" testGroupMin
  runTest "GROUP BY SUM" testGroupSum
  runTest "GROUP BY COUNT" testGroupCount
  runTest "GROUP BY with WHERE filter" testGroupWithFilter

  putStrLn "\n=== Output construction ==="
  runTest "SELECT all-constant output" testSelectAllConstants
  runTest "SELECT repeated variable in output" testSelectRepeatedVar
  runTest "SELECT with unbound variable errors" testSelectUnboundVarErrors

  putStrLn "\n=== Comparisons on non-numeric types ==="
  runTest ">= on non-numeric drops bindings" testGteOnNonNumeric
  runTest "Eq with string literal" testEqStringLiteral
  runTest "Eq with URI literal" testEqUriLiteral
  runTest "Eq var-to-var" testEqVarToVar

  putStrLn "\n=== Rendering ==="
  runTest "Render sort order" testRenderSort
  runTest "Render dedup" testRenderDedup
  runTest "Lex sort subject" testRenderLexicographicSubject

  putStrLn "\n=== Likely-failing / semantic gotchas ==="
  runTest "OR with partial bindings, filter uses one branch's var" testOrWithPartialBindings
  runTest "Aggregate in plain SELECT (no GROUP BY)" testAggInSelectWithoutGroupBy
  runTest "GROUP BY unbound variable" testGroupByUnboundVar
  runTest "NOT inside OR" testNotInsideOr
  runTest "NOT with disjoint unsatisfiable inner" testNotWithDisjointVars
  runTest "NOT with disjoint satisfied inner (known semantic gotcha)" testNotDisjointButSatisfied
  runTest "Output var not in WHERE" testOutputVarNotInWhere

  putStrLn "\nAll tests completed."