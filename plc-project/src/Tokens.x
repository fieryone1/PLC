{ 
module Tokens where 
}

%wrapper "basic" 

$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9\_]
$digit = [0-9]

tokens :-

  $white+                          ;
  "--".*                           ;

  "<-"                             { \_ -> TokenAssign }
  "("                              { \_ -> TokenLParen }
  ")"                              { \_ -> TokenRParen }
  ","                              { \_ -> TokenComma }
  "!="                             { \_ -> TokenNeq }
  ">="                             { \_ -> TokenGeq }
  "<="                             { \_ -> TokenLeq }
  ">"                              { \_ -> TokenGt }
  "<"                              { \_ -> TokenLt }
  "="                              { \_ -> TokenEq }

  "SELECT"                         { \_ -> TokenSelect }
  "WHERE"                          { \_ -> TokenWhere }
  "MATCH"                          { \_ -> TokenMatch }
  "IN"                             { \_ -> TokenIn }
  "AND"                            { \_ -> TokenAnd }
  "OR"                             { \_ -> TokenOr }
  "NOT"                            { \_ -> TokenNot }
  "LOAD"                           { \_ -> TokenLoad }
  "PRINT"                          { \_ -> TokenPrint }
  "UNION"                          { \_ -> TokenUnion }
  "INTERSECT"                      { \_ -> TokenIntersect }
  "MINUS"                          { \_ -> TokenMinus }
  "GROUP"                          { \_ -> TokenGroup }
  "BY"                             { \_ -> TokenBy }
  "MAX"                            { \_ -> TokenMax }
  "MIN"                            { \_ -> TokenMin }
  "COUNT"                          { \_ -> TokenCount }
  "SUM"                            { \_ -> TokenSum }

  $digit+                          { \s -> TokenInteger (read s) }
  \? $alpha $alnum*                { \s -> TokenVar (tail s) }
  \< [^ \>\<\t\n]+ \>              { \s -> TokenUri (init (tail s)) }                    
  \" [^\"]* \"                     { \s -> TokenString (init (tail s)) }
  $alpha $alnum*                   { \s -> TokenIdent s }

{
data Token = TokenAssign
           | TokenLParen
           | TokenRParen
           | TokenComma
           | TokenEq
           | TokenNeq
           | TokenGeq
           | TokenLeq
           | TokenGt
           | TokenLt
           | TokenSelect
           | TokenWhere
           | TokenMatch
           | TokenIn
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenLoad
           | TokenPrint
           | TokenUnion
           | TokenIntersect
           | TokenMinus
           | TokenGroup
           | TokenBy
           | TokenMax
           | TokenMin
           | TokenCount
           | TokenSum
           | TokenInteger Int
           | TokenVar String
           | TokenUri String
           | TokenString String
           | TokenIdent String
           deriving (Show)
}