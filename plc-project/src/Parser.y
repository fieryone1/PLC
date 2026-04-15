{
module Parser where
import Tokens
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    '<-'      { TokenAssign }
    '('       { TokenLParen }
    ')'       { TokenRParen }
    ','       { TokenComma }
    '='       { TokenEq }
    '!='      { TokenNeq }
    '>='      { TokenGeq }
    '<='      { TokenLeq }
    '>'       { TokenGt }
    '<'       { TokenLt }
    SELECT    { TokenSelect }
    WHERE     { TokenWhere }
    MATCH     { TokenMatch }
    IN        { TokenIn }
    AND       { TokenAnd }
    OR        { TokenOr }
    NOT       { TokenNot }
    LOAD      { TokenLoad }
    PRINT     { TokenPrint }
    UNION     { TokenUnion }
    INTERSECT { TokenIntersect }
    MINUS     { TokenMinus }
    GROUP     { TokenGroup }
    BY        { TokenBy }
    MAX       { TokenMax }
    MIN       { TokenMin }
    COUNT     { TokenCount }
    SUM       { TokenSum }
    int       { TokenInteger $$ }
    var       { TokenVar $$ }
    uri       { TokenUri $$ }
    str       { TokenString $$ }
    ident     { TokenIdent $$ }

%left OR
%left AND
%right NOT


%%

Program : Prog                                              { reverse $1 }

Prog : Statement                                            { [$1] }
     | Prog Statement                                       { $2 : $1 }

Statement : ident '<-' Expr                                 { Assign $1 $3 }
          | PRINT ident                                     { Print $2 }

Expr : LOAD str                                             { Load $2 }
     | ident UNION ident                                    { Union $1 $3 }
     | ident INTERSECT ident                                { Intersect $1 $3 }
     | ident MINUS ident                                    { Minus $1 $3 }
     | SelectExpr                                           { $1 }

SelectExpr : SELECT '(' OutputList ')' WhereClause GroupBy  { SelectGroup (reverse $3) $5 $6 }
           | SELECT '(' OutputList ')' WhereClause          { Select (reverse $3) $5 }

WhereClause : WHERE Condition                               { $2 }

GroupBy : GROUP BY var                                      { GroupBy $3 }

OutputList : OutputTerm                                     { [$1] }
           | OutputList ',' OutputTerm                      { $3 : $1 }

OutputTerm : var                                            { OutVar $1 }
           | uri                                            { OutURI $1 }
           | str                                            { OutStr $1 }
           | int                                            { OutInt $1 }
           | AggFunc '(' var ')'                            { OutAgg $1 $3 }

AggFunc : MAX                                               { Max }
        | MIN                                               { Min }
        | COUNT                                             { Count }
        | SUM                                               { Sum }

Condition : MatchExpr                                       { $1 }
          | Comparison                                      { $1 }
          | Condition AND Condition                          { And $1 $3 }
          | Condition OR Condition                           { Or $1 $3 }
          | NOT Condition                                    { Not $2 }
          | '(' Condition ')'                                { $2 }

Comparison : var '=' Value                                  { Eq $1 $3 }
           | var '!=' Value                                 { Neq $1 $3 }
           | var '>=' int                                   { Gte $1 $3 }
           | var '<=' int                                   { Lte $1 $3 }
           | var '>' int                                    { Gt $1 $3 }
           | var '<' int                                    { Lt $1 $3 }

Value : var                                                 { ValVar $1 }
      | uri                                                 { ValURI $1 }
      | str                                                 { ValStr $1 }
      | int                                                 { ValInt $1 }

MatchExpr : MATCH '(' Term ',' Term ',' Term ')' IN ident   { Match $3 $5 $7 $10 }

Term : var                                                  { TermVar $1 }
     | uri                                                  { TermURI $1 }
     | str                                                  { TermStr $1 }
     | int                                                  { TermInt $1 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error: unexpected end of input"
parseError (t:_) = error ("Parse error: unexpected token " ++ show t)

data Statement = Assign String Expr
               | Print String
               deriving (Show, Eq)
           

data Expr = Load String
          | Union String String
          | Intersect String String
          | Minus String String
          | Select [OutputTerm] Condition
          | SelectGroup [OutputTerm] Condition GroupByClause
          deriving (Show, Eq)

data GroupByClause = GroupBy String
                   deriving (Show, Eq)

data OutputTerm = OutVar String
                | OutURI String
                | OutStr String
                | OutInt Int
                | OutAgg AggFunc String
                deriving (Show, Eq)

data AggFunc = Max | Min | Count | Sum
             deriving (Show, Eq)

data Condition = Match Term Term Term String
               | Eq String Value
               | Neq String Value
               | Gte String Int
               | Lte String Int
               | Gt String Int
               | Lt String Int
               | And Condition Condition
               | Or Condition Condition
               | Not Condition
               deriving (Show, Eq)

data Value = ValVar String
           | ValURI String
           | ValStr String
           | ValInt Int
           deriving (Show, Eq)

data Term = TermVar String
          | TermURI String
          | TermStr String
          | TermInt Int
          deriving (Show, Eq)
}