{
-- module GoatParser where
module Main (main, parse) where   -- for standalone testing
import GoatLexer
import GoatAST

import Data.List (intersperse)
}

%name p
%tokentype { Token }
%error { parseError }

%left     or
%left     and
%nonassoc '!'
%nonassoc '=' neq '<' lte '>' gte
%left     '+' '-'
%left     '*' '/'
%right    NEG

%token
  -- keywords
  key_begin { BEGIN }
  key_bool  { BOOL }
  key_call  { CALL }
  key_do    { DO }
  key_else  { ELSE }
  key_end   { END }
  key_fi    { FI }
  key_float { FLOAT }
  key_if    { IF }
  key_int   { INT }
  key_od    { OD }
  key_proc  { PROC }
  key_read  { READ }
  key_ref   { REF }
  key_then  { THEN }
  key_val   { VAL }
  key_while { WHILE }
  key_write { WRITE }
  -- assignment, parens, brackets, comma, semicolon
  asg       { ASSIGN }
  '('       { LPAREN }
  ')'       { RPAREN }
  '['       { LBRACKET }
  ']'       { RBRACKET }
  ','       { COMMA }
  ';'       { SEMI }
  -- binary arithmetic operators
  '+'       { ADD }
  '-'       { SUB }   -- distinct from NEG token in precedence directive above
  '*'       { MUL }
  '/'       { DIV }
  -- binary relational operators
  '='       { EQU }
  neq       { NEQ }
  '<'       { LTH }
  lte       { LTE }
  '>'       { GTH }
  gte       { GTE }
  -- binary boolean operators
  and       { AND }
  or        { OR }
  -- boolean complement
  '!'       { NOT }
  -- constants
  boolconst { BOOL_CONST $$ }
  fracconst { FRACTIONAL_CONST $$ }
  intconst  { INTEGRAL_CONST $$ }
  strconst  { STR $$ }
  -- identifier
  ident     { IDENT $$ }
%%

Goat :: { GoatProgram }
  : Procs                               { GoatProgram $1 }

-- one or more procedures
Procs :: { [Proc] }
  : Proc                                { [$1] }
  | Proc Procs                          { $1:$2 }

Proc :: { Proc }
  : key_proc ident '(' Params ')' Decls key_begin Stmts key_end
                                        { Proc $2 $4 $6 $8 }

-- zero or more parameters separated by commas
Params :: { [Param] }
  :                                     { [] }
  | Params1                             { $1 }

-- one or more parameters separated by commas
Params1 :: { [Param] }
  : Param                               { [$1] }
  | Param ',' Params1                   { $1:$3 }

Param :: { Param }
  : PassBy BaseType ident               { Param $1 $2 $3 }

PassBy :: { PassBy }
  : key_val                             { Val }
  | key_ref                             { Ref }

BaseType :: { BaseType }
  : key_bool                            { BoolType }
  | key_float                           { FloatType }
  | key_int                             { IntType }

-- zero or more declarations
Decls :: { [Decl] }
  :                                     { [] }
  | Decl Decls                          { $1:$2 }

Decl :: { Decl }
  : BaseType ident Dim ';'              { Decl $1 $2 $3 }

Dim :: { Dim }
  :                                     { Dim0 }
  | '[' intconst ']'                    { Dim1 $2 }
  | '[' intconst ']''[' intconst ']'    { Dim2 $2 $5 }

-- one or more statements
Stmts :: { [Stmt] }
  : Stmt                                { [$1] }
  | Stmt Stmts                          { $1:$2 }

Stmt :: { Stmt }
  : Var asg Expr ';'                    { Asg $1 $3 }
  | key_read Var ';'                    { Read $2 }
  | key_write Expr ';'                  { Write $2 }
  | key_call ident '(' Exprs ')' ';'    { Call $2 $4 }
  | key_if Expr key_then Stmts key_fi   { If $2 $4 }
  | key_if Expr key_then Stmts key_else Stmts key_fi
                                        { IfElse $2 $4 $6 }
  | key_while Expr key_do Stmts key_od  { While $2 $4 }

Var :: { Var }
  : ident                               { Var0 $1 }
  | ident '[' Expr ']'                  { Var1 $1 $3 }
  | ident '[' Expr ']' '[' Expr ']'     { Var2 $1 $3 $6 }

-- zero or more expressions separated by commas
Exprs :: { [Expr] }
  :                                     { [] }
  | Exprs1                              { $1 }

-- one or more expressions separated by commas
Exprs1 :: { [Expr] }
  : Expr                                { [$1] }
  | Expr ',' Exprs                      { $1:$3 }

Expr :: { Expr }
  : Var                                 { VarExpr $1 }
  | boolconst                           { BoolConst $1 }
  | fracconst                           { FloatConst $1 }
  | intconst                            { IntConst $1 }
  | strconst                            { StrConst $1 }
  | '(' Expr ')'                        { $2 }
  | Expr BinOp Expr                     { BinExpr $2 $1 $3 }
  | UnOp Expr                           { UnExpr $1 $2 }

BinOp :: { BinOp }
  : '+'                                 { Add }
  | '-'                                 { Sub }
  | '*'                                 { Mul }
  | '/'                                 { Div }
  | '='                                 { Equ }
  | neq                                 { NEq }
  | '<'                                 { LTh }
  | lte                                 { LEq }
  | '>'                                 { GTh }
  | gte                                 { GEq }
  | and                                 { And }
  | or                                  { Or }

UnOp :: { UnOp }
  : '!'                                 { Not }
  | '-' %prec NEG                       { Neg }

{
parse :: String -> GoatProgram
parse s
  = p $ lexer s

parseError :: [Token] -> a
parseError tks
  = error $ "Parse error at token " ++
      spaced_list (map show (take 12 tks)) ++ " ..."

spaced_list :: [String] -> String
spaced_list
  = concat . intersperse " "

main
  = do
      s <- getContents
      print (parse s)
}
