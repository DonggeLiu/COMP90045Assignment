{
module GoatLexer (Token(..), lexer) where
-- module Main (main, Token(..), lexer) where  -- for standalone testing
import Data.List (init, tail, intercalate)
import Data.List.Split (splitOn)
}

%wrapper "basic"

$digit       = 0-9
@digits      = $digit+
@alpha       = [a-zA-Z]
@stringlit   = \" [^\"\n\t]* \"
@ident       = @alpha (@alpha | $digit | \_ | \')*
@comment     = \# [^\n]* \n

rules :-
  $white+             ;
  @comment            ;
  -- reserved words (excl. true, false)
  begin               { \s -> BEGIN }
  bool                { \s -> BOOL }
  call                { \s -> CALL }
  do                  { \s -> DO }
  else                { \s -> ELSE }
  end                 { \s -> END }
  fi                  { \s -> FI }
  float               { \s -> FLOAT }
  if                  { \s -> IF }
  int                 { \s -> INT }
  od                  { \s -> OD }
  proc                { \s -> PROC }
  read                { \s -> READ }
  ref                 { \s -> REF }
  then                { \s -> THEN }
  val                 { \s -> VAL }
  while               { \s -> WHILE }
  write               { \s -> WRITE }
  -- assignment, parens, brackets, comma, semicolon
  :=                  { \s -> ASSIGN }
  \(                  { \s -> LPAREN }
  \)                  { \s -> RPAREN }
  \[                  { \s -> LBRACKET }
  \]                  { \s -> RBRACKET }
  \,                  { \s -> COMMA }
  \;                  { \s -> SEMI }
  -- binary arithmetic operators
  \+                  { \s -> ADD }
  \*                  { \s -> MUL }
  \/                  { \s -> DIV }
  -- binary relational operators
  =                   { \s -> EQU }
  !=                  { \s -> NEQ }
  \<                  { \s -> LTH }
  \<=                 { \s -> LTE }
  \>                  { \s -> GTH }
  \>=                 { \s -> GTE }
  -- binary boolean operators
  \&\&                { \s -> AND }
  \|\|                { \s -> OR }
  -- binary or unary minus
  \-                  { \s -> SUB }
  -- boolean complement
  !                   { \s -> NOT }
  -- boolean constants
  true                { \s -> BOOL_CONST True }
  false               { \s -> BOOL_CONST False }
  -- integral numerical constant (number with no fractional component)
  -- note: this may represent an int or float; parser must infer from context
  @digits             { \s -> INTEGRAL_CONST (read s :: Int) }
  -- fractional numerical constant
  -- note: greediness ensures that a fractional number will assume this token
  @digits\.@digits    { \s -> FRACTIONAL_CONST (read s :: Float) }
  -- string literals
  @stringlit          { \s -> STR (interpret s) }
  -- identifier
  @ident              { \s -> IDENT s }

{
data Token
  -- reserved words (excl. true, false)
  = BEGIN | BOOL | CALL | DO | ELSE | END | FI | FLOAT | IF | INT | OD | PROC
  | READ | REF | THEN | VAL | WHILE | WRITE
  -- assignment (:=)
  | ASSIGN
  -- parens, brackets, comma, semicolon
  | LPAREN | RPAREN | LBRACKET | RBRACKET | COMMA | SEMI
  -- binary arithmetic operators
  | ADD | MUL | DIV
  -- binary relational operators
  | EQU | NEQ | LTH | LTE | GTH | GTE
  -- binary boolean operators
  | AND | OR
  -- binary or unary minus
  | SUB
  -- boolean complement
  | NOT
  -- constants/literals
  | INTEGRAL_CONST Int | FRACTIONAL_CONST Float | BOOL_CONST Bool | STR String
  -- identifier
  | IDENT String
    deriving (Eq, Show)

-- Remove outer quotes and replace all "\n" escape sequences with newlines
interpret :: String -> String
interpret s
  = replace "\\n" "\n" $ (init . tail) s

-- Replace all occurrences of a query substring with a replacement string
-- (works more generally on lists of elements in the Eq typeclass)
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace query replacement
  = intercalate replacement . splitOn query


lexer = alexScanTokens

-- main
--   = do
--       s <- getContents
--       print (alexScanTokens s)
}
