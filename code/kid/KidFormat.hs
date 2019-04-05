module KidFormat(prettifyKP) where

import Data.List (intercalate)

import KidAST

-----------------------------------
-- Pretty-printer for Kid Programs
-----------------------------------

-- prettifyKP
-- produce a string representing a cleanly-structured
-- KidProgram for pretty-printing
prettifyKP :: KidProgram -> String
prettifyKP kp
  -- we'll do so by forming the program as a series of 'lines'
  -- and then merging them into one big String right at the end:
  = unlines (lineupProg kp)

-- indent
-- helper function to add 4 spaces to the start of a line
indent :: String -> String
indent line = (' ':' ':' ':' ':line)

-- lineupProg
-- convert a KidProgram into a cleanly-structured list of lines
lineupProg :: KidProgram -> [String]
lineupProg (Program decls stmts)
  = concat  [ ["proc main ()"]
            , map indent (concatMap lineupDecl decls)
            , ["begin"]
            , map indent (concatMap lineupStmt stmts)
            , ["end"]
            ]

-- lineupDecl
-- convert a declaration into a (singleton list of) cleanly-formatted lines
lineupDecl :: Decl -> [String]
lineupDecl (Decl ident baseType)
  = [ formatType baseType ++ " " ++ ident ++ ";" ]

-- lineupStmt
-- convert a statement into a list of cleanly-formatted lines;
lineupStmt :: Stmt -> [String]
lineupStmt (Assign (LId ident) expr)
  = [ ident ++ " := " ++ formatExpr expr ++ ";" ]
lineupStmt (Read (LId ident))
  = [ "read " ++ ident ++ ";" ]
lineupStmt (Write expr)
  = [ "write " ++ formatExpr expr ++ ";" ]
lineupStmt (Call ident exprs)
  = [ "call " ++ ident ++ "(" ++ formatExprList exprs ++ ");" ]

-- this may involve recursively formatting and indenting lists
-- of statements within the bodies of composite expressions:
lineupStmt (If expr stmts)
  = concat  [ ["if " ++ formatExpr expr ++ " then"]
            , map indent (concatMap lineupStmt stmts)
            , ["fi"]
            ]
lineupStmt (IfElse expr stmts1 stmts2)
  = concat  [ ["if " ++ formatExpr expr ++ " then"]
            , map indent (concatMap lineupStmt stmts1)
            , ["else"]
            , map indent (concatMap lineupStmt stmts2)
            , ["fi"]
            ]
lineupStmt (While expr stmts)
  = concat  [ ["while " ++ formatExpr expr ++ " do"]
            , map indent (concatMap lineupStmt stmts)
            , ["od"]
            ]

-- formatType
-- represent a type declaration as a String
formatType :: BaseType -> String
formatType BoolType
  = "bool"
formatType IntType
  = "int"

-- formatExprList
-- cleanly represent a comma-separated list of expressions as a single String
formatExprList :: [Expr] -> String
formatExprList exprs
  = intercalate ", " $ map formatExpr exprs

-- formatExpr
-- cleanly represent an expression as a String
formatExpr :: Expr -> String
formatExpr (BoolConst bool)
  | bool      = "true"
  | otherwise = "false"
formatExpr (IntConst int)
  = show int
formatExpr (StrConst str)
  = "\"" ++ str ++ "\""
formatExpr (Id ident)
  = ident
-- this may involve parenthesising subexpressions if they contain binary 
-- operations themselves, hence the call to formatExprParens
formatExpr (Add expr1 expr2)
  = formatExprParens expr1 ++ " + " ++ formatExprParens expr2
formatExpr (Mul expr1 expr2)
  = formatExprParens expr1 ++ " * " ++ formatExprParens expr2
formatExpr (UnaryMinus expr)
  = "-" ++ formatExprParens expr

-- formatExprParens
-- cleanly represent an expression as a String, surrounded with parentheses
-- if it represents a binary operation (or just normally otherwise)
formatExprParens :: Expr -> String
formatExprParens expr@(Add _ _)
  = "(" ++ formatExpr expr ++ ")"
formatExprParens expr@(Mul _ _)
  = "(" ++ formatExpr expr ++ ")"
formatExprParens expr
  = formatExpr expr
