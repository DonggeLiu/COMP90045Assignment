module GoatFormat (
  prettify
) where

-- ----------------------------------------------------------------------------
-- ORIGINAL GOAT FORMAT ALGORITHM USING LOTS OF LISTS AND CONCATENATION
-- DEPRECATED IN FAVOUR OF MONADIC STRING-BUILDER APPROACH
-- ----------------------------------------------------------------------------

import Data.List (intercalate)

import GoatAST

-- prettify
-- produce a string representing a cleanly-structured
-- GoatProgram for pretty-printing
prettify :: GoatProgram -> String
prettify gp
  -- we'll do so by forming the program as a series of 'lines'
  -- and then merging them into one big String right at the end:
  = unlines (lineupProg gp)



-- lineupProg
-- convert a GoatProgram into a cleanly-structured list of lines
lineupProg :: GoatProgram -> [String]
lineupProg (GoatProgram procs)
  = intercalate [""] $ map lineupProc procs

lineupProc :: Proc -> [String]
lineupProc (Proc (Id name) params decls stmts)
  = concat  [ ["proc "++name++" ("++sepByComma (map formatParam params)++")"]
            , map indent (concatMap lineupDecl decls)
            , ["begin"]
            , map indent (concatMap lineupStmt stmts)
            , ["end"]
            ]

-- lineupDecl
-- convert a declaration into a (singleton list of) cleanly-formatted lines
lineupDecl :: Decl -> [String]
lineupDecl (Decl baseType (Id name) dim)
  = [ formatType baseType ++ " " ++ name ++ formatDim dim ++ ";" ]

-- lineupStmt
-- convert a statement into a list of cleanly-formatted lines;
lineupStmt :: Stmt -> [String]
lineupStmt (Asg var expr)
  = [ formatVar var ++ " := " ++ formatExpr expr ++ ";" ]
lineupStmt (Read var)
  = [ "read " ++ formatVar var ++ ";" ]
lineupStmt (Write expr)
  = [ "write " ++ formatExpr expr ++ ";" ]
lineupStmt (Call (Id name) exprs)
  = [ "call " ++ name ++ "(" ++ sepByComma (map formatExpr exprs) ++ ");" ]
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

-- formatParam
-- represent a formal paramater as a string
formatParam :: Param -> String
formatParam (Param passBy baseType (Id name))
  = formatPassBy passBy ++ " " ++ formatType baseType ++ " " ++ name

-- formatPassBy
-- represent a paramater passing mechanism as a String
formatPassBy :: PassBy -> String
formatPassBy Val
  = "val"
formatPassBy Ref
  = "ref"

-- formatType
-- represent a type declaration as a String
formatType :: BaseType -> String
formatType BoolType
  = "bool"
formatType FloatType
  = "float"
formatType IntType
  = "int"

-- formatVar
-- represent a variable as a string
formatVar :: Var -> String
formatVar (Var0 (Id name))
  = name
formatVar (Var1 (Id name) expr)
  = name ++ wrapBrackets (formatExpr expr)
formatVar (Var2 (Id name) expr1 expr2)
  = name ++ wrapBrackets (sepByComma $ map formatExpr [expr1, expr2])

formatDim :: Dim -> String
formatDim (Dim0)
  = ""
formatDim (Dim1 n)
  = wrapBrackets $ show n
formatDim (Dim2 n m)
  = wrapBrackets $ show n ++ "," ++ show m

-- formatExpr
-- cleanly represent an expression as a String
formatExpr :: Expr -> String
formatExpr (BoolConst bool)
  | bool      = "true"
  | otherwise = "false"
formatExpr (IntConst int)
  = show int
formatExpr (FloatConst float)
  = show float
formatExpr (StrConst str)
  = "\"" ++ str ++ "\""
formatExpr (VarExpr var)
  = formatVar var
-- this may involve parenthesising subexpressions if they contain binary 
-- operations themselves, hence the calls to formatExprParens:
formatExpr (Add expr1 expr2)
  = formatExprParens expr1 ++ " + " ++ formatExprParens expr2
formatExpr (Sub expr1 expr2)
  = formatExprParens expr1 ++ " - " ++ formatExprParens expr2
formatExpr (Mul expr1 expr2)
  = formatExprParens expr1 ++ " * " ++ formatExprParens expr2
formatExpr (Div expr1 expr2)
  = formatExprParens expr1 ++ " / " ++ formatExprParens expr2
formatExpr (Neg expr)
  = "-" ++ formatExprParens expr

formatExpr (Equ expr1 expr2)
  = formatExprParens expr1 ++ " = " ++ formatExprParens expr2
formatExpr (NEq expr1 expr2)
  = formatExprParens expr1 ++ " != " ++ formatExprParens expr2
formatExpr (LTh expr1 expr2)
  = formatExprParens expr1 ++ " < " ++ formatExprParens expr2
formatExpr (LEq expr1 expr2)
  = formatExprParens expr1 ++ " <= " ++ formatExprParens expr2
formatExpr (GTh expr1 expr2)
  = formatExprParens expr1 ++ " > " ++ formatExprParens expr2
formatExpr (GEq expr1 expr2)
  = formatExprParens expr1 ++ " >= " ++ formatExprParens expr2

formatExpr (And expr1 expr2)
  = formatExprParens expr1 ++ " && " ++ formatExprParens expr2
formatExpr (Or expr1 expr2)
  = formatExprParens expr1 ++ " || " ++ formatExprParens expr2
formatExpr (Not expr)
  = "!" ++ formatExprParens expr

-- formatExprParens
-- cleanly represent an expression as a String, surrounded with parentheses
-- if it represents a binary operation (or just normally otherwise)
formatExprParens :: Expr -> String
formatExprParens expr
  = case expr of
      -- wrap binary operations in parens
      Add _ _ -> wrapParens $ formatExpr expr
      Sub _ _ -> wrapParens $ formatExpr expr
      Mul _ _ -> wrapParens $ formatExpr expr
      Div _ _ -> wrapParens $ formatExpr expr
      Equ _ _ -> wrapParens $ formatExpr expr
      NEq _ _ -> wrapParens $ formatExpr expr
      LTh _ _ -> wrapParens $ formatExpr expr
      LEq _ _ -> wrapParens $ formatExpr expr
      GTh _ _ -> wrapParens $ formatExpr expr
      GEq _ _ -> wrapParens $ formatExpr expr
      And _ _ -> wrapParens $ formatExpr expr
      Or  _ _ -> wrapParens $ formatExpr expr
      _       -> formatExpr expr -- but not other expressions

-- helper functions

-- helper function to add 4 spaces to the start of a line
indent :: String -> String
indent
  = ("    "++)

sepByComma :: [String] -> String
sepByComma
  = intercalate ", "

wrapBrackets :: String -> String
wrapBrackets s
  = '[' : s ++ "]"

wrapParens :: String -> String
wrapParens s
  = '(' : s ++ ")"
