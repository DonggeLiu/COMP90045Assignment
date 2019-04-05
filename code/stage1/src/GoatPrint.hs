module GoatPrint where

-- ----------------------------------------------------------------------------
-- DIRECT IO-BASED GOAT PRINTING ALGORITHM
-- DEPRECATED IN FAVOUR OF MONADIC STRING-BUILDER APPROACH
-- ----------------------------------------------------------------------------

import Data.List (intercalate)
import Control.Monad (forM_, mapM_)

import GoatAST

prettyPrintGP :: GoatProgram -> IO ()
prettyPrintGP (GoatProgram (proc:procs))
  = do
      prettyPrintProc proc
      forM_ procs (\proc -> do
          newline
          prettyPrintProc proc
        )

prettyPrintProc :: Proc -> IO ()
prettyPrintProc (Proc (Id name) params decls stmts)
  = do
      let putLine  = putStrLn
      let putLine' = putLine.indent
      putLine $ "proc "++name++" ("++sepByComma (map formatParam params)++")"
      forM_ decls $ prettyPrintDecl putLine'
      putLine "begin"
      forM_ stmts $ prettyPrintStmt putLine'
      putLine "end"

prettyPrintDecl :: (String -> IO ()) -> Decl -> IO ()
prettyPrintDecl putLine (Decl baseType (Id name) dim)
  = putLine $ formatType baseType++" "++name++formatDim dim++";"

prettyPrintStmt :: (String -> IO ()) -> Stmt -> IO ()
-- this may involve printing a single line with the current
-- level of indentation
prettyPrintStmt putLine (Asg var expr)
  = putLine $ formatVar var ++ " := " ++ formatExpr expr ++ ";"

prettyPrintStmt putLine (Read var)
  = putLine $ "read "++formatVar var++";"

prettyPrintStmt putLine (Write expr)
  = putLine $ "write "++formatExpr expr++";"

prettyPrintStmt putLine (Call (Id name) exprs)
  = putLine $ "call "++name++"("++sepByComma (map formatExpr exprs)++");"

-- or, this may involve recursively formatting and indenting
-- lists of statements within the bodies of composite statements:
prettyPrintStmt putLine (If expr stmts)
  = do
      let putLine' = putLine.indent
      putLine $ "if "++formatExpr expr++" then"
      forM_ stmts $ prettyPrintStmt putLine'
      putLine $ "fi"

prettyPrintStmt putLine (IfElse expr stmts1 stmts2)
  = do
      let putLine' = putLine.indent
      putLine $ "if " ++ formatExpr expr ++ " then"
      forM_ stmts1 $ prettyPrintStmt putLine'
      putLine "else"
      forM_ stmts2 $ prettyPrintStmt putLine'
      putLine "fi"

prettyPrintStmt putLine (While expr stmts)
  = do
      let putLine' = putLine.indent
      putLine $ "while " ++ formatExpr expr ++ " do"
      forM_ stmts $ prettyPrintStmt putLine'
      putLine "od"



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


newline :: IO ()
newline = putStr "\n"

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
