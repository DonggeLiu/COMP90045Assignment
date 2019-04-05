module GoatWriter (
  prettify
) where

-- ----------------------------------------------------------------------------
-- NOTE: This module is NOT to be confised with the 2007 Action film starring
-- Nicholas Cage (... and Eva Mendez. Nicholas Cage starred in at least FOUR 
-- films in 2007---Confusing; I know!).
--
--                            This is not Goast Wrider.
--
--                                    This is...
-- 
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT WRITER - PRETTY-PRINTING MODULE
--
-- Well-chosen team name:              pli-dream-team-twentee-nineteen
-- Well-chosen team members:
-- * Alan Ung                          alanu
-- * David Stern                       dibstern
-- * Dongge Liu                        donggel
-- * Mariam Shahid                     mariams
-- * Matthew Farrugia-Roberts          farrugiam
--
-- ----------------------------------------------------------------------------

import Control.Monad (mapM, mapM_)


import GoatAST
import StringBuilder

-- ----------------------------------------------------------------------------
-- Our StringBuilder utility module provides us a monadic interface for 
-- efficiently constructing strings (using a specialised Writer Monad powered
-- by difference lists).
-- 
-- With this, we can prettify a Goat Program using Monadic style. Yay!
-- You can think of it as kind of like an opposite of Parsec---Complete
-- with some helpful string builder combinators defined later in this
-- file.
-- 
-- Okay, let's get to it!                                  (∩ᄑ_ᄑ)⊃━･`ﾟ*･｡*･☆
-- ----------------------------------------------------------------------------

-- prettify
-- Transform a GoatProgram (an Abstract Syntax Tree) into a String.
-- NOTE: The result includes a trailing newline! If printing, just use
-- putStr rather than putStrLn.
prettify :: GoatProgram -> String
prettify gp
  = buildString $ writeGoatProgram gp


-- writeGoatProgram
-- Create an action for building a String representing an entire
-- Goat Program
writeGoatProgram :: GoatProgram -> StringBuilder
writeGoatProgram (GoatProgram procs)
  = sepBy newline (map writeProc procs)


-- writeProc
-- Create an action for building a String representing a procedure
writeProc :: Proc -> StringBuilder
writeProc (Proc name params decls stmts)
  = do
      -- first write a line with the keyword and the procedure header
      write "proc" >> space >> write name >> space
      parens (commas (map writeF params)) >> newline
      
      -- then proceed to write lines for each decl and stmt (indented 1 level)
      mapM (writeDeclWith indent1) decls
      writeLn "begin"
      mapM (writeStmtWith indent1) stmts

      -- finish with the "end" keyword on its own line
      writeLn "end"

-- >                        Aren't Monads awesome?                __φ ʕ ^ᴥ^ ʔ Μ


-- writeDeclWith
-- Create an action for building a declaration as a string, using a
-- provided action for indenting each line (actually, there is only
-- one line in this case)
writeDeclWith :: StringBuilder -> Decl -> StringBuilder
writeDeclWith indent (Decl baseType name dim)
  = indent >> writeF baseType >> space >> write name >> writeF dim >> endline


-- writeStmtWith
-- Create an action for building a statement (atomic or composite) as
-- a string, using a provided action for indenting each line.
writeStmtWith :: StringBuilder -> Stmt -> StringBuilder

-- For atomic statements, building will involve writing a single line
-- with the current level of indentation (given by indent)
writeStmtWith indent (Asg var expr)
  = indent >> writeVar var >> spaces (write ":=") >> writeExpr expr >> endline

writeStmtWith indent (Read var)
  = indent >> write "read" >> space >> writeVar var >> endline

writeStmtWith indent (Write expr)
  = indent >> write "write" >> space >> writeExpr expr >> endline

writeStmtWith indent (Call name exprs)
  = do
      indent >> write "call" >> space >> write name
      parens (commas (map writeExpr exprs)) >> endline

-- For composite statements, we will have to write some lines at the current 
-- level of indentation (given by indent), and also some statements at the
-- next level of indentation (using indent' = indent1 >> indent).
writeStmtWith indent (If expr stmts)
  = do
      let indent' = indent1 >> indent
      indent >> write "if" >> spaces (writeExpr expr) >> writeLn "then"
      mapM (writeStmtWith indent') stmts
      indent >> writeLn "fi"
writeStmtWith indent (IfElse expr stmts1 stmts2)
  = do
      let indent' = indent1 >> indent
      indent >> write "if" >> spaces (writeExpr expr) >> writeLn "then"
      mapM (writeStmtWith indent') stmts1
      indent >> writeLn "else"
      mapM (writeStmtWith indent') stmts2
      indent >> writeLn "fi"

writeStmtWith indent (While expr stmts)
  = do
      let indent' = indent1 >> indent
      indent >> write "while" >> spaces (writeExpr expr) >> writeLn "do"
      mapM (writeStmtWith indent') stmts
      indent >> writeLn "od"


-- writeVar
-- Create an action to represent a variable as a String
writeVar :: Var -> StringBuilder
writeVar (Var0 name)
  = write name
writeVar (Var1 name expr)
  = write name >> brackets (writeExpr expr)
writeVar (Var2 name expr1 expr2)
  = write name >> brackets (commas (map writeExpr [expr1, expr2]))


-- writeExpr
-- Create an action to represent an expression as a String
writeExpr :: Expr -> StringBuilder

-- For simple expressions, we can construct the action directly:
writeExpr (BoolConst True)
  = write "true"
writeExpr (BoolConst False)
  = write "false"
writeExpr (IntConst int)
  = write $ show int
writeExpr (FloatConst float)
  = write $ show float
writeExpr (StrConst str)
  = quote (write str)
writeExpr (VarExpr var)
  = writeVar var

-- But for complex expressions, the string may also involve parenthesed
-- subexpressions (if they are binary expressions themselves).
-- We use `writeExprParens' to detect this and add parens if necessary:
writeExpr (BinExpr op expr1 expr2)
  = writeExprParens expr1 >> spaces (writeF op) >> writeExprParens expr2
writeExpr (UnExpr op expr)
  = writeF op >> writeExprParens expr

-- writeExprParens
-- Create an action to create a string for an expression, enclosed in
-- parentheses if it represents a binary operation expression (or not
-- otherwise)
writeExprParens :: Expr -> StringBuilder
writeExprParens expr@(BinExpr _ _ _)
  = parens $ writeExpr expr
writeExprParens expr
  = writeExpr expr


-- ----------------------------------------------------------------------------
-- Custom StringBuilders and StringBuilder combinators
-- ----------------------------------------------------------------------------

-- Simple helper StringBuilders:

-- space
-- Action to add a single space character
space :: StringBuilder
space
  = write " "

-- indent1
-- Action to add a single level of indentation
indent1 :: StringBuilder
indent1
  = space >> space >> space >> space

-- newline
-- To add a newline character
newline :: StringBuilder
newline
  = write "\n"

-- semi
-- To add a semicolon character
semi :: StringBuilder
semi 
  = write ";"

-- newline
-- To add a semicolon AND terminate the line
endline :: StringBuilder
endline
  = semi >> newline


-- StringBuilder Combinators

-- parens
-- To enclose an action in parentheses
parens :: StringBuilder -> StringBuilder
parens writer
  = write "(" >> writer >> write ")"

-- brackets
-- To enclose an action in square brackets
brackets :: StringBuilder -> StringBuilder
brackets writer
  = write "(" >> writer >> write ")"

-- quote
-- To surround an action in double quotes
quote :: StringBuilder -> StringBuilder
quote writer
  = write "\"" >> writer >> write "\""

-- spaces
-- To surround an action in single spaces
spaces :: StringBuilder -> StringBuilder
spaces writer
  = space >> writer >> space

-- sepBy
-- To intersperse a list of writers with a separating writer
sepBy :: StringBuilder -> [StringBuilder] -> StringBuilder
sepBy _ []
  = return ()
sepBy separator (writer:writers)
  = do
      writer
      mapM_ (separator >>) writers

-- commas
-- To intersperse commas/spaces between a list of writers
-- (reflecting common usage of sepBy)
commas :: [StringBuilder] -> StringBuilder
commas
  = sepBy (write ", ")


-- writeF
-- Shortcut for action to write a displayable as a string using the format 
-- function (see below for Display type-class definitions)
writeF :: (Display a) => a -> StringBuilder
writeF d
  = write $ format d


-- ----------------------------------------------------------------------------
-- The remaining code deals with converting simpler structures to
-- to Strings directly, in non-monadic style. We create a new typeclass
-- `Displayable' providing a function `format' for converting its members'
-- values to strings.
-- 
-- We could have overridden the implementation of `show' for these types
-- but this way we are able to keep the default behaviour or `show' (which
-- is useful to enable viewing ASTs such as for debugging this module).
-- ----------------------------------------------------------------------------

-- A Display type implements the format function, for converting
-- values to strings for pretty-printing.
class Display displayable where
  format :: displayable -> String

-- Represent a formal paramater as a string
instance Display Param where
  format (Param passBy baseType name)
    = format passBy ++ " " ++ format baseType ++ " " ++ name

-- Represent a paramater passing mechanism as a String
instance Display PassBy where
  format Val
    = "val"
  format Ref
    = "ref"

-- Represent a type declaration as a String
instance Display BaseType where
  format BoolType
    = "bool"
  format FloatType
    = "float"
  format IntType
    = "int"

-- Represent a shape/dimensionality indicator as a string
instance Display Dim where
  format (Dim0)
    = ""
  format (Dim1 n)
    = wrapBrackets $ show n
  format (Dim2 n m)
    = wrapBrackets $ show n ++ "," ++ show m

wrapBrackets :: String -> String
wrapBrackets s
  = "[" ++ s ++ "]"

-- Represent a binary operator as a string
instance Display BinOp where
  format Add
    = "+"
  format Sub
    = "-"
  format Mul
    = "*"
  format Div
    = "/"
  format Equ
    = "="
  format NEq
    = "!="
  format LTh
    = "<"
  format LEq
    = "<="
  format GTh
    = ">"
  format GEq
    = ">="
  format And
    = "&&"
  format Or
    = "||"

-- Represent a unary operator as a string
instance Display UnOp where
  format Neg
    = "-"
  format Not
    = "!"