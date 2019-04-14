module GoatLang.PrettyPrint where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - PRETTY-PRINTING MODULE
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
import Numeric (showFFloatAlt) -- see LMS

import GoatLang.AST


-- Our custom StringBuilder utility module provides us a monadic interface for
-- efficiently constructing strings (using a specialised Writer Monad powered
-- by difference lists).
--
import Util.StringBuilder
--
-- With this, we can prettify a Goat Program using Monadic style. Yay!
-- You can think of it as kind of like an opposite of Parsec---Complete
-- with some helpful string builders and string builder combinators defined
-- for us by that file (imported above, plus some Goat-specific ones defined
-- below)
--
-- Okay, let's get to it!                              (∩ᄑ_ᄑ)⊃━･`ﾟ*･｡*･☆



-- printProgram
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a string and print it directly to stdout.
printProgram :: GoatProgram -> IO ()
printProgram gp
  = putStr $ prettify gp


-- prettify
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a String, using an efficient String Builder approach.
-- NOTE: The result includes a trailing newline! If printing, just use
-- putStr rather than putStrLn. Or just use `printProgram'.
prettify :: GoatProgram -> String
prettify gp
  = buildString $ writeGoatProgram gp



-- ----------------------------------------------------------------------------
-- StringBuilders for the different elements of a program
-- ----------------------------------------------------------------------------

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
      parens (commaSep (map writeF params)) >> newline

      -- then proceed to write lines for each decl and stmt (indented 1 level)
      let levelOneIndentation = softTab
      mapM (writeDeclWith levelOneIndentation) decls
      writeLn "begin"
      mapM (writeStmtWith levelOneIndentation) stmts

      -- finish with the "end" keyword on its own line
      writeLn "end"

--                          Aren't Monads Awesome?                ___φʕ ^ᴥ^ ʔ



-- softTab
-- Helper-action to write a single level of indentation
softTab :: StringBuilder
softTab
  = space >> space >> space >> space

-- endLine
-- Helper action to add a semicolon AND terminate the line
endLine :: StringBuilder
endLine
  = semi >> newline



-- writeDeclWith
-- Create an action for building a declaration as a string, using a
-- provided action for indenting each line
-- (actually, there is only one line in this case)
writeDeclWith :: StringBuilder -> Decl -> StringBuilder
writeDeclWith indentn (Decl baseType name dim)
  = indentn >> writeF baseType >> space >> write name >> writeF dim >> endLine


-- writeStmtWith
-- Create an action for building a statement (atomic or composite) as
-- a string, using a provided action for indenting each line.
writeStmtWith :: StringBuilder -> Stmt -> StringBuilder

-- For atomic statements, building will involve writing a single line
-- with the current level of indentation
writeStmtWith indentation (Asg var expr)
  = do
      indentation >> writeVar var >> spaces (write ":=")
      writeExpr expr >> endLine

writeStmtWith indentation (Read var)
  = indentation >> write "read" >> space >> writeVar var >> endLine

writeStmtWith indentation (Write expr)
  = indentation >> write "write" >> space >> writeExpr expr >> endLine

writeStmtWith indentation (Call name args)
  = do
      indentation >> write "call" >> space >> write name
      parens (commaSep (map writeExpr args)) >> endLine

-- For composite statements, we will have to write some lines at the current
-- level of indentation, and also some statements at the next level of 
-- indentation (using nextLevelIndentation = indentation >> softTab).
writeStmtWith indentation (If cond thenStmts)
  = do
      let nextLevelIndentation = indentation >> softTab
      indentation >> write "if" >> spaces (writeExpr cond) >> writeLn "then"
      mapM (writeStmtWith nextLevelIndentation) thenStmts
      indentation >> writeLn "fi"

writeStmtWith indentation (IfElse cond thenStmts elseStmts)
  = do
      let nextLevelIndentation = indentation >> softTab
      indentation >> write "if" >> spaces (writeExpr cond) >> writeLn "then"
      mapM (writeStmtWith nextLevelIndentation) thenStmts
      indentation >> writeLn "else"
      mapM (writeStmtWith nextLevelIndentation) elseStmts
      indentation >> writeLn "fi"

writeStmtWith indentation (While cond doStmts)
  = do
      let nextLevelIndentation = indentation >> softTab
      indentation >> write "while" >> spaces (writeExpr cond) >> writeLn "do"
      mapM (writeStmtWith nextLevelIndentation) doStmts
      indentation >> writeLn "od"


-- writeVar
-- Create an action to represent a variable as a String
writeVar :: Var -> StringBuilder
writeVar (Var0 name)
  = write name
writeVar (Var1 name index)
  = write name >> brackets (writeExpr index)
writeVar (Var2 name index1 index2)
  = write name >> brackets (commaSep (map writeExpr [index1, index2]))


-- writeExpr
-- Create an action to represent an expression as a String
writeExpr :: Expr -> StringBuilder

-- For simple expressions, we can construct the action directly:
writeExpr (BoolConst True)
  = write "true"
writeExpr (BoolConst False)
  = write "false"
writeExpr (IntConst int)
  -- default `show` behaves fine for integers
  = write $ show int
writeExpr (FloatConst float)
  -- but we always need to show floats without exponentials and with `.`
  = write $ showFFloatAlt Nothing float ""
writeExpr (StrConst str)
  -- we must also 'un-parse' string literals incl. recreating newline combos
  = quote $ mapM_ writeF str
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
-- The remaining code deals with converting simpler structures to
-- to Strings directly, in non-monadic style. We create a new typeclass
-- `Displayable' providing a function `format' for converting its members'
-- values to strings, and ONE general function `writeF' for writing something
-- of this type class to our StringBuilder.
--
-- NOTE: We could have overridden the implementation of `show' for these types
-- but this way we are still able to keep the default behaviour or `show'
-- (which is useful to enable viewing ASTs such as for debugging this module).
-- ----------------------------------------------------------------------------

-- writeF
-- Shortcut for action to write a displayable as a string using the format
-- function (see below for Display type-class definitions)
writeF :: (Display a) => a -> StringBuilder
writeF d
  = write $ format d


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
    = wrapBrackets $ show n ++ ", " ++ show m

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

-- Represent a string literal character as a string
instance Display Char where
  -- translate newline back into \ and n combo:
  format '\n'
    = "\\n"
  -- all other characters are just printed as singleton strings
  format c
    = c:""