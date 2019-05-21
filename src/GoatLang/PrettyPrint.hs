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

import Control.Monad (mapM_)
import Numeric (showFFloatAlt) -- for formatting floats; see LMS

-- Our custom CodeWriter utility module provides us a monadic interface for
-- efficiently constructing syntax-highlighted strings (using a specialised
-- State Monad powered by difference lists and ANSI escape sequences).
--
import Util.CodeWriter
--
-- With this, we can prettify a Goat Program using Monadic style. Yay!
-- You can think of it as kind of like an opposite of Parsec---Complete
-- with some helpful code writers and code writer combinators defined for
-- us by Util.CodeWriter.

import GoatLang.AST


--
-- Okay, let's get to it!                              (∩ᄑ_ᄑ)⊃━･`ﾟ*･｡*･☆



-- ----------------------------------------------------------------------------
-- Top-level functions to write a whole Goat Program to stdout
-- ----------------------------------------------------------------------------

-- printGoatProgram
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a string and print it directly to stdout.
printGoatProgram :: GoatProgram -> IO ()
printGoatProgram prog
  = putStr $ prettify prog
  -- = putStr $ writeCode $ writeGoatProgram prog

-- printGoatProgramColoured
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a syntax-highlighted string and print it directly to stdout.
printGoatProgramColoured :: ColourScheme -> GoatProgram -> IO ()
printGoatProgramColoured cs prog
  = putStr $ prettifyColoured cs prog
  -- = putStr $ writeCodeColoured cs $ writeGoatProgram prog


-- ----------------------------------------------------------------------------
-- 'Pretty' class allows us to convert arbitrary slices of an AST to Strings
-- ----------------------------------------------------------------------------

class Pretty a where
  -- writer
  -- Backend function to turn an ast node into a CodeWriter (). The other
  -- functions are expressed in terms of this function, so that it's the
  -- minimum required for a complete program definition
  writer :: a -> CodeWriter ()

  -- prettifyColoured
  -- Transform part of an Abstract Syntax Tree into a Syntax-highlighted String
  prettifyColoured :: ColourScheme -> a -> String
  prettifyColoured cs node
    = writeCodeColoured cs $ writer node

  -- prettify
  -- Like prettifyColoured, but without syntax highlighting
  prettify :: a -> String
  prettify node
    = writeCode $ writer node

-- Hook up the relevant writer for various slices of AST:
instance Pretty GoatProgram where
  writer = writeGoatProgram
instance Pretty Proc where
  writer = writeProc
instance Pretty Param where
  writer = writeParam
instance Pretty Decl where
  writer = writeDecl
instance Pretty Stmt where
  writer = writeStmt
instance Pretty Expr where
  writer = writeExpr


-- ----------------------------------------------------------------------------
-- CodeWriters for the different elements of a program
-- ----------------------------------------------------------------------------

-- writeGoatProgram
-- Create an action for building a String representing an entire
-- Goat Program.
writeGoatProgram :: GoatProgram -> CodeWriter ()
writeGoatProgram (GoatProgram procs)
  = sepBy newline $ map writeProc procs
  -- NOTE: this will add a second newline between each proc, except at eof
  -- (each procedure already comes with a newline after the "end" keyword).


-- writeProc
-- Create an action for building a String representing a procedure
writeProc :: Proc -> CodeWriter ()
writeProc (Proc (Id name) params decls stmts _)
  = do
      -- first write a line with the keyword and the procedure header
      line $ writeKeyword "proc" >> space >> writeIdent name >> space >>
        parens (commaSep (map writeParam params))

      -- then proceed to write lines for each decl (indented 1 level)
      withIncreasedIndentation softTab $ mapM_ writeDecl decls

      -- 'begin' separates decls from statements
      line $ writeKeyword "begin"

      -- statements are also indented one level
      withIncreasedIndentation softTab $ mapM_ writeStmt stmts

      -- finish with the "end" keyword on its own line
      line $ writeKeyword "end"

--                          Aren't Monads Awesome?                ___φʕ ^ᴥ^ ʔ

-- softTab
-- Helper-action to write a single level of indentation
softTab :: CodeWriter ()
softTab
  = space >> space >> space >> space


-- writeParam
-- Create an action to build a parameter specification as a string
writeParam :: Param -> CodeWriter ()
writeParam (Param passBy baseType (Id name) _)
  = do
      writeKeyword (format passBy)
      space
      writeKeyword (format baseType)
      space
      writeIdent name


-- writeDecl
-- Create an action for building a declaration as a string
writeDecl :: Decl -> CodeWriter ()
writeDecl (Decl baseType (Id name) dim _)
  = semiLine $ writeKeyword (format baseType) >> space >> writeIdent name >>
      writeDim dim


-- writeDim
-- Create an action to build a shape/dimensionality indicator as a string
writeDim :: Dim -> CodeWriter ()
writeDim Dim0
  = return ()
writeDim (Dim1 n)
  = brackets $ writeIntLit n
writeDim (Dim2 n m)
  = brackets $ commaSep $ map writeIntLit [n, m]


-- writeStmt
-- Create an action for building a statement (atomic or composite) as a string.
writeStmt :: Stmt -> CodeWriter ()

writeStmt (Asg scalar expr _)
  = semiLine $ writeScalar scalar >> spaces (write ":=") >> writeExpr expr

writeStmt (Read scalar _)
  = semiLine $ writeKeyword "read" >> space >> writeScalar scalar

writeStmt (WriteExpr expr _)
  = semiLine $ writeKeyword "write" >> space >> writeExpr expr

writeStmt (WriteString str _)
  = semiLine $ writeKeyword "write" >> space >> asString (writeStringLit str)

writeStmt (Call (Id name) args _)
  = semiLine $ writeKeyword "call" >> space >> writeIdent name >>
      parens (commaSep (map writeExpr args))

-- For composite statements, we will have to write some lines at the current
-- level of indentation, and also some statements at the next level of
-- indentation (using nextLevelIndentation = indentation >> softTab).
writeStmt (If cond thenStmts _)
  = do
      line $ writeKeyword "if" >> spaces (writeExpr cond) >> writeKeyword "then"
      withIncreasedIndentation softTab $ mapM_ writeStmt thenStmts
      line $ writeKeyword "fi"

writeStmt (IfElse cond thenStmts elseStmts _)
  = do
      line $ writeKeyword "if" >> spaces (writeExpr cond) >> writeKeyword "then"
      withIncreasedIndentation softTab $ mapM_ writeStmt thenStmts
      line $ writeKeyword "else"
      withIncreasedIndentation softTab $ mapM_ writeStmt elseStmts
      line $ writeKeyword "fi"

writeStmt (While cond doStmts _)
  = do
      line $ writeKeyword "while" >> spaces (writeExpr cond) >> writeKeyword "do"
      withIncreasedIndentation softTab $ mapM_ writeStmt doStmts
      line $ writeKeyword "od"


-- writeScalar
-- Create an action to represent a scalar (variable element) as a String
writeScalar :: Scalar -> CodeWriter ()
writeScalar (Single (Id name) _)
  = writeIdent name
writeScalar (Array (Id name) index _)
  = do
      writeIdent name
      brackets $ writeExpr index
writeScalar (Matrix (Id name) index1 index2 _)
  = do
      writeIdent name
      brackets $ commaSep (map writeExpr [index1, index2])



-- writeExpr
-- Create an action to represent an expression as a String
writeExpr :: Expr -> CodeWriter ()

-- For simple expressions, we can construct the action directly:
writeExpr (BoolConst bool)
  = writeBoolLit bool
writeExpr (IntConst int)
  = writeIntLit int
writeExpr (FloatConst float)
  = writeFloatLit float
writeExpr (ScalarExpr scalar)
  = writeScalar scalar

-- But for complex expressions, the string may also involve parenthesed
-- subexpressions (if they are binary expressions themselves).
-- We use `writeExprParens' to detect this and add parens if necessary:
writeExpr (BinExpr op lExpr rExpr)
  = writeExprParens lExpr >> spaces (write (format op)) >> writeExprParens rExpr
writeExpr (UnExpr op expr)
  = write (format op) >> writeExprParens expr

-- writeExprParens
-- Create an action to create a string for an expression, enclosed in
-- parentheses if it represents a binary operation expression (or not
-- otherwise)
writeExprParens :: Expr -> CodeWriter ()
writeExprParens expr@(BinExpr _ _ _)
  = parens $ writeExpr expr
writeExprParens expr
  = writeExpr expr


-- ----------------------------------------------------------------------------
-- CodeWriters for formatting literals (bool, int, float, str) in Goat format
-- ----------------------------------------------------------------------------


-- writeBoolLit
-- Create an action to represent a bool in the required format (which is
-- to be lower-case 'true' or 'false'.
writeBoolLit :: Bool -> CodeWriter ()
writeBoolLit True
  = writeKeyword "true"
writeBoolLit False
  = writeKeyword "false"


-- writeIntLit
-- Create an action to represent an int in the required format (which is just
-- the default 'show' format).
writeIntLit :: Int -> CodeWriter ()
writeIntLit int
  = asNumber $ showWrite int


-- writeFloatLit
-- Create an action to represent a float in decimal notation (always with '.'
-- and never in exponential notation). See LMS Discussion Board.
writeFloatLit :: Float -> CodeWriter ()
writeFloatLit float
  = asNumber $ write $ showFFloatAlt Nothing float ""


-- writeStringLit
-- Create an action to represent a string literal as a string.
-- Note: we have to 'unparse' the string for representation, or it will contain
-- actual newlines etc.
writeStringLit :: String -> CodeWriter ()
writeStringLit str
  = asString $ quote $ mapM_ writeCharEsc str

-- writeCharEsc
-- Write a single character, taking care to 'unparse' escaped characters back
-- into escape sequences (namely `\n` --> `\` followed by `n`).
writeCharEsc :: Char -> CodeWriter ()
writeCharEsc '\n'
    -- a `\` (slash) followed by `n`.
    = write "\\" >> write "n"
writeCharEsc c
    -- just the character itself (as a string)
    = write (c:"")



-- ----------------------------------------------------------------------------
-- The remaining code deals with converting simpler structures to
-- to Strings directly, in non-monadic style. We create a new typeclass
-- `Displayable' providing a function `format' for converting its members'
-- values to strings.
--
-- NOTE: We could have overridden the implementation of `show' for these types
-- but this way we are still able to keep the default behaviour or `show'
-- (which is useful to enable viewing ASTs such as for debugging this module).
-- ----------------------------------------------------------------------------

-- A Display type implements the format function, for converting
-- values to strings for pretty-printing.
class Display displayable where
  format :: displayable -> String

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
