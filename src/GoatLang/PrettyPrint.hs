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


-- printGoatProgram
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a string and print it directly to stdout.
printGoatProgram :: GoatProgram -> IO ()
printGoatProgram gp
  = printGoatProgramColoured (getColourSchemeByName NoColours) gp
printGoatProgramColoured :: ColourScheme -> GoatProgram -> IO ()
printGoatProgramColoured cs gp
  = putStr $ prettify cs gp

-- prettify
-- Top-level function to transform a GoatProgram (an Abstract Syntax Tree) into
-- a String, using an efficient String Builder approach.
-- NOTE: The result includes a trailing newline! If printing, just use
-- putStr rather than putStrLn. Or just use `printProgram'.
prettify :: ColourScheme -> GoatProgram -> String
prettify cs gp
  = writeCodeColoured cs $ writeGoatProgram gp


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
writeProc (Proc (Id name) params decls stmts)
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
writeParam (Param passBy baseType (Id name))
  = do
      writeKeyword (format passBy)
      space
      writeKeyword (format baseType)
      space
      writeIdent name


-- writeDecl
-- Create an action for building a declaration as a string
writeDecl :: Decl -> CodeWriter ()
writeDecl (Decl baseType (Id name) dim)
  = semiLine $ writeKeyword (format baseType) >> space >> writeIdent name >> 
      writeDim dim


-- writeDim
-- Create an action to build a shape/dimensionality indicator as a string
writeDim :: Dim -> CodeWriter ()
writeDim Dim0
  = return ()
writeDim (Dim1 n)
  = brackets $ writeInt n
writeDim (Dim2 n m)
  = brackets $ commaSep $ map writeInt [n, m]


-- writeStmt
-- Create an action for building a statement (atomic or composite) as a string.
writeStmt :: Stmt -> CodeWriter ()

writeStmt (Asg scalar expr)
  = semiLine $ writeScalar scalar >> spaces (write ":=") >> writeExpr expr

writeStmt (Read scalar)
  = semiLine $ writeKeyword "read" >> space >> writeScalar scalar

writeStmt (WriteExpr expr)
  = semiLine $ writeKeyword "write" >> space >> writeExpr expr

writeStmt (WriteString str)
  = semiLine $ writeKeyword "write" >> space >> asString (writeStrLit str)

writeStmt (Call (Id name) args)
  = semiLine $ writeKeyword "call" >> space >> writeIdent name >>
      parens (commaSep (map writeExpr args))

-- For composite statements, we will have to write some lines at the current
-- level of indentation, and also some statements at the next level of
-- indentation (using nextLevelIndentation = indentation >> softTab).
writeStmt (If cond thenStmts)
  = do
      line $ writeKeyword "if" >> spaces (writeExpr cond) >> writeKeyword "then"
      withIncreasedIndentation softTab $ mapM_ writeStmt thenStmts
      line $ writeKeyword "fi"

writeStmt (IfElse cond thenStmts elseStmts)
  = do
      line $ writeKeyword "if" >> spaces (writeExpr cond) >> writeKeyword "then"
      withIncreasedIndentation softTab $ mapM_ writeStmt thenStmts
      line $ writeKeyword "else"
      withIncreasedIndentation softTab $ mapM_ writeStmt elseStmts
      line $ writeKeyword "fi"

writeStmt (While cond doStmts)
  = do
      line $ writeKeyword "while" >> spaces (writeExpr cond) >> writeKeyword "do"
      withIncreasedIndentation softTab $ mapM_ writeStmt doStmts
      line $ writeKeyword "od"


-- writeScalar
-- Create an action to represent a scalar (variable element) as a String
writeScalar :: Scalar -> CodeWriter ()
writeScalar (Single (Id name))
  = writeIdent name
writeScalar (Array (Id name) index)
  = do
      writeIdent name
      brackets $ writeExpr index
writeScalar (Matrix (Id name) index1 index2)
  = do
      writeIdent name
      brackets $ commaSep (map writeExpr [index1, index2])



-- writeExpr
-- Create an action to represent an expression as a String
writeExpr :: Expr -> CodeWriter ()

-- For simple expressions, we can construct the action directly:
writeExpr (BoolConst bool)
  = writeBool bool
writeExpr (IntConst int)
  = writeInt int
writeExpr (FloatConst float)
  = writeFloat float
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


-- writeBool
-- Create an action to represent a bool in the required format (which is
-- to be lower-case 'true' or 'false'.
writeBool :: Bool -> CodeWriter ()
writeBool True
  = writeKeyword "true"
writeBool False
  = writeKeyword "false"


-- writeInt
-- Create an action to represent an int in the required format (which is just
-- the default 'show' format).
writeInt :: Int -> CodeWriter ()
writeInt int
  = asNumber $ showWrite int


-- writeFloat
-- Create an action to represent a float in decimal notation (always with '.'
-- and never in exponential notation). See LMS Discussion Board.
writeFloat :: Float -> CodeWriter ()
writeFloat float
  = asNumber $ write $ showFFloatAlt Nothing float ""


-- writeStrLit
-- Create an action to represent a string literal as a string.
-- Note: we have to 'unparse' the string for representation, or it will contain
-- actual newlines etc.
writeStrLit :: String -> CodeWriter ()
writeStrLit str
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


-- Represent a string literal character as a string
instance Display Char where
  -- translate newline back into \ and n combo:
  format '\n'
    = "\\n"
  -- all other characters are just printed as singleton strings
  format c
    = c:""
