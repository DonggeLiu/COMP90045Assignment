module GoatLang.AST where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - ABSTRACT SYNTAX TREE
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

import Text.Parsec.Pos

class ASTNode node

-- The position in the Goat source file.
-- Type provided by Parsec - contains the SourceName, Line and Column.
type Pos = SourcePos

-- The root of a Goat AST is of type GoatProgram. It holds a list of procedures.
data GoatProgram
  = GoatProgram [Proc]
    deriving (Show, Eq)
instance ASTNode GoatProgram

-- A procedure is referenced by an identifier, takes a (possibly empty)
-- list of parameters, contains a (possibly empty) list of local variable
-- declarations and contains a body of statements.
data Proc
  = Proc Id [Param] [Decl] [Stmt] Pos
    deriving (Show, Eq)
instance ASTNode Proc

-- An identifier is a string which is used to reference -
--   (a) a procedure; or
--   (b) in conjunction with 0, 1 or 2 expressions -
--       (i)  a parameter; or
--       (ii) a local variable.
-- The number of required expressions is governed by dimensionality.
data Id
  = Id String
    deriving (Show, Eq, Ord)
instance ASTNode Id

-- A parameter must be of a type contained in the BaseType data type (either
-- BoolType, FloatType or IntType). It is passed to a procedure by either value
-- or reference and assigned an identifier.
data Param
  = Param PassBy BaseType Id Pos
    deriving (Show, Eq)
instance ASTNode Param

data PassBy
  = Val | Ref
    deriving (Show, Eq)
instance ASTNode PassBy

data BaseType
  = BoolType | FloatType | IntType
    deriving (Show, Eq)
instance ASTNode BaseType

-- The declaration of a local variable consists of an identifier and a
-- dimensionality indicator. It must be of a type contained in the BaseType
-- data type.
data Decl
  = Decl BaseType Id Dim Pos
    deriving (Show, Eq)
instance ASTNode Decl

-- A dimensionality indicator has a constructor of the form DimN (N Ints)
-- representing the 'shape' of the variable (single scalar, array of scalars,
-- or matrix of scalars) being declared.
data Dim
  = Dim0             -- a single scalar
  | Dim1 Int         -- an array of scalars, with integer 'length'
  | Dim2 Int Int     -- a matrix of scalars, with integer 'length' and 'width'
    deriving (Show, Eq)
instance ASTNode Dim

-- Statements can take 7 different forms, as indicated below.
data Stmt
  = Asg Scalar Expr Pos           -- assignment of an expression to a scalar
  | Read Scalar Pos               -- assignment of user input to a scalar
  | WriteExpr Expr Pos            -- printing of the result of an expression
  | WriteString String Pos        -- printing of a literal string
  | Call Id [Expr] Pos            -- invocation of a procedure
  | If Expr [Stmt] Pos            -- conditional statement (without alternative)
  | IfElse Expr [Stmt] [Stmt] Pos -- conditional statement (with alternative)
  | While Expr [Stmt] Pos         -- conditional loop
    deriving (Show, Eq)
instance ASTNode Stmt

-- A scalar is an object that contains a single value. This may be the value of
-- a singleton variable, or a single element of an array or matrix variable.
--
-- A scalar (:: Scalar) is distinct from an identifier (:: Id): An identifier
-- refers to a particular variable, which may comprise multiple scalars (if it
-- is a container for multiple values, e.g. an array). A scalar is an identifier
-- in conjunction with 0, 1 or 2 expressions (depending on the identified
-- variable's dimensionality) to denote a specific element within that variable:
data Scalar
  = Single Id Pos           -- a singleton variable's element (no subscript)
  | Array  Id Expr Pos      -- an array element (identifier plus one subscript)
  | Matrix Id Expr Expr Pos -- a matrix element (identifier plus two subscripts)
    deriving (Show, Eq)
instance ASTNode Scalar

-- scalarExpr
-- Helper function to provide convenient access to a scalar's identifier
scalarIdent :: Scalar -> Id
scalarIdent (Single ident _)
  = ident
scalarIdent (Array ident _ _)
  = ident
scalarIdent (Matrix ident _ _ _)
  = ident

-- Expressions can take 6 different forms, as indicated below.
data Expr
  = ScalarExpr Scalar         -- the value inside a scalar (variable element)
  | BoolConst Bool            -- boolean constant
  | FloatConst Float          -- floating point constant
  | IntConst Int              -- integer constant
  | BinExpr BinOp Expr Expr   -- binary expression
  | UnExpr UnOp Expr          -- unary expression
    deriving (Show, Eq)
instance ASTNode Expr

-- data ExprAttr = ExprAttr { lineNum :: Int
--                          , value :: a
--                          , id :: String
--                          } deriving (Show)

-- We're thinking of going with THIS ONE:

-- -- ABinExpr attr Add (AIntConst attr 1) (AIntConst attr 2)  -- AExpr -> 105-112          <- Chosen for now
-- data AExpr
--   = AScalarExpr ExprAttr Scalar         -- the value inside a scalar (variable element)
--   | ABoolConst ExprAttr Bool            -- boolean constant
--   | AFloatConst ExprAttr Float          -- floating point constant
--   | AIntConst ExprAttr Int              -- integer constant
--   | ABinExpr ExprAttr BinOp AExpr AExpr -- binary expression
--   | AUnExpr ExprAttr UnOp AExpr         -- unary expression
--     deriving (Show, Eq)

-- Alternatives:

-- type AExpr = (Expr, Attr)
-- data Expr
--   = ScalarExpr Scalar         -- the value inside a scalar (variable element)
--   | BoolConst Bool            -- boolean constant
--   | FloatConst Float          -- floating point constant
--   | IntConst Int              -- integer constant
--   | BinExpr BinOp AExpr AExpr   -- binary expression
--   | UnExpr UnOp AExpr          -- unary expression
--     deriving (Show, Eq)
--
-- data AExpr = Node Attr Expr


-- (BinExpr Add (IntConst 1, attr) (IntConst 2, attr), attr)
-- (attr, BinExpr Add (attr, IntConst 1) (attr, IntConst 2))
-- ABinExpr attr Add (AIntConst attr 1) (AIntConst attr 2)  -- AExpr -> 105-112          <- Chosen for now
-- Node attr (BinExpr Add (Node attr (IntConst 1)) (Node attr (IntConst 2)))



-- Binary operators
data BinOp
 = Add | Sub | Mul | Div              -- arithmetic
 | Equ | NEq | LTh | LEq | GTh | GEq  -- relational
 | And | Or                           -- boolean
  deriving (Show, Eq)
instance ASTNode BinOp

-- Unary operators
data UnOp
  = Neg   -- mathematical (not boolean) negation
  | Not   -- boolean complement
    deriving (Show, Eq)
instance ASTNode UnOp
