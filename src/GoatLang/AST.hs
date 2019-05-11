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

class ASTNode node

-- The root of a Goat AST is of type GoatProgram. It holds a list of procedures.
data GoatProgram
  = GoatProgram [Proc]
    deriving (Show, Eq)
instance ASTNode GoatProgram

-- A procedure is referenced by an identifier, takes a (possibly empty)
-- list of parameters, contains a (possibly empty) list of local variable
-- declarations and contains a body of statements.
data Proc
  = Proc Id [Param] [Decl] [Stmt]
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
    deriving (Show, Eq)
instance ASTNode Id

-- A parameter must be of a type contained in the BaseType data type (either
-- BoolType, FloatType or IntType). It is passed to a procedure by either value
-- or reference and assigned an identifier.
data Param
  = Param PassBy BaseType Id
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
  = Decl BaseType Id Dim
    deriving (Show, Eq)
instance ASTNode Decl

-- A dimensionality indicator has a constructor of the form DimN (N Ints)
-- representing the 'shape' of the variable---or collection or variables---being
-- declared.
data Dim
  = Dim0             -- a single variable
  | Dim1 Int         -- an array of variables, with integer 'length'
  | Dim2 Int Int     -- a matrix of variables, with integer 'length' and 'width'
    deriving (Show, Eq)
instance ASTNode Dim

-- Statements can take 6 different forms, as indicated below.
data Stmt
  = Asg Var Expr                -- assignment of an expression to a variable
  | Read Var                    -- assignment of user input to a variable
  | Write Expr                  -- printing of the result of an expression
  | Call Id [Expr]              -- invocation of a procedure
  | If Expr [Stmt]              -- conditional statement (without alternative)
  | IfElse Expr [Stmt] [Stmt]   -- conditional statement (with alternative)
  | While Expr [Stmt]           -- conditional loop
    deriving (Show, Eq)
instance ASTNode Stmt

-- A variable (:: Var) is distinct from an identifier (:: Id). A variable is
-- an identifier used in conjunction with 0, 1 or 2 expressions (depending on
-- dimensionality) to denote a specific memory location which can hold a value.
-- The Var type is analagous to the mathematical notion of a (possibly)
-- subscripted 'variable' (e.g. y, x_1, or A_ij) whereas the Id type is simply
-- a name given to a singular variable (e.g. y), array of variables (e.g. x,
-- the vector) or matrix of variables (e.g. A, the matrix), or to a procedure.
data Var
  = Var0 Id              -- a 'direct identifier' (no subscript necessary)
  | Var1 Id Expr         -- an array element (identifier plus one subscript)
  | Var2 Id Expr Expr    -- a matrix element (identifier plus two subscripts)
    deriving (Show, Eq)
instance ASTNode Var

-- Expressions can take 7 different forms, as indicated below.
data Expr
  = VarExpr Var               -- variable
  | BoolConst Bool            -- boolean constant
  | FloatConst Float          -- floating point constant
  | IntConst Int              -- integer constant
  | StrConst String           -- string constant (only be used for writing)
  | BinExpr BinOp Expr Expr   -- binary expression
  | UnExpr UnOp Expr          -- unary expression
    deriving (Show, Eq)
instance ASTNode Expr

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
