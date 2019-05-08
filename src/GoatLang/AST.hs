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

-- The root of a Goat AST is of type GoatProgram. It holds a list of procedures.
data GoatProgram
  = GoatProgram [Proc]
    deriving (Show, Eq)

-- A procedure is referenced by an identifier, takes a (possibly empty)
-- list of parameters, contains a (possibly empty) list of local variable
-- declarations and contains a body of statements.
data Proc
  = Proc Id [Param] [Decl] [Stmt]
    deriving (Show, Eq)

-- An identifier is a string which is used to reference -
--   (a) a procedure; or
--   (b) in conjunction with 0, 1 or 2 expressions -
--       (i)  a parameter; or
--       (ii) a local variable.
-- The number of required expressions is governed by dimensionality.
type Id = String

-- A parameter must be of a type contained in the BaseType data type (either
-- BoolType, FloatType or IntType). It is passed to a procedure by either value
-- or reference and assigned an identifier.
data Param
  = Param PassBy BaseType Id
    deriving (Show, Eq)

data PassBy
  = Val | Ref
    deriving (Show, Eq)

data BaseType
  = BoolType | FloatType | IntType
    deriving (Show, Eq)

-- The declaration of a local variable consists of an identifier and a
-- dimensionality indicator. It must be of a type contained in the BaseType
-- data type.
data Decl
  = Decl BaseType Id Dim
    deriving (Show, Eq)

-- A dimensionality indicator has a constructor of the form DimN (N Ints)
-- representing the 'shape' of the variable (single scalar, array of scalars,
-- or matrix of scalars) being declared.
data Dim
  = Dim0             -- a single scalar
  | Dim1 Int         -- an array of scalars, with integer 'length'
  | Dim2 Int Int     -- a matrix of scalars, with integer 'length' and 'width'
    deriving (Show, Eq)

-- Statements can take 7 different forms, as indicated below.
data Stmt
  = Asg Scalar Expr             -- assignment of an expression to a scalar
  | Read Scalar                 -- assignment of user input to a scalar
  | WriteExpr Expr              -- printing of the result of an expression
  | WriteString String          -- printing of a literal string
  | Call Id [Expr]              -- invocation of a procedure
  | If Expr [Stmt]              -- conditional statement (without alternative)
  | IfElse Expr [Stmt] [Stmt]   -- conditional statement (with alternative)
  | While Expr [Stmt]           -- conditional loop
    deriving (Show, Eq)

-- A scalar is an object that contains a single value. This may be the value of
-- a singleton variable, or a single element of an array or matrix variable.
--
-- Thus a scalar (:: Scalar) is distinct from an identifier (:: Id): While an
-- identifier refers to a particular variable, which variable may comprise
-- multiple scalars (if it is a container for multiple values, e.g. an array).
-- Thus a scalar is an identifier in conjunction with 0, 1 or 2 expressions
-- (depending on the identified variable's dimensionality) to denote a specific
-- element within that variable:
data Scalar
  = Single Id              -- a singleton variable's element (no subscript)
  | Array  Id Expr         -- an array element (identifier plus one subscript)
  | Matrix Id Expr Expr    -- a matrix element (identifier plus two subscripts)
    deriving (Show, Eq)

-- Expressions can take 6 different forms, as indicated below.
data Expr
  = ScalarExpr Scalar         -- the value inside a scalar (variable element)
  | BoolConst Bool            -- boolean constant
  | FloatConst Float          -- floating point constant
  | IntConst Int              -- integer constant
  | BinExpr BinOp Expr Expr   -- binary expression
  | UnExpr UnOp Expr          -- unary expression
    deriving (Show, Eq)

-- Binary operators
data BinOp
 = Add | Sub | Mul | Div              -- arithmetic
 | Equ | NEq | LTh | LEq | GTh | GEq  -- relational
 | And | Or                           -- boolean
  deriving (Show, Eq)

-- Unary operators
data UnOp
  = Neg   -- mathematical (not boolean) negation
  | Not   -- boolean complement
    deriving (Show, Eq)
