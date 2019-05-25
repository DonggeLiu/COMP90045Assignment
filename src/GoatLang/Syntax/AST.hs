module GoatLang.Syntax.AST where

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

-- The position in the Goat source file.
data Pos
  = NoPos
  | Pos { sourceFile :: String
        , lineNumber :: Int
        , colNumber  :: Int
        }
  deriving (Show, Eq)


class ASTNode node
-- TODO: where nodePos :: node -> Pos ?
-- Some of the AST nodes would have to be cut.
-- where
--   pos :: node -> Pos

-- The root of a Goat AST is of type GoatProgram. It holds a list of procedures.
data GoatProgram
  = GoatProgram [Proc]
    deriving (Show, Eq)
instance ASTNode GoatProgram

-- A procedure is referenced by an identifier, takes a (possibly empty)
-- list of parameters, contains a (possibly empty) list of local variable
-- declarations and contains a body of statements.
data Proc
  = Proc Pos Id [Param] [Decl] [Stmt]
    deriving (Show, Eq)
instance ASTNode Proc

-- An identifier is a string which is used to reference -
--   (a) a procedure; or
--   (b) in conjunction with 0, 1 or 2 expressions -
--       (i)  a parameter; or
--       (ii) a local variable.
-- The number of required expressions is governed by dimensionality.
data Id
  = Id Pos String
    deriving (Show)
instance ASTNode Id
instance Eq Id where
  (==) (Id _ a) (Id _ b)
    = a == b
instance Ord Id where
  compare (Id _ a) (Id _ b)
    = compare a b

-- A parameter must be of a type contained in the BaseType data type (either
-- BoolType, FloatType or IntType). It is passed to a procedure by either value
-- or reference and assigned an identifier.
data Param
  = Param Pos PassBy BaseType Id
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
  = Decl Pos BaseType Id Dim
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
  = Asg Pos Scalar Expr           -- assignment of an expression to a scalar
  | Read Pos Scalar               -- assignment of user input to a scalar
  | WriteExpr Pos Expr            -- printing of the result of an expression
  | WriteString Pos String        -- printing of a literal string
  | Call Pos Id [Expr]            -- invocation of a procedure
  | If Pos Expr [Stmt]            -- conditional statement (without alternative)
  | IfElse Pos Expr [Stmt] [Stmt] -- conditional statement (with alternative)
  | While Pos Expr [Stmt]         -- conditional loop
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
  = Single Pos Id           -- a singleton variable's element (no subscript)
  | Array  Pos Id Expr      -- an array element (identifier plus one subscript)
  | Matrix Pos Id Expr Expr -- a matrix element (identifier plus two subscripts)
    deriving (Show, Eq)
instance ASTNode Scalar


-- Expressions can take 6 different forms, as indicated below.
data Expr
  = ScalarExpr Pos Scalar       -- the value inside a scalar (variable element)
  | BoolConst Pos Bool          -- boolean constant
  | FloatConst Pos Float        -- floating point constant
  | IntConst Pos Int            -- integer constant
  | BinExpr Pos BinOp Expr Expr -- binary expression
  | UnExpr Pos UnOp Expr        -- unary expression
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
