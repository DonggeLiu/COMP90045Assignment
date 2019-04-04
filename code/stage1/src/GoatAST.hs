module GoatAST where

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
data GoatProgram = GoatProgram [Proc]
    deriving (Show, Eq)

-- A procedure is referenced by an identifier, takes a (possibly empty)
-- list of parameters, contains a (possibly empty) list of local variable
-- declarations and contains a body of statements.
data Proc = Proc Id [Param] [Decl] [Stmt]
    deriving (Show, Eq)

-- An identifier is a string which is used to reference -
--   (a) a procedure; or
--   (b) in conjunction with 0, 1 or 2 expressions -
--       (i)  a parameter; or
--       (ii) a local variable.
-- The number of required expressions is governed by dimensionality.
data Id = Id String
    deriving (Show, Eq)

-- A parameter must be of a type contained in the BaseType data type (either
-- BoolType, FloatType or IntType). It is passed to a procedure by either value
-- or reference and assigned an identifier.
data Param = Param PassBy BaseType Id
    deriving (Show, Eq)
data PassBy = Val | Ref
    deriving (Show, Eq)
data BaseType = BoolType | FloatType | IntType
    deriving (Show, Eq)

-- The declaration of a local variable consists of an identifier and a
-- dimensionality indicator. It must be of a type contained in the BaseType
-- data type.
data Decl = Decl BaseType Id Dim
    deriving (Show, Eq)
data Dim = Dim0             -- single variable
         | Dim1 Int         -- array of variables
         | Dim2 Int Int     -- matrix of variables
    deriving (Show, Eq)

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

-- A variable (:: Var) is distinct from an identifier (:: Id). A variable is
-- an identifier used in conjunction with 0, 1 or 2 expressions (depending on
-- dimensionality) to denote a specific memory location which can hold a value.
-- The Var type is analagous to the mathematical notion of a subscripted
-- 'variable', whereas the Id type is simply a name given to a single variable,
-- array of variables or matrix of variables (or procedure).
data Var = Var0 Id              -- a direct identifier
         | Var1 Id Expr         -- an identifier requiring one subscript
         | Var2 Id Expr Expr    -- a matrix of variables
    deriving (Show, Eq)

-- Expressions can take 19 different forms, as indicated below.
data Expr
  = VarExp Var          -- variable

  | BoolConst Bool      -- boolean constant
  | FloatConst Float    -- floating point constant
  | IntConst Int        -- integer constant
  | StrConst String     -- string constant (only be used for writing)

  | Add Expr Expr       -- sum of expressions
  | Sub Expr Expr       -- difference of expressions
  | Mul Expr Expr       -- product of expressions
  | Div Expr Expr       -- quotient of expressions
  | Neg Expr            -- mathematical (not boolean) negation of an expression

  | Equ Expr Expr       -- equality of expressions
  | NEq Expr Expr       -- inequality of expressions
  | LT  Expr Expr       -- less-than comparison
  | LTE Expr Expr       -- less-than-or-equal comparison
  | GT  Expr Expr       -- greater-than comparison
  | GTE Expr Expr       -- greater-than-or-equal comparison

  | And Expr Expr       -- boolean conjunction
  | Or  Expr Expr       -- boolean disjunction
  | Not Expr            -- boolean complement
    deriving (Show, Eq)
