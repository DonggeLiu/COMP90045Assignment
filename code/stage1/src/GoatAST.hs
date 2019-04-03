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

data GoatProgram = GoatProgram [Proc]
    deriving (Show, Eq)

data Proc = Proc Id [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data Id = Id String
    deriving (Show, Eq)

data Param = Param PassBy Type Id
    deriving (Show, Eq)
data PassBy = Val | Ref
    deriving (Show, Eq)
data Type = BoolType | FloatType | IntType
    deriving (Show, Eq)


data Decl = Decl Type Id Dim
    deriving (Show, Eq)
data Dim = Dim0
         | Dim1 Int
         | Dim2 Int Int
    deriving (Show, Eq)
data Stmt
  = Asg Var Expr
  | Read Var
  | Write Expr
  | Call Id [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)


data Var = Var0 Id           -- a direct identifier
         | Var1 Id Expr      -- an identifier requiring one subscript
         | Var2 Id Expr Expr -- a matrix of variables
    deriving (Show, Eq)


data Expr
  = VarExp Var

  | BoolConst Bool
  | FloatConst Float
  | IntConst Int
  | StrConst String
  
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  
  | Equ Expr Expr
  | NEq Expr Expr
  | LT  Expr Expr
  | LTE Expr Expr
  | GT  Expr Expr
  | GTE Expr Expr
  
  | And Expr Expr
  | Or  Expr Expr
  | Not Expr
    deriving (Show, Eq)
