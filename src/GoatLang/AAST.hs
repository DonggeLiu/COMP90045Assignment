module GoatLang.AAST where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                GOAT - ANNOTATED ABSTRACT SYNTAX TREE
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

import GoatLang.AST
import GoatLang.OzCode

data AGoatProgram
  = AGoatProgram [AProc]
    -- deriving (Show, Eq)

data AProc
  = AProc Id [AParam] [ADecl] [AStmt] ProcAttr
    -- deriving (Show, Eq)
data ProcAttr
  = ProcAttr { procFrameSize :: FrameSize }
    -- deriving (Show, Eq)

data AParam
  = AParam PassBy BaseType Id ParamAttr
    -- deriving (Show, Eq)
data ParamAttr
  = ParamAttr { paramStackSlot :: Slot }
    -- deriving (Show, Eq)

data ADecl
  = ADecl BaseType Id Dim DeclAttr
    -- deriving (Show, Eq)
data DeclAttr
  = DeclAttr { declStackSlots :: [Slot] }
    -- deriving (Show, Eq)

  


-- Statements can take 7 different forms, as indicated below.
data AStmt
  = AAsg AScalar AExpr
  | ARead AScalar ReadAttr
  | AWriteExpr AExpr WriteExprAttr
  | AWriteString String
  | ACall Id [AExpr] CallAttr
  | AIf AExpr [AStmt]
  | AIfElse AExpr [AStmt] [AStmt]
  | AWhile AExpr [AStmt]
    -- deriving (Show, Eq)

data ReadAttr
  = ReadAttr { readBuiltin :: BuiltinFunc }
    -- deriving (Show, Eq)
data WriteExprAttr
  = WriteExprAttr { writeExprBuiltin :: BuiltinFunc }
    -- deriving (Show, Eq)
data CallAttr
  = CallAttr { callParams :: [AParam] }
    -- deriving (Show, Eq)

data AScalar
  = ASingle Id SingleAttr
  | AArray  Id AExpr ArrayAttr
  | AMatrix Id AExpr AExpr MatrixAttr
    -- deriving (Show, Eq)
data SingleAttr
  = SingleAttr { singlePassBy :: PassBy
               , singleStackSlot :: Slot
               }
    -- deriving (Show, Eq)
data ArrayAttr
  = ArrayAttr { arrayStartSlot :: Slot }
    -- deriving (Show, Eq)
data MatrixAttr
  = MatrixAttr { matrixStartSlot :: Slot 
               , matrixNumCols   :: Int
               }
    -- deriving (Show, Eq)

data AExpr
  = AScalarExpr AScalar
  | ABoolConst Bool
  | AFloatConst Float
  | AIntConst Int
  | ABinExpr BinOp AExpr AExpr BinExprAttr
  | AUnExpr UnOp AExpr UnExprAttr
  | AFloatCast AExpr
    -- deriving (Show, Eq)

data BinExprAttr
  = BinExprAttr { binExprInstr :: (Reg -> Reg -> Reg -> Instruction) }
    -- deriving (Show, Eq)

data UnExprAttr
  = UnExprAttr { unExprInstr :: (Reg -> Reg -> Instruction) }
    -- deriving (Show, Eq)
