module GoatLang.Semantics.AAST where

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
import OzLang.Code

data AGoatProgram
  = AGoatProgram [AProc]

data AProc
  = AProc Id [AParam] [ADecl] [AStmt] ProcAttr
data ProcAttr
  = ProcAttr { procFrameSize :: FrameSize }

data AParam
  = AParam PassBy BaseType Id ParamAttr
data ParamAttr
  = ParamAttr { paramStackSlot :: Slot }

data ADecl
  = ADecl BaseType Id Dim DeclAttr
data DeclAttr
  = DeclAttr { declStackSlots :: [Slot] }


data AStmt
  = AAsg AScalar AExpr
  | ARead AScalar ReadAttr
  | AWriteExpr AExpr WriteExprAttr
  | AWriteString String
  | ACall Id [AExpr] CallAttr
  | AIf AExpr [AStmt]
  | AIfElse AExpr [AStmt] [AStmt]
  | AWhile AExpr [AStmt]
data ReadAttr
  = ReadAttr { readBuiltin :: BuiltinFunc }
data WriteExprAttr
  = WriteExprAttr { writeExprBuiltin :: BuiltinFunc }
data CallAttr
  = CallAttr { callPassBys :: [PassBy] }

data AScalar
  = ASingle Id SingleAttr
  | AArray  Id AExpr ArrayAttr
  | AMatrix Id AExpr AExpr MatrixAttr
data SingleAttr
  = SingleAttr { singlePassBy :: PassBy
               , singleStackSlot :: Slot
               }
data ArrayAttr
  = ArrayAttr { arrayStartSlot :: Slot }
data MatrixAttr
  = MatrixAttr { matrixStartSlot :: Slot 
               , matrixRowWidth :: Int
               }

data AExpr
  = AScalarExpr AScalar
  | ABoolConst Bool
  | AFloatConst Float
  | AIntConst Int
  | ABinExpr BinOp AExpr AExpr BinExprAttr
  | AUnExpr UnOp AExpr UnExprAttr
  | AFloatCast AExpr
data BinExprAttr
  = BinExprAttr { binExprInstr :: (Reg -> Reg -> Reg -> Instruction)
                , binExprResultType :: BaseType
                }
data UnExprAttr
  = UnExprAttr { unExprInstr :: (Reg -> Reg -> Instruction)
               , unExprResultType :: BaseType
               }
