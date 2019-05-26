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

import GoatLang.Syntax.AST
import OzLang.Code

data AGoatProgram
  = AGoatProgram [AProc]

data AProc
  = AProc Id [AParam] [ADecl] [AStmt] ProcAttr
data ProcAttr
  = ProcAttr { procFrameSize :: FrameSize
             , prettifiedProc :: String 
             }

data AParam
  = AParam PassBy BaseType Id ParamAttr
data ParamAttr
  = ParamAttr { paramStackSlot :: Slot
              , prettifiedParam :: String
              }

data ADecl
  = ADecl BaseType Id Dim DeclAttr
data DeclAttr
  = DeclAttr { declStackSlots :: [Slot]
             , prettifiedDecl :: String
             }


data AStmt
  = AAsg AScalar AExpr AsgAttr
  | ARead AScalar ReadAttr
  | AWriteExpr AExpr WriteExprAttr
  | AWriteString String WriteStringAttr
  | ACall Id [AExpr] CallAttr
  | AIf AExpr [AStmt] IfAttr
  | AIfElse AExpr [AStmt] [AStmt] IfElseAttr
  | AWhile AExpr [AStmt] WhileAttr
data AsgAttr
  = AsgAttr { prettifiedAsg :: String }
data ReadAttr
  = ReadAttr { readBuiltin :: BuiltinFunc
             , prettifiedRead :: String
             }
data WriteExprAttr
  = WriteExprAttr { writeExprBuiltin :: BuiltinFunc 
                  , prettifiedWriteExpr :: String 
                  }
data WriteStringAttr
  = WriteStringAttr { prettifiedWriteString :: String }
data CallAttr
  = CallAttr { callPassBys :: [PassBy] 
             , prettifiedCall :: String
             }
data IfAttr
  = IfAttr { prettifiedIf :: String }
data IfElseAttr
  = IfElseAttr { prettifiedIfElse :: String }
data WhileAttr
  = WhileAttr { prettifiedWhile :: String }


data AScalar
  = ASingle Id SingleAttr
  | AArray  Id AExpr ArrayAttr
  | AMatrix Id AExpr AExpr MatrixAttr
data SingleAttr
  = SingleAttr { singlePassBy :: PassBy
               , singleStackSlot :: Slot
               , singleBaseType :: BaseType
               }
data ArrayAttr
  = ArrayAttr { arrayStartSlot :: Slot
              , arrayBaseType :: BaseType
              }
data MatrixAttr
  = MatrixAttr { matrixStartSlot :: Slot 
               , matrixRowWidth :: Int
               , matrixBaseType :: BaseType
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
