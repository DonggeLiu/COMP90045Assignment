module GoatLang.Semantics.AAST where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
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
  = ProcAttr { procFrameSize :: FrameSize }

data AParam
  = AParam PassBy BaseType Id ParamAttr
data ParamAttr
  = ParamAttr { paramStackSlot :: Slot
              , paramPretty :: String
              }

data ADecl
  = ADecl BaseType Id Dim DeclAttr
data DeclAttr
  = DeclAttr { declStackSlots :: [Slot]
             , declPretty :: String
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
  = AsgAttr { asgPretty :: String }
data ReadAttr
  = ReadAttr { readBuiltin :: BuiltinFunc
             , readPretty :: String
             }  
data WriteExprAttr
  = WriteExprAttr { writeExprBuiltin :: BuiltinFunc
                  , writeExprPretty :: String
                  }
data WriteStringAttr
  = WriteStringAttr { writeStringPretty :: String }
data CallAttr
  = CallAttr { callPassBys :: [PassBy]
             , callPretty :: String
             }
data IfAttr
  = IfAttr { ifPretty :: String }
data IfElseAttr
  = IfElseAttr { ifElsePretty :: String }
data WhileAttr
  = WhileAttr { whilePretty :: String }


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

-- scalarType
-- Access the base type of an annotated scalar reference.
scalarType :: AScalar -> BaseType
scalarType (ASingle _ attrs)
  = singleBaseType attrs
scalarType (AArray _ _ attrs)
  = arrayBaseType attrs
scalarType (AMatrix _ _ _ attrs)
  = matrixBaseType attrs



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

-- exprType
-- Access the result type of an annotated expression. Not recursive, because we
-- already annotated each expression with enough information to know its result
-- type.
exprType :: AExpr -> BaseType
exprType (AScalarExpr scalar)
  = scalarType scalar
exprType (ABoolConst _)
  = BoolType
exprType (AFloatConst _)
  = FloatType
exprType (AIntConst _)
  = IntType
exprType (ABinExpr op lExpr rExpr attrs)
  = binExprResultType attrs
exprType (AUnExpr op expr attrs)
  = unExprResultType attrs
exprType (AFloatCast expr)
  = FloatType
