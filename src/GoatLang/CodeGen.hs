module GoatLang.CodeGen where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - Oz Code Generator
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

newtype FrameSize = FrameSize Int
newtype Reg = Reg Int
newtype Slot = Slot Int
newtype Comment = Comment String

data Label
  = Label String

data BuiltInFunc
  = ReadBool
  | ReadReal
  | ReadInt
  | PrintBool
  | PrintReal
  | PrintInt
  | PrintStr

data InstrTree
  = InstrList [InstrTree]
  | InstrLeaf Instruction
  | InstrLabel Label
  | InstrComment Comment

data Instruction
  = PushStackFrameInstr FrameSize
  | PopStackFrameInstr FrameSize

  | StoreInstr Slot Reg
  | LoadInstr Reg Slot
  | LoadAddressInstr Reg Slot
  | LoadIndirectInstr Reg Reg
  | StoreIndirectInstr Reg Reg

  | IntConstInstr Reg Int
  | RealConstInstr Reg Float
  | StringConstInstr Reg String

  | AddIntInstr Reg Reg Reg
  | AddRealInstr Reg Reg Reg
  | AddOffsetInstr Reg Reg Reg
  | SubIntInstr Reg Reg Reg
  | SubRealInstr Reg Reg Reg
  | SubOffsetInstr Reg Reg Reg
  | MulIntInstr Reg Reg Reg
  | MulRealInstr Reg Reg Reg
  | DivIntInstr Reg Reg Reg
  | DivRealInstr Reg Reg Reg
  | NegIntInstr Reg Reg
  | NegRealInstr Reg Reg

  | EquIntInstr Reg Reg Reg
  | NEqIntInstr Reg Reg Reg
  | GThIntInstr Reg Reg Reg
  | GEqIntInstr Reg Reg Reg
  | LThIntInstr Reg Reg Reg
  | LEqIntInstr Reg Reg Reg
  | EquRealInstr Reg Reg Reg
  | NEqRealInstr Reg Reg Reg
  | GThRealInstr Reg Reg Reg
  | GEqRealInstr Reg Reg Reg
  | LThRealInstr Reg Reg Reg
  | LEqRealInstr Reg Reg Reg

  | AndInstr Reg Reg Reg
  | OrInstr Reg Reg Reg
  | NotInstr Reg Reg

  | IntToRealInstr Reg Reg
  | MoveInstr Reg Reg

  | BranchOnTrueInstr Reg Label
  | BranchOnFalseInstr Reg Label
  | BrachUncondInstr Label

  | CallInstr Label
  | CallBuiltinInstr BuiltInFunc
  | ReturnInstr
  | HaltInstr

  | DebugRegInstr Reg
  | DebugSlotInstr Slot
  | DebugStackInstr

genCodeExprInto :: Register -> AExpr -> InstrTree
genCodeExprInto register (AIntConst attr i)
  = InstrLeaf $ IntConstInstr register i
genCodeExprInto register (AFloatConst attr f)
  = InstrLeaf $ FloatConstInstr register f
genCodeExprInto register (ABoolConst attr b)
  = case b of
      True -> InstrLeaf $ IntConstInstr register 1
      False -> InstrLeaf $ IntConstInstr register 0
genCodeExprInto register (ABinExpr attr op l r)
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , generateBinOpInstrTree register register (register + 1) op lType rType
    ]
    where
      lType = getExprType l
      rType = getExprType r

generateBinOpInstrTree :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType
  -> InstrTree
generateBinOpInstrTree destReg lReg rReg op lType rType
  = case (lType, rType) of
      (IntType, IntType) -> InstrLeaf intInstruction
      otherwise -> InstrList [ realify lReg lType
                             , realify rReg rType
                             , InstrLeaf floatInstruction
                             ]
      where
        floatInstruction = (lookupOpFloat op) destReg lReg rReg
        intInstruction = (lookupOpInt op) destReg lReg rReg

realify :: Reg -> BaseType -> InstrTree
realify _ FloatType
  = InstrList []
realify reg IntType
  = InstrLeaf $ IntToRealInstr reg reg

lookupOpFloat :: BinOp -> Reg -> Reg -> Reg -> Instruction
lookupOpFloat Add
  = AddRealInstr
lookupOpFloat Sub
  = SubRealInstr
lookupOpFloat Mul
  = MulRealInstr
lookupOpFloat Div
  = DivRealInstr

lookupOpInt :: BinOp -> Reg -> Reg -> Reg -> Instruction
lookupOpInt Add
  = AddIntInstr
lookupOpInt Sub
  = SubIntInstr
lookupOpInt Mul
  = MulIntInstr
lookupOpInt Div
  = DivIntInstr

getExprType :: AExpr -> BaseType
getExprType (ABoolConst _ _)
  = BoolType
getExprType (AFloatConst _ _)
  = FloatType
getExprType (AIntConst _ _)
  = IntType
getExprType (ABinExpr _ operator left right)
  = case operator of
      Add -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Sub -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Mul -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Div -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      otherwise -> BoolType
getExprType (AUnExpr _ operator operand)
  = getExprType operand
getExprType (AScalarExpr _ scalar)
  = getScalarType scalar

-- getScalarType :: Scalar -> BaseType
-- TODO: implement (using symbol table?)

genCodeStmt :: Stmt -> InstrTree
genCodeStmt (WriteExpr expr)
  = InstrList [ genCodeExprInto (Reg 0) expr
              , InstrLeaf $ CallBuiltinInstr builtin
              ]
    where
      builtin = case getExprType expr of
        BoolType -> PrintBool
        FloatType -> PrintReal
        IntType -> PrintInt
genCodeStmt (WriteString str)
  = InstrList [ InstrLeaf $ StringConstInstr (Reg 0) str
              , InstrLeaf $ CallBuiltinInstr PrintStr
              ]
