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

data Label
  = Label String

data BuiltInFunc
  = ReadBool
  | ReadFloat
  | ReadInt
  | PrintBool
  | PrintFloat
  | PrintInt
  | PrintStr

data InstrTree
  = InstrList [InstrTree]
  | InstrLeaf Instruction

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

-- ABinExpr attr Add (AIntConst attr 1) (AIntConst attr 2)  -- AExpr -> 105-112          <- Chosen for now
genCodeExprInto :: Register -> Expr -> InstrTree
genCodeExprInto register (AIntConst attr i)
  = InstrLeaf $ IntConstInstr register i
genCodeExprInto register (AFloatConst attr f)
  = InstrLeaf $ FloatConstInstr register f
genCodeExprInto register (ABoolConst attr b)
  = case b of
      True -> InstrLeaf $ IntConstInstr register 1
      False -> InstrLeaf $ IntConstInstr register 0
genCodeExprInto register (ABinExpr attr Add l r)
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , instr register register (register + 1)
    ]
    where
      instr = lookupInstrBinOp Add l r
lookupInstrBinOp :: BinOp -> AExpr -> AExpr
  -> (Register -> Register -> Register -> Instruction)
lookupInstrBinOp

genCodeExprInto register (ABinExpr t Add (FloatConst l) (IntConst r))
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , AddIntInstr register register (register + 1)
    ]
