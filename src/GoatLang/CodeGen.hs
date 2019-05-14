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

data InstrTree
  = InstrList [InstrTree]
  | InstrLeaf Instruction

-- newtype StackSlot = Slot Int
-- type LabelCounter = Int
newtype Reg = Reg Int
type FuncName = String

data Instruction 
  = IntConstrInstr Reg Int
  | RealConstInstr Reg Float
  | StringConstInstr Reg String
  | CallBuiltinInstr FuncName

  -- | StoreInstr StackSlot Reg
  -- | LoadInstr Reg StackSlot
  -- | LoadAddrInstr Reg StackSlot
  -- | LoadIndr Reg Reg
  -- | StoreIndr Reg Reg

  -- | AddIntInstr Reg Reg Reg
  -- | AddRealInstr Reg Reg Reg
  -- | AddOffsetInstr Reg Reg Reg
  -- | SubIntInstr Reg Reg Reg
  -- | SubRealInstr Reg Reg Reg
  -- | SubOffsetInstr Reg Reg Reg
  -- | MulIntInstr Reg Reg Reg
  -- | MulRealInstr Reg Reg Reg
  -- | DivIntInstr Reg Reg Reg
  -- | DivRealInstr Reg Reg Reg
  -- | NegIntInstr Reg Reg
  -- | NegRealInstr Reg Reg

  -- | CmpEqInInstr Reg Reg Reg
  -- | CmpNeIntInst Reg Reg Reg
  -- | CmpGtIntInstr Reg Reg Reg
  -- | CmpGeIntInstr Reg Reg Reg
  -- | CmpLtIntInstr Reg Reg Reg
  -- | CmpLeIntInstr Reg Reg Reg

  -- | CmpEqRealInstr Reg Reg Reg
  -- | CmpNeRealInstr Reg Reg Reg
  -- | CmpGtRealInstr Reg Reg Reg
  -- | CmpGeRealInstr Reg Reg Reg
  -- | CmpLtRealInstr Reg Reg Reg
  -- | CmpLeRealInstr Reg Reg Reg

  -- | AndInstr Reg Reg Reg
  -- | OrInstr Reg Reg Reg
  -- | NotInstr Reg Reg

  -- | IntToRealInstr Reg Reg
  -- | MoveInstr Reg Reg

  -- | BranchOnTrueInstr Reg LabelCounter??
  -- | BranchOnFalseInstr Reg LabelCounter??
  -- | BranchUncondInstr LabelCounter??

  -- | CallInstr 
  -- | CallBuiltinInstr String?? FuncName??
  -- | Return 
  -- | Halt

  -- | DebugReg Reg
  -- | DebugSlot StackSlot
  -- | DebugStack



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
