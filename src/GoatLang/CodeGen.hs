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

type FrameSize = Int
type Register = Int
type Slot = Int

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
  | StoreInstr Slot Register
  | LoadInstr Register Slot
  | LoadAddressInstr Register Slot
  | LoadIndirectInstr Register Register
  | StoreIndirectInstr Register Register
  | IntConstInstr Register Int
  | RealConstInstr Register Float
  | StringConstInstr Register String
  | AddIntInstr Register Register Register
  | AddRealInstr Register Register Register
  | AddOffsetInstr Register Register Register
  | SubIntInstr Register Register Register
  | SubRealInstr Register Register Register
  | SubOffsetInstr Register Register Register
  | MulIntInstr Register Register Register
  | MulRealInstr Register Register Register
  | DivIntInstr Register Register Register
  | DivRealInstr Register Register Register
  | NegIntInstr Register Register
  | NegRealInstr Register Register
  | EquIntInstr Register Register Register
  | NEqIntInstr Register Register Register
  | GThIntInstr Register Register Register
  | GEqIntInstr Register Register Register
  | LThIntInstr Register Register Register
  | LEqIntInstr Register Register Register
  | EquRealInstr Register Register Register
  | NEqRealInstr Register Register Register
  | GThRealInstr Register Register Register
  | GEqRealInstr Register Register Register
  | LThRealInstr Register Register Register
  | LEqRealInstr Register Register Register
  | AndInstr Register Register Register
  | OrInstr Register Register Register
  | NotInstr Register Register
  | IntToRealInstr Register Register
  | MoveInstr Register Register
  | BranchOnTrueInstr Register Label
  | BranchOnFalseInstr Register Label
  | BrachUncondInstr Label
  | CallInstr Label
  | CallBuiltinInstr BuiltInFunc
  | ReturnInstr
  | HaltInstr
  | DebugRegInstr Register
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
