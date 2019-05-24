module OzLang.Code where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                          GOAT - Oz Code Constants
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

-- ----------------------------------------------------------------------------
-- Fundamental Oz program concepts
-- ----------------------------------------------------------------------------

newtype FrameSize
  = FrameSize Int
  deriving (Show, Eq)

newtype Reg
  = Reg Int
  deriving (Show, Eq)
instance Enum Reg where
  fromEnum (Reg r) = r
  toEnum r = Reg r

newtype Slot
  = Slot Int
  deriving (Show, Eq)
instance Enum Slot where
  fromEnum (Slot s) = s
  toEnum s = Slot s

data Label
  = ProcLabel String
  | BlockLabel Int
  deriving (Show, Eq)

data BuiltinFunc
  = ReadBool
  | ReadReal
  | ReadInt
  | PrintBool
  | PrintReal
  | PrintInt
  | PrintStr
  deriving (Show, Eq)


-- ----------------------------------------------------------------------------
-- Representation of an entire Oz program as a list of (pseudo) instructions
-- ----------------------------------------------------------------------------

data OzProgram
  = OzProgram [OzLine]
  deriving (Show, Eq)

data OzLine
  = Instr Instruction
  | Label Label
  | Comment String
  deriving (Show, Eq)

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
  | BranchUncondInstr Label

  | CallInstr Label
  | CallBuiltinInstr BuiltinFunc
  | ReturnInstr
  | HaltInstr

  | DebugRegInstr Reg
  | DebugSlotInstr Slot
  | DebugStackInstr
  deriving (Show, Eq)
