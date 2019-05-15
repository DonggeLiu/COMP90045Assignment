module GoatLang.OzCode where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                  GOAT - Oz Code Constants and Printing
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

import Control.Monad (mapM_)
import Numeric (showFFloatAlt)

import Util.StringBuilder


newtype FrameSize
  = FrameSize Int
newtype Reg
  = Reg Int
instance Enum Reg where
  fromEnum (Reg r) = r
  toEnum r = Reg r

newtype Slot
  = Slot Int

data Label
  = ProcLabel String
  | BlockLabel Int

data InstrTree
  = InstrList [InstrTree]
  | InstrLeaf Instruction
  | InstrLabel Label
  | InstrComment String

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
  | CallBuiltinInstr BuiltinFunc
  | ReturnInstr
  | HaltInstr

  | DebugRegInstr Reg
  | DebugSlotInstr Slot
  | DebugStackInstr

data BuiltinFunc
  = ReadBool
  | ReadReal
  | ReadInt
  | PrintBool
  | PrintReal
  | PrintInt
  | PrintStr



-- printInstructions
-- Top-level function to transform an Instruction Tree into a string and print
-- it directly to stdout.
printInstructions :: InstrTree -> IO ()
printInstructions instructions
  = putStr $ stringify instructions


-- stringify
-- Convert an Instruction Tree to a multi-line string representing its generated
-- Oz code.
-- NOTE: The result includes a trailing newline! If printing, just use
-- putStr rather than putStrLn. Or just use `printInstructions'.
stringify :: InstrTree -> String
stringify instructions
  = buildString $ writeInstructions instructions

-- writeInstructions
-- Traverse an instruction tree to construct a string representation. This
-- function handles lining up the instructions, indentation, placement
-- of labels, etc.
writeInstructions :: InstrTree -> StringBuilder
writeInstructions (InstrLeaf instr)
  = space >> space >> space >> space >> writeInstruction instr >> newline
writeInstructions (InstrLabel label)
  = writeLabelName label >> write ":" >> newline
writeInstructions (InstrComment text)
  = space >> space >> writeComment text >> newline
writeInstructions (InstrList instrs)
  = mapM_ writeInstructions instrs

-- writeComment
-- A comment is just a string starting with "# ".
writeComment :: String -> StringBuilder
writeComment text
  = write "#" >> space >> write text

-- writeLabelName
-- A label is just a string with ":" appended.
writeLabelName :: Label -> StringBuilder
writeLabelName (ProcLabel procname)
  = write "proc_" >> write procname
writeLabelName (BlockLabel labelnum)
  = write "label_" >> showWrite labelnum

-- writeInstruction
-- Format an Instruction as the corresponding Oz code, including the Oz name
-- for the instruction, and its (possibly empty) comma-separated list of
-- arguments.
writeInstruction :: Instruction -> StringBuilder
writeInstruction (PushStackFrameInstr framesize)
  = write "push_stack_frame" >> space >> writeFrameSize framesize
writeInstruction (PopStackFrameInstr  framesize)
  = write "pop_stack_frame" >> space >> writeFrameSize framesize

writeInstruction (StoreInstr slot rI)
  = write "store" >> space >> commaSep [writeSlot slot, writeReg rI]
writeInstruction (LoadInstr rI slot)
  = write "load" >> space >> commaSep [writeReg rI, writeSlot slot]
writeInstruction (LoadAddressInstr rI slot)
  = write "load_address" >> space >> commaSep [writeReg rI, writeSlot slot]
writeInstruction (LoadIndirectInstr rI rJ)
  = write "load_indirect" >> space >> commaSep [writeReg rI, writeReg rJ]
writeInstruction (StoreIndirectInstr rI rJ)
  = write "store_indirect" >> space >> commaSep [writeReg rI, writeReg rJ]

writeInstruction (IntConstInstr rI intconst)
  = write "int_const" >> space >> commaSep [writeReg rI, showWrite intconst]
writeInstruction (RealConstInstr rI realconst)
  = write "real_const" >> space >> commaSep [writeReg rI, writeFloat realconst]
writeInstruction (StringConstInstr rI strconst)
  = write "string_const" >> space >> commaSep [writeReg rI,writeStrLit strconst]

writeInstruction (AddIntInstr rI rJ rK)
  = write "add_int" >> space >> registers [rI, rJ, rK]
writeInstruction (AddRealInstr rI rJ rK)
  = write "add_real" >> space >> registers [rI, rJ, rK]
writeInstruction (AddOffsetInstr rI rJ rK)
  = write "add_offset" >> space >> registers [rI, rJ, rK]
writeInstruction (SubIntInstr rI rJ rK)
  = write "sub_int" >> space >> registers [rI, rJ, rK]
writeInstruction (SubRealInstr rI rJ rK)
  = write "sub_real" >> space >> registers [rI, rJ, rK]
writeInstruction (SubOffsetInstr rI rJ rK)
  = write "sub_offset" >> space >> registers [rI, rJ, rK]
writeInstruction (MulIntInstr rI rJ rK)
  = write "mul_int" >> space >> registers [rI, rJ, rK]
writeInstruction (MulRealInstr rI rJ rK)
  = write "mul_real" >> space >> registers [rI, rJ, rK]
writeInstruction (DivIntInstr rI rJ rK)
  = write "div_int" >> space >> registers [rI, rJ, rK]
writeInstruction (DivRealInstr rI rJ rK)
  = write "div_real" >> space >> registers [rI, rJ, rK]
writeInstruction (NegIntInstr rI rJ)
  = write "neg_int" >> space >> registers [rI, rJ]
writeInstruction (NegRealInstr rI rJ)
  = write "neg_real" >> space >> registers [rI, rJ]

writeInstruction (EquIntInstr rI rJ rK)
  = write "cmp_eq_int" >> space >> registers [rI, rJ, rK]
writeInstruction (NEqIntInstr rI rJ rK)
  = write "cmp_ne_int" >> space >> registers [rI, rJ, rK]
writeInstruction (GThIntInstr rI rJ rK)
  = write "cmp_gt_int" >> space >> registers [rI, rJ, rK]
writeInstruction (GEqIntInstr rI rJ rK)
  = write "cmp_ge_int" >> space >> registers [rI, rJ, rK]
writeInstruction (LThIntInstr rI rJ rK)
  = write "cmp_lt_int" >> space >> registers [rI, rJ, rK]
writeInstruction (LEqIntInstr rI rJ rK)
  = write "cmp_le_int" >> space >> registers [rI, rJ, rK]
writeInstruction (EquRealInstr rI rJ rK)
  = write "cmp_eq_real" >> space >> registers [rI, rJ, rK]
writeInstruction (NEqRealInstr rI rJ rK)
  = write "cmp_ne_real" >> space >> registers [rI, rJ, rK]
writeInstruction (GThRealInstr rI rJ rK)
  = write "cmp_gt_real" >> space >> registers [rI, rJ, rK]
writeInstruction (GEqRealInstr rI rJ rK)
  = write "cmp_ge_real" >> space >> registers [rI, rJ, rK]
writeInstruction (LThRealInstr rI rJ rK)
  = write "cmp_lt_real" >> space >> registers [rI, rJ, rK]
writeInstruction (LEqRealInstr rI rJ rK)
  = write "cmp_le_real" >> space >> registers [rI, rJ, rK]

writeInstruction (AndInstr rI rJ rK)
  = write "and" >> space >> registers [rI, rJ, rK]
writeInstruction (OrInstr rI rJ rK)
  = write "or" >> space >> registers [rI, rJ, rK]
writeInstruction (NotInstr rI rJ)
  = write "not" >> space >> registers [rI, rJ]

writeInstruction (IntToRealInstr rI rJ)
  = write "int_to_real" >> space >> registers [rI, rJ]
writeInstruction (MoveInstr rI rJ)
  = write "move" >> space >> registers [rI, rJ]

writeInstruction (BranchOnTrueInstr rI label)
  = do
      write "branch_on_true" >> space
      commaSep [writeReg rI, writeLabelName label]
writeInstruction (BranchOnFalseInstr rI label)
  = do
      write "branch_on_false" >> space
      commaSep [writeReg rI, writeLabelName label]
writeInstruction (BrachUncondInstr label)
  = write "branch_uncond" >> space >> writeLabelName label

writeInstruction (CallInstr label)
  = write "call" >> space >> writeLabelName label
writeInstruction (CallBuiltinInstr builtin)
  = write "call_builtin" >> space >> writeBuiltinFunc builtin
writeInstruction ReturnInstr
  = write "return"
writeInstruction HaltInstr
  = write "halt"

writeInstruction (DebugRegInstr rI)
  = write "debug_reg" >> space >> writeReg rI
writeInstruction (DebugSlotInstr slotnum)
  = write "debug_slot" >> space >> writeSlot slotnum
writeInstruction DebugStackInstr
  = write "debug_stack"


-- registers
-- Helper to create action to write a comma-separed list of registers
-- (since this is such a common instruction argument list format).
registers :: [Reg] -> StringBuilder
registers rs
  = commaSep $ map writeReg rs

-- writeFrameSize
-- Create an action to write a frame size (an integer)
writeFrameSize :: FrameSize -> StringBuilder
writeFrameSize (FrameSize size)
  = showWrite size

-- writeReg
-- Create an action to write a register (an integer, preceded by r)
writeReg :: Reg -> StringBuilder
writeReg (Reg registernumber)
  = write "r" >> showWrite registernumber

-- writeSlot
-- Create an action to write a slot number (an integer)
writeSlot :: Slot -> StringBuilder
writeSlot (Slot slotnumber)
  = showWrite slotnumber

-- writeBuiltinFunc
-- Create action to write the Oz name for a BuiltinFunc
writeBuiltinFunc :: BuiltinFunc -> StringBuilder
writeBuiltinFunc ReadBool
  = write "read_bool"
writeBuiltinFunc ReadReal
  = write "read_real"
writeBuiltinFunc ReadInt
  = write "read_int"
writeBuiltinFunc PrintBool
  = write "print_bool"
writeBuiltinFunc PrintReal
  = write "print_real"
writeBuiltinFunc PrintInt
  = write "print_int"
writeBuiltinFunc PrintStr
  = write "print_string"


-- writeFloat
-- Create an action to represent a float in decimal notation (always with '.'
-- and never in exponential notation). See LMS Discussion Board.
writeFloat :: Float -> StringBuilder
writeFloat float
  = write $ showFFloatAlt Nothing float ""

-- writeStrLit
-- Create an action to represent a string literal as a string.
-- Note: we have to 'unparse' the string for representation, or it will contain
-- actual newlines etc.
writeStrLit :: String -> StringBuilder
writeStrLit str
  = quote $ mapM_ writeCharEsc str

-- writeCharEsc
-- Write a single character, taking care to 'unparse' escaped characters back
-- into escape sequences (namely `\n` --> `\` followed by `n`).
writeCharEsc :: Char -> StringBuilder
writeCharEsc '\n'
    -- a `\` (slash) followed by `n`.
    = write "\\" >> write "n"
writeCharEsc c
    -- just the character itself (as a string)
    = write (c:"")
