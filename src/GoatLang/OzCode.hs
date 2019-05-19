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

import Util.CodeWriter


-- ----------------------------------------------------------------------------
-- Internal representation of an Oz program
-- ----------------------------------------------------------------------------

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

data OzProgram
  = OzProgram [OzLine]

data OzLine
  = Instr Instruction
  | Label Label
  | Comment String

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

data BuiltinFunc
  = ReadBool
  | ReadReal
  | ReadInt
  | PrintBool
  | PrintReal
  | PrintInt
  | PrintStr


-- ----------------------------------------------------------------------------
-- Constructing an Oz Program as a string
-- ----------------------------------------------------------------------------

-- printOzProgram
-- Top-level function to transform an Instruction Tree into a string and print
-- it directly to stdout.
printOzProgram :: OzProgram -> IO ()
printOzProgram prog
  = printOzProgramColoured (getColourSchemeByName NoColours) prog

printOzProgramColoured :: ColourScheme -> OzProgram -> IO ()
printOzProgramColoured cs prog
  = putStr $ stringify cs prog

-- stringify
-- Convert an Instruction Tree to a multi-line string representing its generated
-- Oz code.
-- NOTE: The result includes a trailing newline! If printing, just use
-- putStr rather than putStrLn. Or just use `printInstructions'.
stringify :: ColourScheme -> OzProgram -> String
stringify cs prog
  = writeCodeColoured cs $ writeOzProgram prog


-- ----------------------------------------------------------------------------
-- CodeWriters for the different elements of an Oz program
-- ----------------------------------------------------------------------------

-- writeOzProgram
-- Traverse an instruction list to construct a string representation.
writeOzProgram :: OzProgram -> CodeWriter ()
writeOzProgram (OzProgram lines)
  = mapM_ writeOzLine lines

-- writeOzLine
-- Write different types of oz lines appropriately. This function handles
-- lining up the instructions, indentation, placement of labels, etc.
writeOzLine :: OzLine -> CodeWriter ()
writeOzLine (Instr instr)
  = space >> space >> space >> space >> writeInstruction instr >> newline
writeOzLine (Label label)
  = writeLabelName label >> write ":" >> newline
writeOzLine (Comment text)
  = space >> space >> writeComment text >> newline

-- writeComment
-- A comment is just a string starting with "# ".
writeComment :: String -> CodeWriter ()
writeComment text
  = asComment $ write "#" >> space >> write text

-- writeLabelName
-- We need a uniform way to 'spell' labels (one for each counter and procedure,
-- for example) when they are included in the instruction list and/or mentioned
-- as arguments to specific instructions.
writeLabelName :: Label -> CodeWriter ()
writeLabelName (ProcLabel procname)
  = asIdent $ write "proc_" >> write procname
writeLabelName (BlockLabel labelnum)
  = asIdent $ write "label_" >> showWrite labelnum


-- writeInstruction
-- Format an Instruction as the corresponding Oz code, including the Oz name
-- for the instruction, and its (possibly empty) comma-separated list of
-- arguments.
writeInstruction :: Instruction -> CodeWriter ()

writeInstruction (PushStackFrameInstr framesize)
  = writeKeyword "push_stack_frame" >> space >> writeFrameSize framesize
writeInstruction (PopStackFrameInstr  framesize)
  = writeKeyword "pop_stack_frame" >> space >> writeFrameSize framesize

writeInstruction (StoreInstr slot rI)
  = writeKeyword "store" >> space >> commaSep [writeSlot slot, writeReg rI]
writeInstruction (LoadInstr rI slot)
  = writeKeyword "load" >> space >> commaSep [writeReg rI, writeSlot slot]
writeInstruction (LoadAddressInstr rI slot)
  = writeKeyword "load_address" >> space >> commaSep [writeReg rI, writeSlot slot]
writeInstruction (LoadIndirectInstr rI rJ)
  = writeKeyword "load_indirect" >> space >> commaSep [writeReg rI, writeReg rJ]
writeInstruction (StoreIndirectInstr rI rJ)
  = writeKeyword "store_indirect" >> space >> commaSep [writeReg rI, writeReg rJ]

writeInstruction (IntConstInstr rI intconst)
  = writeKeyword "int_const" >> space >>
      commaSep [writeReg rI, writeIntLit intconst]
writeInstruction (RealConstInstr rI realconst)
  = writeKeyword "real_const" >> space >>
      commaSep [writeReg rI, writeFloatLit realconst]
writeInstruction (StringConstInstr rI strconst)
  = writeKeyword "string_const" >> space >>
      commaSep [writeReg rI,writeStringLit strconst]

writeInstruction (AddIntInstr rI rJ rK)
  = writeKeyword "add_int" >> space >> registers [rI, rJ, rK]
writeInstruction (AddRealInstr rI rJ rK)
  = writeKeyword "add_real" >> space >> registers [rI, rJ, rK]
writeInstruction (AddOffsetInstr rI rJ rK)
  = writeKeyword "add_offset" >> space >> registers [rI, rJ, rK]
writeInstruction (SubIntInstr rI rJ rK)
  = writeKeyword "sub_int" >> space >> registers [rI, rJ, rK]
writeInstruction (SubRealInstr rI rJ rK)
  = writeKeyword "sub_real" >> space >> registers [rI, rJ, rK]
writeInstruction (SubOffsetInstr rI rJ rK)
  = writeKeyword "sub_offset" >> space >> registers [rI, rJ, rK]
writeInstruction (MulIntInstr rI rJ rK)
  = writeKeyword "mul_int" >> space >> registers [rI, rJ, rK]
writeInstruction (MulRealInstr rI rJ rK)
  = writeKeyword "mul_real" >> space >> registers [rI, rJ, rK]
writeInstruction (DivIntInstr rI rJ rK)
  = writeKeyword "div_int" >> space >> registers [rI, rJ, rK]
writeInstruction (DivRealInstr rI rJ rK)
  = writeKeyword "div_real" >> space >> registers [rI, rJ, rK]
writeInstruction (NegIntInstr rI rJ)
  = writeKeyword "neg_int" >> space >> registers [rI, rJ]
writeInstruction (NegRealInstr rI rJ)
  = writeKeyword "neg_real" >> space >> registers [rI, rJ]

writeInstruction (EquIntInstr rI rJ rK)
  = writeKeyword "cmp_eq_int" >> space >> registers [rI, rJ, rK]
writeInstruction (NEqIntInstr rI rJ rK)
  = writeKeyword "cmp_ne_int" >> space >> registers [rI, rJ, rK]
writeInstruction (GThIntInstr rI rJ rK)
  = writeKeyword "cmp_gt_int" >> space >> registers [rI, rJ, rK]
writeInstruction (GEqIntInstr rI rJ rK)
  = writeKeyword "cmp_ge_int" >> space >> registers [rI, rJ, rK]
writeInstruction (LThIntInstr rI rJ rK)
  = writeKeyword "cmp_lt_int" >> space >> registers [rI, rJ, rK]
writeInstruction (LEqIntInstr rI rJ rK)
  = writeKeyword "cmp_le_int" >> space >> registers [rI, rJ, rK]
writeInstruction (EquRealInstr rI rJ rK)
  = writeKeyword "cmp_eq_real" >> space >> registers [rI, rJ, rK]
writeInstruction (NEqRealInstr rI rJ rK)
  = writeKeyword "cmp_ne_real" >> space >> registers [rI, rJ, rK]
writeInstruction (GThRealInstr rI rJ rK)
  = writeKeyword "cmp_gt_real" >> space >> registers [rI, rJ, rK]
writeInstruction (GEqRealInstr rI rJ rK)
  = writeKeyword "cmp_ge_real" >> space >> registers [rI, rJ, rK]
writeInstruction (LThRealInstr rI rJ rK)
  = writeKeyword "cmp_lt_real" >> space >> registers [rI, rJ, rK]
writeInstruction (LEqRealInstr rI rJ rK)
  = writeKeyword "cmp_le_real" >> space >> registers [rI, rJ, rK]

writeInstruction (AndInstr rI rJ rK)
  = writeKeyword "and" >> space >> registers [rI, rJ, rK]
writeInstruction (OrInstr rI rJ rK)
  = writeKeyword "or" >> space >> registers [rI, rJ, rK]
writeInstruction (NotInstr rI rJ)
  = writeKeyword "not" >> space >> registers [rI, rJ]

writeInstruction (IntToRealInstr rI rJ)
  = writeKeyword "int_to_real" >> space >> registers [rI, rJ]
writeInstruction (MoveInstr rI rJ)
  = writeKeyword "move" >> space >> registers [rI, rJ]

writeInstruction (BranchOnTrueInstr rI label)
  = writeKeyword "branch_on_true" >> space >> 
      commaSep [writeReg rI, writeLabelName label]
writeInstruction (BranchOnFalseInstr rI label)
  = writeKeyword "branch_on_false" >> space >> 
      commaSep [writeReg rI, writeLabelName label]
writeInstruction (BranchUncondInstr label)
  = writeKeyword "branch_uncond" >> space >> writeLabelName label

writeInstruction (CallInstr label)
  = writeKeyword "call" >> space >> writeLabelName label
writeInstruction (CallBuiltinInstr builtin)
  = writeKeyword "call_builtin" >> space >> writeBuiltinFunc builtin
writeInstruction ReturnInstr
  = writeKeyword "return"
writeInstruction HaltInstr
  = writeKeyword "halt"

writeInstruction (DebugRegInstr rI)
  = writeKeyword "debug_reg" >> space >> writeReg rI
writeInstruction (DebugSlotInstr slotnum)
  = writeKeyword "debug_slot" >> space >> writeSlot slotnum
writeInstruction DebugStackInstr
  = writeKeyword "debug_stack"


-- ----------------------------------------------------------------------------
-- Helper code writers for the arguments of Oz instructions
-- ----------------------------------------------------------------------------

-- registers
-- Helper to create action to write a comma-separed list of registers
-- (since this is such a common instruction argument list format).
registers :: [Reg] -> CodeWriter ()
registers rs
  = commaSep $ map writeReg rs

-- writeFrameSize
-- Create an action to write a frame size (an integer)
writeFrameSize :: FrameSize -> CodeWriter ()
writeFrameSize (FrameSize size)
  = asNumber $ showWrite size

-- writeReg
-- Create an action to write a register (an integer, preceded by r)
writeReg :: Reg -> CodeWriter ()
writeReg (Reg registernumber)
  = write "r" >> showWrite registernumber

-- writeSlot
-- Create an action to write a slot number (an integer)
writeSlot :: Slot -> CodeWriter ()
writeSlot (Slot slotnumber)
  = asIdent $ showWrite slotnumber

-- writeBuiltinFunc
-- Create action to write the Oz name for a BuiltinFunc
writeBuiltinFunc :: BuiltinFunc -> CodeWriter ()
writeBuiltinFunc ReadBool
  = writeIdent "read_bool"
writeBuiltinFunc ReadReal
  = writeIdent "read_real"
writeBuiltinFunc ReadInt
  = writeIdent "read_int"
writeBuiltinFunc PrintBool
  = writeIdent "print_bool"
writeBuiltinFunc PrintReal
  = writeIdent "print_real"
writeBuiltinFunc PrintInt
  = writeIdent "print_int"
writeBuiltinFunc PrintStr
  = writeIdent "print_string"


-- ----------------------------------------------------------------------------
-- CodeWriters for formatting constants (int, float, string) in Oz format
-- ----------------------------------------------------------------------------

-- writeIntLit
-- Create an action to represent an int in the required format (which is just
-- the default 'show' format).
writeIntLit :: Int -> CodeWriter ()
writeIntLit int
  = asNumber $ showWrite int

-- writeFloatLit
-- Create an action to represent a float in decimal notation (always with '.'
-- and never in exponential notation). See LMS Discussion Board.
writeFloatLit :: Float -> CodeWriter ()
writeFloatLit float
  = asNumber $ write $ showFFloatAlt Nothing float ""

-- writeStringLit
-- Create an action to represent a string literal as a string.
-- Note: we have to 'unparse' the string for representation, or it will contain
-- actual newlines etc.
writeStringLit :: String -> CodeWriter ()
writeStringLit str
  = asString $ quote $ mapM_ writeCharEsc str

-- writeCharEsc
-- Write a single character, taking care to 'unparse' escaped characters back
-- into escape sequences (namely `\n` --> `\` followed by `n`).
writeCharEsc :: Char -> CodeWriter ()
writeCharEsc '\n'
    -- a `\` (slash) followed by `n`.
    = write "\\" >> write "n"
writeCharEsc c
    -- just the character itself (as a string)
    = write (c:"")
