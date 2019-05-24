module GoatLang.Semantics.CodeGenMonad where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                GOAT - Oz Code Generator Monad and Helpers
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

import Control.Monad.State

import Util.DiffList

import GoatLang.AST
import GoatLang.OzCode


-- ----------------------------------------------------------------------------
-- CodeGen State Monad
-- ----------------------------------------------------------------------------

-- We'll use a state monad to simplify construction of Oz programs
type CodeGen a
  = State CodeGenState a

-- The state monad will store a label counter (an int) and a (difference) list
-- of OzLines (instructions or peudo-instructions; see OzCode module).
data CodeGenState
  = CodeGenState { nextLabelCounter :: LabelCounter
                 , generatedLines   :: DiffList OzLine
                 }

type LabelCounter
  = Int


-- ----------------------------------------------------------------------------
-- Running the Monad
-- ----------------------------------------------------------------------------

-- genCode
-- Top level function: use the code generators defined below to convert an
-- annotated Goat Program (an AAST) into an Oz Program (a list of Oz lines).
genCode :: CodeGen () -> OzProgram
genCode programGenerator
  = ozProgram
  where
    -- start with label counter at 0 and an empty list of program lines
    startState = CodeGenState { nextLabelCounter = 0
                              , generatedLines = mempty :: DiffList OzLine
                              }
    -- run the code generator to determine the final state
    ((), endState) = runState programGenerator startState
    -- extract the completed difference list of lines and convert the result
    -- into an Oz program
    ozProgram = OzProgram $ listify $ generatedLines endState


-- ----------------------------------------------------------------------------
-- Internal helper functions for interacting directly with the CodeGen state
-- ----------------------------------------------------------------------------

-- getLabelCounter
-- Extract the label counter from the monad's state and return.
getLabelCounter :: CodeGen LabelCounter
getLabelCounter
  = do
      state <- get
      return $ nextLabelCounter state

-- incLabelCounter
-- Add 1 to the label counter in the monad's state.
incLabelCounter :: CodeGen ()
incLabelCounter
  = do
      state <- get
      put $ state { nextLabelCounter = nextLabelCounter state + 1 }

-- appendLine
-- Append a new oz line to the monad's (difference) list of oz lines
-- (conceptually adding it to the end of the program-so-far).
appendLine :: OzLine -> CodeGen ()
appendLine line
  = do
      state <- get
      put $ state { generatedLines = (generatedLines state) `snoc` line }
      -- NOTE: snoc is 'backwards cons': append a single thing to a diff list


-- ----------------------------------------------------------------------------
-- External helper functions for indirectly using the CodeGen state
-- ----------------------------------------------------------------------------

-- getNewBlockLabel
-- Increment the internal label counter and return a fresh block label.
getNewBlockLabel :: CodeGen Label
getNewBlockLabel
  = do
      labelCounter <- getLabelCounter
      incLabelCounter
      return (BlockLabel labelCounter)

-- instr
-- Append an Instruction to the program's lines.
instr :: Instruction -> CodeGen ()
instr instruction
  = appendLine $ Instr instruction

-- label
-- Append a Label to the program's lines (as a pseudo-instruction).
label :: Label -> CodeGen ()
label lab
  = appendLine $ Label lab

-- comment
-- Append a Comment to the program's lines (as a pseudo-instruction).
comment :: String -> CodeGen ()
comment text
  = appendLine $ Comment text

