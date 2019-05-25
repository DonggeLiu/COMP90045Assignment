module GoatLang.Semantics.SymbolTable where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                        GOAT - Symbol Tables
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

import Data.Map.Strict (Map, fromList, size, (!?), elems, insert, empty)

import GoatLang.Syntax.AST

import OzLang.Code


-- ----------------------------------------------------------------------------
-- Types for symbol tables and their contents
-- ----------------------------------------------------------------------------

-- TODO: Switch back to lookup by Id?

data ProcSymTable
  = ProcSymTable (Map String ProcRecord)

data VarSymTable
  = VarSymTable (Map String VarRecord) Int

data ProcRecord
  = ProcRecord { procParams :: [Param]
               , procDefnPos :: Pos
               }

data VarRecord
  = VarRecord { varShape :: Dim
              , varType :: BaseType
              , varPassBy :: PassBy
              , varStackSlot :: Slot
              , varDefnPos :: Pos
              }

-- ----------------------------------------------------------------------------
-- Querying symbol tables
-- ----------------------------------------------------------------------------

-- lookupVarRecord
-- Simply lookup the VarRecord for a given Variable's name.
lookupVarRecord :: VarSymTable -> String -> Maybe VarRecord
lookupVarRecord (VarSymTable varMap _) name
  = varMap !? name

-- lookupProcRecord
-- Simply lookup the ProcRecord for a given Procedure's name.
lookupProcRecord :: ProcSymTable -> String -> Maybe ProcRecord
lookupProcRecord (ProcSymTable procMap) name
  = procMap !? name


-- ----------------------------------------------------------------------------
-- Building symbol tables
-- ----------------------------------------------------------------------------

emptyProcSymTable :: ProcSymTable
emptyProcSymTable
  = ProcSymTable empty

insertProcRecord :: String -> ProcRecord -> ProcSymTable -> ProcSymTable
insertProcRecord name record (ProcSymTable procMap)
  = ProcSymTable $ insert name record procMap



emptyVarSymTable :: VarSymTable
emptyVarSymTable
  = VarSymTable empty 0

insertVarRecord :: String -> VarRecord -> VarSymTable -> VarSymTable
insertVarRecord name record (VarSymTable varMap numSlots)
  = VarSymTable (insert name record varMap) numSlots

allocateSlots :: Int -> VarSymTable -> VarSymTable
allocateSlots numNewSlots (VarSymTable varMap currentNumSlots)
  = VarSymTable varMap (currentNumSlots + numNewSlots)

