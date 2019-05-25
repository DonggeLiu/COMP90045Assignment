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





-- -- declStartSlotsFrom
-- -- Determine appropriate starting slots for a series of declarations, starting
-- -- from a given slot. The resulting list contains n+1 slots where n is the
-- -- number of declarations: the last slot is the next available slot after the
-- -- slots for all the decls.
-- declStartSlotsFrom :: Slot -> [Decl] -> [Slot]
-- declStartSlotsFrom (Slot start) decls
--   = map Slot $ scanl (+) start $ map numSlotsDecl decls


-- -- numSlotsDecl
-- -- Gets the number of slots required for a given declared variable.
-- numSlotsDecl :: Decl -> Int
-- numSlotsDecl (Decl _ _ _ dim)
--   = numSlotsDim dim

