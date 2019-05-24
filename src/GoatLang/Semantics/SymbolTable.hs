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

import Data.Map.Strict (Map, fromList, size, (!?), elems)

import GoatLang.Syntax.AST

import OzLang.Code


-- ----------------------------------------------------------------------------
-- Types for symbol tables and their contents
-- ----------------------------------------------------------------------------

data ProcSymTable
  = ProcSymTable (Map String ProcRecord)

data VarSymTable
  = VarSymTable (Map String VarRecord)

data ProcRecord
  = ProcRecord { procParams :: [Param] }

data VarRecord
  = VarRecord { varShape :: Dim
              , varType :: BaseType
              , varPassBy :: PassBy
              , varStackSlot :: Slot
              }

-- ----------------------------------------------------------------------------
-- Querying symbol tables
-- ----------------------------------------------------------------------------

-- lookupVarRecord
-- Simply lookup the VarRecord for a given Variable's name.
lookupVarRecord :: VarSymTable -> String -> Maybe VarRecord
lookupVarRecord (VarSymTable m) name
  = m !? name

-- lookupProcRecord
-- Simply lookup the ProcRecord for a given Procedure's name.
lookupProcRecord :: ProcSymTable -> String -> Maybe ProcRecord
lookupProcRecord (ProcSymTable m) name
  = m !? name


-- ----------------------------------------------------------------------------
-- Building symbol tables
-- ----------------------------------------------------------------------------

constructProcSymTable :: [Proc] -> ProcSymTable
constructProcSymTable procs
  = ProcSymTable procMap
    where
      procMap = fromList procMappings
      procMappings = map constructProcMapping procs

constructProcMapping :: Proc -> (String, ProcRecord)
constructProcMapping (Proc _ (Id _ name) params decls _)
  = (name, record)
    where
      record = ProcRecord { procParams = params }

-- constructVarSymTable
-- Given lists of decls and params, generate slots for params and decls, and
-- then teturn a set of tuples from the params and decls to VarRecords, s.t.
-- each VarRecord stores the appropriate Slot.
constructVarSymTable :: [Param] -> [Decl] -> VarSymTable
constructVarSymTable params decls
  = VarSymTable symbolMap
    where
      symbolMap = fromList $ paramMappings ++ declMappings
      -- first allocate enough slots for all of the declared variables
      declStartSlots = declStartSlotsFrom (Slot 0) decls
      declMappings = zipWith constructDeclVarMapping decls declStartSlots
      -- then continue allocating remaining slots for params (which will only
      -- need a single slot each)
      -- NOTE: declStartSlots ends with the next available slot; start there
      firstParamStartSlot = last declStartSlots
      paramStartSlots = [firstParamStartSlot..]
      paramMappings = zipWith constructParamVarMapping params paramStartSlots


-- constructParamVarMapping
-- Take a Param and a slot and return a tuple with its id and a VarRecord
constructParamVarMapping :: Param -> Slot -> (String, VarRecord)
constructParamVarMapping (Param _ passby basetype (Id _ name)) slot
  = (name, record)
    where
      record = VarRecord { varShape = Dim0
                         , varType = basetype
                         , varPassBy = passby
                         , varStackSlot = slot
                         }


-- constructDeclVarMapping
-- Take a Decl and return a tuple with its id and a VarRecord
constructDeclVarMapping :: Decl -> Slot -> (String, VarRecord)
constructDeclVarMapping (Decl _ basetype (Id _ name) dim) slot
  = (name, record)
    where
      record = VarRecord { varShape = dim
                         , varType = basetype
                         , varPassBy = Val
                         , varStackSlot = slot
                         }





-- TODO: Maybe move some of these to semantic analysis module?

-- numSlots
-- Simply return the number of slots for a Variable Symbol Table
numSlots :: VarSymTable -> Int
numSlots (VarSymTable m)
  = sum $ map (numSlotsDim . varShape) (elems m)


-- declStartSlotsFrom
-- Determine appropriate starting slots for a series of declarations, starting
-- from a given slot. The resulting list contains n+1 slots where n is the
-- number of declarations: the last slot is the next available slot after the
-- slots for all the decls.
declStartSlotsFrom :: Slot -> [Decl] -> [Slot]
declStartSlotsFrom (Slot start) decls
  = map Slot $ scanl (+) start $ map numSlotsDecl decls


-- numSlotsDecl
-- Gets the number of slots required for a given declared variable.
numSlotsDecl :: Decl -> Int
numSlotsDecl (Decl _ _ _ dim)
  = numSlotsDim dim


-- numSlotsDim
-- Retrieves the size implied by the dimensionality
numSlotsDim :: Dim -> Int
numSlotsDim Dim0
  = 1
numSlotsDim (Dim1 n)
  = n
numSlotsDim (Dim2 n m)
  = n * m
