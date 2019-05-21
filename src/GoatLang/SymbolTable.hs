module GoatLang.SymbolTable where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - Symbol Table
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

import Data.Map.Strict (Map, fromList, size, (!), elems)

import GoatLang.AST
import GoatLang.OzCode

data ProcSymTable
  = ProcSymTable (Map Id ProcRecord)

data VarSymTable
  = VarSymTable (Map Id VarRecord)

data ProcRecord
  = ProcRecord { procFrameSize :: FrameSize
               , procParams :: [Param]
               , procVarSymTable :: VarSymTable
               }

data VarRecord
  = VarRecord { varShape :: Dim
              , varType :: BaseType
              , varPassBy :: PassBy
              , varStackSlot :: Slot
              }

-- numSlots
-- Simply return the number of slots for a Variable Symbol Table
numSlots :: VarSymTable -> Int
numSlots (VarSymTable m)
  = sum $ map (numSlotsDim . varShape) (elems m)

-- lookupVarRecord
-- Simply lookup the VarRecord for a given Variable's Id.
lookupVarRecord :: VarSymTable -> Id -> VarRecord
lookupVarRecord (VarSymTable m) ident
  = m ! ident

lookupProcRecord :: ProcSymTable -> Id -> ProcRecord
lookupProcRecord (ProcSymTable m) ident
  = m ! ident

constructProcSymTable :: [Proc] -> ProcSymTable
constructProcSymTable procs
  = ProcSymTable procMap
    where
      procMap = fromList procMappings
      procMappings = map constructProcMapping procs

constructProcMapping :: Proc -> (Id, ProcRecord)
constructProcMapping (Proc ident params decls _)
  = (ident, record)
    where
      record = ProcRecord { procFrameSize = FrameSize frameSize
                          , procParams = params
                          , procVarSymTable = varSymTable
                          }
      varSymTable = constructVarSymTable params decls
      frameSize = numSlots varSymTable


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
constructParamVarMapping :: Param -> Slot -> (Id, VarRecord)
constructParamVarMapping (Param passby basetype ident) slot
  = (ident, record)
    where
      record = VarRecord { varShape = Dim0
                         , varType = basetype
                         , varPassBy = passby
                         , varStackSlot = slot
                         }


-- constructDeclVarMapping
-- Take a Decl and return a tuple with its id and a VarRecord
constructDeclVarMapping :: Decl -> Slot -> (Id, VarRecord)
constructDeclVarMapping (Decl basetype ident dim) slot 
  = (ident, record)
    where
      record = VarRecord { varShape = dim
                         , varType = basetype
                         , varPassBy = Val
                         , varStackSlot = slot
                         }


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
numSlotsDecl (Decl _ _ dim)
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
