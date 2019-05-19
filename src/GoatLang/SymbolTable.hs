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

import Data.Map.Strict (Map, fromList, size, (!))

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
  = size m

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
                          , procVarSymTable = constructVarSymTable params decls
                          }
      frameSize = length params + length decls


-- constructVarSymTable
-- Given lists of decls and params, generate slots for params and decls, and
-- then teturn a set of tuples from the params and decls to VarRecords, s.t.
-- each VarRecord stores the appropriate Slot.
constructVarSymTable :: [Param] -> [Decl] -> VarSymTable
constructVarSymTable params decls
  = VarSymTable symbolMap
    where
      symbolMap = fromList $ paramMappings ++ declMappings
      paramMappings = zipWith constructParamVarMapping params (map Slot [0..])
      declMappings = zipWith constructDeclVarMapping decls (map Slot [n..])
      n = length params

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
