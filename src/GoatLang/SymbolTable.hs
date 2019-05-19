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
               , procVarSymTable :: VarSymTable
               }

data VarRecord
  = VarRecord { varShape :: Dim
              , varType :: BaseType
              , varPassBy :: PassBy
              , varStackSlot :: Slot
              }

numSlots :: VarSymTable -> Int
numSlots (VarSymTable m)
  = size m

lookupVarRecord :: VarSymTable -> Id -> VarRecord
lookupVarRecord (VarSymTable m) ident
  = m ! ident

constructVarSymTable :: [Param] -> [Decl] -> VarSymTable
constructVarSymTable params decls
  = VarSymTable symbolMap
    where
      symbolMap = fromList $ paramMappings ++ declMappings
      paramMappings = zipWith constructParamVarMapping params (map Slot [0..])
      declMappings = zipWith constructDeclVarMapping decls (map Slot [n..])
      n = length params

constructParamVarMapping :: Param -> Slot -> (Id, VarRecord)
constructParamVarMapping (Param passby basetype ident) slot
  = (ident, record)
    where
      record = VarRecord { varShape = Dim0
                         , varType = basetype
                         , varPassBy = passby
                         , varStackSlot = slot
                         }

constructDeclVarMapping :: Decl -> Slot -> (Id, VarRecord)
constructDeclVarMapping (Decl basetype ident dim) slot
  = (ident, record)
    where
      record = VarRecord { varShape = dim
                         , varType = basetype
                         , varPassBy = Val
                         , varStackSlot = slot
                         }
