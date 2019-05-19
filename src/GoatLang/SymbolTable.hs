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
-- Updated for Arrays and Matrices
numSlots :: VarSymTable -> Int
numSlots (VarSymTable m)
  = foldl (\x varRec -> x + (getSize varRec)) 0 (elems m)
  where
    getSize = dimSize . varShape

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
      declMappings = zipWith constructDeclVarMapping decls (getSlots decls n)
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

-- getSlots
-- Get a Slot with its appropriate slot number for each declaration.
getSlots decls n
  = map Slot $ scanl1 (+) $ (:) 0 $ map getNumSlots decls

-- getNumSlots
-- Gets the number of slots required for a given declared variable.
getNumSlots :: Decl -> Int
getNumSlots (Decl _ _ dim) = dimSize dim

-- dimSize
-- Retrieves the size implied by the dimensionality
dimSize :: Dim -> Int
dimSize Dim0
  = 1
dimSize (Dim1 n)
  = n
dimSize (Dim2 n m)
  = n * m


-- Previously (uglier) used the following functions. More efficient b/c it
-- only requires one scan, but the code is uglier.

-- -- getSlots
-- -- Gets a list of starting slots for the given declarations
-- getSlots :: [Decl] -> Int -> [Slot]
-- getSlots ((Decl _ _ dim):rest) n
--   = (Slot n) : (getSlots rest $ n + (getNumSlots dim))
-- getSlots [] _
--   = []

-- -- getNumSlots
-- -- Given a Dim, get the number of slots required to store the Scalar's contents
-- getNumSlots :: Dim -> Int
-- getNumSlots Dim0
--   = 1
-- getNumSlots (Dim1 n)
--   = n
-- getNumSlots (Dim2 n m)
--   = n * m
