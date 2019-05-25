module GoatLang.Semantics.AnalysisMonad where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--           GOAT - Monadic helpers for static semantic analysis
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

import GoatLang.Semantics.Error
import GoatLang.Semantics.SymbolTable

import OzLang.Code


-- ----------------------------------------------------------------------------
-- Semantic Analysis State Monad
-- ----------------------------------------------------------------------------

type SemanticAnalysis a
  = State SemanticAnalysisState a

data SemanticAnalysisState
  = SemanticAnalysisState { procSymTableStack :: [ProcSymTable]
                          , varSymTableStack :: [VarSymTable]
                          , semanticErrors :: DiffList SemanticError
                          }


-- ----------------------------------------------------------------------------
-- Running the Monad
-- ----------------------------------------------------------------------------

-- analyse
-- Top level function: use a semantic analysis monad to analyse a program and
-- produce a result, or a list fo semantic errors.
analyse :: SemanticAnalysis a -> Either [SemanticError] a
analyse programAnalyser
  = if null errors then Right result else Left errors
  where
    startState = SemanticAnalysisState { procSymTableStack = []
                                       , varSymTableStack = []
                                       , semanticErrors = mempty
                                       }
    -- run the analysis monad an extract any errors
    (result, endState) = runState programAnalyser startState
    errors = listify $ semanticErrors endState


-- ----------------------------------------------------------------------------
-- Reporting errors to the Semantic Analysis
-- ----------------------------------------------------------------------------

-- semanticError
-- Append a semantic error to the analysis' list of semantic errors.
semanticError :: SemanticError -> SemanticAnalysis ()
semanticError err
  = do
      state <- get
      put $ state {semanticErrors = (semanticErrors state) `snoc` err}


-- ----------------------------------------------------------------------------
-- Manipulating the stacks of symbol tables
-- ----------------------------------------------------------------------------

pushProcSymTable :: SemanticAnalysis ()
pushProcSymTable
  = do
      state <- get
      let newSymTable = emptyProcSymTable
      let currentStack = procSymTableStack state
      put $ state { procSymTableStack = newSymTable : currentStack }

addProcMapping :: String -> ProcRecord -> SemanticAnalysis ()
addProcMapping name newRecord
  = do
      -- extract the top symbol table from the stack
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topTable:restOfStack) = procSymTableStack state
      
      -- check to make sure a procedure of the same name has not been defined
      -- already (within the current scope i.e. top proc symbol table)
      let maybeExistingRecord = lookupProcRecord topTable name
      case maybeExistingRecord of
        Nothing -> return ()
        Just existingRecord -> semanticError $
          RepeatedDefinitionError 
            (procDefnPos newRecord)
            (procDefnPos existingRecord)
            ("repeat definition of procedure " ++ show name)

      -- define the procedure anyway and continue to analyse the program
      let newTopTable = insertProcRecord name newRecord topTable
      put $ state { procSymTableStack = newTopTable:restOfStack }

popProcSymTable :: SemanticAnalysis ProcSymTable
popProcSymTable
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topProcSymTable:restOfStack) = procSymTableStack state
      put $ state { procSymTableStack = restOfStack }
      return topProcSymTable



pushVarSymTable :: SemanticAnalysis ()
pushVarSymTable
  = do
      state <- get
      let newSymTable = emptyVarSymTable
      let currentStack = varSymTableStack state
      put $ state { varSymTableStack = newSymTable : currentStack }

addVarMapping :: String -> VarRecord -> SemanticAnalysis ()
addVarMapping name newRecord
  = do
      -- extract the top symbol table from the stack
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topTable:restOfStack) = varSymTableStack state
      
      -- check to make sure a local variable of the same name has not been defnd
      -- already IN THE CURRENT SCOPE (in the top symbol table on the stack)
      let maybeExistingRecord = lookupVarRecord topTable name
      case maybeExistingRecord of
        Nothing -> return ()
        Just existingRecord -> semanticError $
          RepeatedDefinitionError 
            (varDefnPos newRecord)
            (varDefnPos existingRecord)
            ("repeat definition of local variable " ++ show name)
      
      -- in any case, we will continue with this definition for the remainder
      -- of this scope
      let newTopTable = insertVarRecord name newRecord topTable
      put $ state { varSymTableStack = newTopTable:restOfStack }

getRequiredFrameSize :: SemanticAnalysis FrameSize
getRequiredFrameSize
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topTable:_) = varSymTableStack state
      let (VarSymTable _ numSlots) = topTable
      return $ FrameSize numSlots

allocateStackSlots :: Int -> SemanticAnalysis Slot
allocateStackSlots numSlots
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topTable:restOfStack) = varSymTableStack state
      let (VarSymTable _ currentNumSlots) = topTable
      let newTopTable = allocateSlots numSlots topTable
      put $ state { varSymTableStack = newTopTable:restOfStack }
      return $ Slot currentNumSlots

popVarSymTable :: SemanticAnalysis VarSymTable
popVarSymTable
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topVarSymTable:restOfStack) = varSymTableStack state
      put $ state { varSymTableStack = restOfStack }
      return topVarSymTable



-- ----------------------------------------------------------------------------
-- Querying the stacks of symbol tables
-- ----------------------------------------------------------------------------

lookupProc :: String -> SemanticAnalysis (Maybe ProcRecord)
lookupProc name
  = do
      state <- get
      return $ lookupProcStack (procSymTableStack state) name

lookupProcStack :: [ProcSymTable] -> String -> Maybe ProcRecord
lookupProcStack [] name
  = Nothing
lookupProcStack (table:stack) name
  = case (lookupProcRecord table name) of
      Nothing -> lookupProcStack stack name
      justRecord -> justRecord


lookupVar :: String -> SemanticAnalysis (Maybe VarRecord)
lookupVar name
  = do
      state <- get
      return $ lookupVarStack (varSymTableStack state) name

lookupVarStack :: [VarSymTable] -> String -> Maybe VarRecord
lookupVarStack [] name
  = Nothing
lookupVarStack (table:stack) name
  = case (lookupVarRecord table name) of
      Nothing -> lookupVarStack stack name
      justRecord -> justRecord

