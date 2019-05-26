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

import Control.Monad (when)
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
-- Low-level access to the components of the state (wrapping get and put)
-- ----------------------------------------------------------------------------

getFromState :: (SemanticAnalysisState -> a) -> SemanticAnalysis a
getFromState accessor
  = do
      state <- get
      return $ accessor state

setVarSymTableStack :: [VarSymTable] -> SemanticAnalysis ()
setVarSymTableStack newStack
  = do
      state <- get
      put $ state { varSymTableStack = newStack }

setProcSymTableStack :: [ProcSymTable] -> SemanticAnalysis ()
setProcSymTableStack newStack
  = do
      state <- get
      put $ state { procSymTableStack = newStack }

setSemanticErrors :: DiffList SemanticError -> SemanticAnalysis ()
setSemanticErrors newErrors
  = do
      state <- get
      put $ state { semanticErrors = newErrors }


-- ----------------------------------------------------------------------------
-- Reporting errors to the Semantic Analysis
-- ----------------------------------------------------------------------------

-- semanticError
-- Append a semantic error to the analysis' list of semantic errors.
semanticError :: SemanticError -> SemanticAnalysis ()
semanticError err
  = do
      errs <- getFromState semanticErrors
      setSemanticErrors $ errs `snoc` err

-- assert
-- Append a semantic error to the analysis' list of semantic errors unless
-- a condition is satisfied. Reflect common usage of semanticError.
assert :: Bool -> SemanticError -> SemanticAnalysis ()
assert cond err
  = when (not cond) $ semanticError err


-- ----------------------------------------------------------------------------
-- Manipulating the procedure symbol table
-- ----------------------------------------------------------------------------

pushProcSymTable :: SemanticAnalysis ()
pushProcSymTable
  = do
      currentStack <- getFromState procSymTableStack
      setProcSymTableStack $ emptyProcSymTable : currentStack

popProcSymTable :: SemanticAnalysis ProcSymTable
popProcSymTable
  = do
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState procSymTableStack
      let (topProcSymTable:restOfStack) = symTableStack
      setProcSymTableStack restOfStack
      return topProcSymTable

addProcMapping :: String -> ProcRecord -> SemanticAnalysis ()
addProcMapping name newRecord
  = do
      -- extract the top symbol table from the stack
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState procSymTableStack
      let (topTable:restOfStack) = symTableStack

      -- check to make sure a procedure of the same name has not been defined
      -- already (within the current scope i.e. top proc symbol table)
      let maybeExistingRecord = lookupProcRecord topTable name
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert new proc record since name is unique (thus far)
          let newTopTable = insertProcRecord name newRecord topTable
          setProcSymTableStack $ newTopTable : restOfStack
        Just existingRecord -> semanticError $
          RepeatedDefinitionError
            (procDefnPos newRecord)
            (procDefnPos existingRecord)
            ("repeat definition of procedure " ++ show name)


-- ----------------------------------------------------------------------------
-- Manipulating the variable symbol table
-- ----------------------------------------------------------------------------

pushVarSymTable :: SemanticAnalysis ()
pushVarSymTable
  = do
      currentStack <- getFromState varSymTableStack
      setVarSymTableStack $ emptyVarSymTable : currentStack

popVarSymTable :: SemanticAnalysis VarSymTable
popVarSymTable
  = do
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState varSymTableStack
      let (topVarSymTable:restOfStack) = symTableStack
      setVarSymTableStack restOfStack
      return topVarSymTable

-- Add a VarRecord to the VarSymTable at the top of the stack
addVarMapping :: String -> VarRecord -> SemanticAnalysis ()
addVarMapping name newRecord
  = do
      -- extract the top symbol table from the stack
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState varSymTableStack
      let (topTable:restOfStack) = symTableStack

      -- check to make sure a local variable of the same name has not been defnd
      -- already IN THE CURRENT SCOPE (in the top symbol table on the stack)
      let maybeExistingRecord = lookupVarRecord topTable name
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert record since name is unique (thus far)
          let newTopTable = insertVarRecord name newRecord topTable
          setVarSymTableStack $ newTopTable : restOfStack
        Just existingRecord -> semanticError $
          RepeatedDefinitionError
            (varDefnPos newRecord)
            (varDefnPos existingRecord)
            ("repeat definition of local variable " ++ show name)


getRequiredFrameSize :: SemanticAnalysis FrameSize
getRequiredFrameSize
  = do
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState varSymTableStack
      let (VarSymTable _ numSlots : _) = symTableStack
      return $ FrameSize numSlots

-- Allocate the given number of new slots to the symbol table at the top of
-- the VarSymTableStack.
allocateStackSlots :: Int -> SemanticAnalysis Slot
allocateStackSlots numSlots
  = do
      -- NOTE: Runtime error if stack is empty:
      symTableStack <- getFromState varSymTableStack
      let (topTable:restOfStack) = symTableStack
      let (VarSymTable _ currentNumSlots) = topTable
      let newTopTable = allocateSlots numSlots topTable
      setVarSymTableStack $ newTopTable : restOfStack
      return $ Slot currentNumSlots


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
