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

import GoatLang.Error
import GoatLang.Syntax.AST
import GoatLang.Syntax.Printer
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

-- getFromState
-- Generic accessor function that takes a SemanticAnalysisState's record
-- accessor function and applies it to the SemanticAnalysis monad's state,
-- retrieving the corresponding component from the state.
getFromState :: (SemanticAnalysisState -> a) -> SemanticAnalysis a
getFromState accessor
  = do
      state <- get
      return $ accessor state

-- setVarSymTableStack
-- Replace the analysis monad's current variable symbol table stack.
setVarSymTableStack :: [VarSymTable] -> SemanticAnalysis ()
setVarSymTableStack newStack
  = do
      state <- get
      put $ state { varSymTableStack = newStack }

-- setProcSymTableStack
-- Replace the analysis monad's current procedure symbol table stack.
setProcSymTableStack :: [ProcSymTable] -> SemanticAnalysis ()
setProcSymTableStack newStack
  = do
      state <- get
      put $ state { procSymTableStack = newStack }

-- setSemanticErrors
-- Replace the analysis monad's current (difference) list of semantic errors.
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
-- a condition is satisfied, reflecting common usage of semanticError.
assert :: Bool -> SemanticError -> SemanticAnalysis ()
assert cond err
  = when (not cond) $ semanticError err


-- ----------------------------------------------------------------------------
-- Manipulating the procedure symbol table
-- ----------------------------------------------------------------------------

-- pushProcSymTable
-- Add a new procedure symbol table atop the procedure symbol table stack.
pushProcSymTable :: ProcSymTable -> SemanticAnalysis ()
pushProcSymTable newProcSymTable
  = do
      currentStack <- getFromState procSymTableStack
      setProcSymTableStack $ newProcSymTable : currentStack

-- pushBlankProcSymTable
-- Add a new, empty procedure symbol table to the procedure symbol table stack.
pushBlankProcSymTable :: SemanticAnalysis ()
pushBlankProcSymTable
  = pushProcSymTable emptyProcSymTable

-- popProcSymTable
-- Remove and return the top procedure symbol table from the procedure symbol 
-- table stack.
-- Throw a sensible runtime error in the case where the procedure symbol table 
-- stack is empty.
popProcSymTable :: SemanticAnalysis ProcSymTable
popProcSymTable
  = do
      symTableStack <- getFromState procSymTableStack
      case symTableStack of
        [] -> error "attempt to modify top of empty procSymTableStack"
        topProcSymTable:restOfStack -> do
          setProcSymTableStack restOfStack
          return topProcSymTable

-- peekProcSymTable
-- Return (but do not remove) the top procedure symbol table from the procedure 
-- symbol table stack.
-- Throw a sensible runtime error in the case where the procedure symbol table 
-- stack is empty.
peekProcSymTable :: SemanticAnalysis ProcSymTable
peekProcSymTable
  = do
      symTableStack <- getFromState procSymTableStack
      case symTableStack of
        [] -> error "attempt to access top of empty procSymTableStack"
        topProcSymTable:restOfStack -> return topProcSymTable

-- addProcMapping
-- Add a ProcRecord to the top procedure symbol table.
-- Log a semantic error if a procedure record with the same Id already exists
-- in the TOP TABLE (but it's fine if the clashing procedure record is in one
-- of the lower tables in the stack).
-- RUNTIME ERROR if performed when the stack is empty.
addProcMapping :: Id -> ProcRecord -> SemanticAnalysis ()
addProcMapping ident newRecord
  = do
      -- extract the top symbol table from the stack
      -- RUNTIME ERROR if performed when the stack is empty
      topTable <- peekProcSymTable

      -- check to make sure a procedure of the same name has not been defined
      -- already (within the current scope i.e. top proc symbol table)
      let maybeExistingRecord = lookupProcRecord topTable ident
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert new proc record since name is unique (thus far)
          let newTopTable = insertProcRecord ident newRecord topTable
          -- replace the old symbol table with this modified symbol table
          popProcSymTable
          pushProcSymTable newTopTable
        Just existingRecord -> semanticError $
          RepeatedDefinitionError
            (procDefnPos newRecord)
            (procDefnPos existingRecord)
            ("repeat declaration of procedure: " ++ prettify ident)


-- ----------------------------------------------------------------------------
-- Manipulating the variable symbol table
-- ----------------------------------------------------------------------------

-- pushVarSymTable
-- Add a new variable symbol table atop the variable symbol table stack.
pushVarSymTable :: VarSymTable -> SemanticAnalysis ()
pushVarSymTable newVarSymTable
  = do
      currentStack <- getFromState varSymTableStack
      setVarSymTableStack $ newVarSymTable : currentStack

-- pushBlankVarSymTable
-- Add a new, empty variable symbol table to the variable symbol table stack.
pushBlankVarSymTable :: SemanticAnalysis ()
pushBlankVarSymTable
  = pushVarSymTable emptyVarSymTable

-- popVarSymTable
-- Remove and return the top variable symbol table from the variable symbol 
-- table stack.
-- Throw a sensible runtime error in the case where the variable symbol table 
-- stack is empty.
popVarSymTable :: SemanticAnalysis VarSymTable
popVarSymTable
  = do
      symTableStack <- getFromState varSymTableStack
      case symTableStack of
        [] -> error "attempt to modify top of empty varSymTableStack"
        topVarSymTable:restOfStack -> do
          setVarSymTableStack restOfStack
          return topVarSymTable

-- peekVarSymTable
-- Return (but do not remove) the top variable symbol table from the variable 
-- symbol table stack.
-- Throw a sensible runtime error in the case where the variable symbol table 
-- stack is empty.
peekVarSymTable :: SemanticAnalysis VarSymTable
peekVarSymTable
  = do
      symTableStack <- getFromState varSymTableStack
      case symTableStack of
        [] -> error "attempt to access top of empty varSymTableStack"
        topVarSymTable:restOfStack -> return topVarSymTable

-- allocateStackSlots
-- Mark the requested number of (runtime) stack slots as allocated for the
-- variable symbol table on top of the (compile time variable symbol table)
-- stack; return the start slot of the allocated block of slots.
-- RUNTIME ERROR if performed when the symbol table stack is empty.
allocateStackSlots :: Int -> SemanticAnalysis Slot
allocateStackSlots numSlots
  = do
      -- extract the top symbol table from the stack
      -- RUNTIME ERROR if performed when the symbol table stack is empty
      topTable <- popVarSymTable
      -- find the next available slot
      let (VarSymTable _ currentNumSlots) = topTable
      -- modify the symbol table to note that slots have been allocated, and
      -- restore it to the stack
      let newTopTable = allocateSlots numSlots topTable
      pushVarSymTable newTopTable
      -- return the starting slot for the caller
      return $ Slot currentNumSlots

-- addVarMapping
-- Add a VarRecord to the top variable symbol table.
-- Log a semantic error if a variable record with the same Id already exists
-- in the TOP TABLE (but it's fine if the clashing variable record is in one
-- of the lower tables in the stack).
-- RUNTIME ERROR if performed when the stack is empty.
addVarMapping :: Id -> VarRecord -> SemanticAnalysis ()
addVarMapping ident newRecord
  = do
      -- extract the top symbol table from the stack
      -- RUNTIME ERROR if performed when the stack is empty
      topTable <- peekVarSymTable

      -- check to make sure a local variable of the same name has not been defnd
      -- already IN THE CURRENT SCOPE (in the top symbol table on the stack)
      let maybeExistingRecord = lookupVarRecord topTable ident
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert record since name is unique (thus far)
          let newTopTable = insertVarRecord ident newRecord topTable
          -- replace the old symbol table with this modified symbol table
          popVarSymTable
          pushVarSymTable newTopTable
        Just existingRecord -> do
          let origTypeSpec = varTypeSpec existingRecord
          let dupTypeSpec = varTypeSpec newRecord
          let message = case (origTypeSpec, dupTypeSpec) of
                (DeclSpec, DeclSpec) -> "repeat declaration of local variable: "
                (ParamSpec _,ParamSpec _) -> "repeat declaration of parameter: "
                _ -> "variable declared with same identifier as parameter: "
          semanticError $ RepeatedDefinitionError
            (varDefnPos newRecord)
            (varDefnPos existingRecord)
            (message ++ prettify ident)

-- getRequiredFrameSize
-- Access the number of stack slots required to store the variables in the
-- variable symbol table atop the symbol table stack.
-- RUNTIME ERROR if performed when the stack is empty.
getRequiredFrameSize :: SemanticAnalysis FrameSize
getRequiredFrameSize
  = do
      topTable <- peekVarSymTable
      let (VarSymTable _ numSlots) = topTable
      return $ FrameSize numSlots


-- ----------------------------------------------------------------------------
-- Querying the stacks of symbol tables
-- ----------------------------------------------------------------------------

-- lookupProc
-- Query the current procedure symbol table stack for a proc record matching the
-- given Id, if it exists, scanning the symbol tables in stack order (i.e. most
-- recently pushed table first).
lookupProc :: Id -> SemanticAnalysis (Maybe ProcRecord)
lookupProc ident
  = do
      symTable <- getFromState procSymTableStack
      return $ lookupProcStack symTable ident
    where
      -- lookupProcStack
      -- Helper function to walk down a stack looking for a match, to implement
      -- the above function.
      lookupProcStack :: [ProcSymTable] -> Id -> Maybe ProcRecord
      lookupProcStack [] ident
        = Nothing
      lookupProcStack (table:stack) ident
        = case (lookupProcRecord table ident) of
            Nothing -> lookupProcStack stack ident
            justRecord -> justRecord

-- lookupVar
-- Query the current variable symbol table stack for a var record matching the
-- given Id, if it exists, scanning the symbol tables in stack order (i.e. most
-- recently pushed table first).
lookupVar :: Id -> SemanticAnalysis (Maybe VarRecord)
lookupVar ident
  = do
      symTable <- getFromState varSymTableStack
      return $ lookupVarStack symTable ident
    where
      -- lookupVarStack
      -- Helper function to walk down a stack looking for a match, to implement
      -- the above function.
      lookupVarStack :: [VarSymTable] -> Id -> Maybe VarRecord
      lookupVarStack [] ident
        = Nothing
      lookupVarStack (table:stack) ident
        = case (lookupVarRecord table ident) of
            Nothing -> lookupVarStack stack ident
            justRecord -> justRecord
