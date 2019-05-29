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

-- getProcSymTableStack1
-- Helper function to access the stack of procedure symbol tables, careful to
-- throw a sensible runtime error in the case where the procedure symbol table 
-- stack is empty.
getProcSymTableStack1 :: SemanticAnalysis [ProcSymTable]
getProcSymTableStack1
  = do
      symTableStack <- getFromState procSymTableStack
      case symTableStack of
        [] -> error "accessing empty procSymTableStack"
        stack -> return stack

-- getVarSymTableStack1
-- Helper function to access the stack of variable symbol tables, careful to
-- throw a sensible runtime error in the case where the variable symbol table 
-- stack is empty.
getVarSymTableStack1 :: SemanticAnalysis [VarSymTable]
getVarSymTableStack1
  = do
      symTableStack <- getFromState varSymTableStack
      case symTableStack of
        [] -> error "accessing empty varSymTableStack"
        stack -> return stack


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
-- Add a new procedure symbol table to the procedure symbol table stack
pushProcSymTable :: SemanticAnalysis ()
pushProcSymTable
  = do
      currentStack <- getFromState procSymTableStack
      setProcSymTableStack $ emptyProcSymTable : currentStack

-- popProcSymTable
-- Remove and return the top procedure symbol table from the procedure symbol 
-- table stack. RUNTIME ERROR if performed when stack is empty.
popProcSymTable :: SemanticAnalysis ProcSymTable
popProcSymTable
  = do
      symTableStack <- getProcSymTableStack1
      let (topProcSymTable:restOfStack) = symTableStack
      setProcSymTableStack restOfStack
      return topProcSymTable

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
      symTableStack <- getProcSymTableStack1
      let (topTable:restOfStack) = symTableStack

      -- check to make sure a procedure of the same name has not been defined
      -- already (within the current scope i.e. top proc symbol table)
      let maybeExistingRecord = lookupProcRecord topTable ident
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert new proc record since name is unique (thus far)
          let newTopTable = insertProcRecord ident newRecord topTable
          setProcSymTableStack $ newTopTable : restOfStack
        Just existingRecord -> semanticError $
          RepeatedDefinitionError
            (procDefnPos newRecord)
            (procDefnPos existingRecord)
            ("repeat declaration of procedure: " ++ prettify ident)


-- ----------------------------------------------------------------------------
-- Manipulating the variable symbol table
-- ----------------------------------------------------------------------------

-- pushVarSymTable
-- Add a new variable symbol table to the variable symbol table stack
pushVarSymTable :: SemanticAnalysis ()
pushVarSymTable
  = do
      currentStack <- getFromState varSymTableStack
      setVarSymTableStack $ emptyVarSymTable : currentStack

-- popVarSymTable
-- Remove and return the top variable symbol table from the variable symbol 
-- table stack. RUNTIME ERROR if performed when stack is empty.
popVarSymTable :: SemanticAnalysis VarSymTable
popVarSymTable
  = do
      symTableStack <- getVarSymTableStack1
      let (topVarSymTable:restOfStack) = symTableStack
      setVarSymTableStack restOfStack
      return topVarSymTable

-- allocateStackSlots
-- Mark the requested number of (runtime) stack slots as allocated for the
-- variable symbol table on top of the (compile time variable symbol table)
-- stack; return the start slot of the allocated block of slots.
-- RUNTIME ERROR if performed when the symbol table stack is empty.
allocateStackSlots :: Int -> SemanticAnalysis Slot
allocateStackSlots numSlots
  = do
      symTableStack <- getVarSymTableStack1
      let (topTable:restOfStack) = symTableStack
      let (VarSymTable _ currentNumSlots) = topTable
      let newTopTable = allocateSlots numSlots topTable
      setVarSymTableStack $ newTopTable : restOfStack
      return $ Slot currentNumSlots

-- addVarMapping
-- Add a VarRecord to the top variable symbol table.
-- Logs a semantic error if a variable record with the same Id already exists
-- in the TOP TABLE (but it's fine if the clashing variable record is in one
-- of the lower tables in the stack).
-- RUNTIME ERROR if performed when the stack is empty.
addVarMapping :: Id -> VarRecord -> SemanticAnalysis ()
addVarMapping ident newRecord
  = do
      -- extract the top symbol table from the stack
      symTableStack <- getVarSymTableStack1
      let (topTable:restOfStack) = symTableStack

      -- check to make sure a local variable of the same name has not been defnd
      -- already IN THE CURRENT SCOPE (in the top symbol table on the stack)
      let maybeExistingRecord = lookupVarRecord topTable ident
      case maybeExistingRecord of
        Nothing -> do
          -- safe to insert record since name is unique (thus far)
          let newTopTable = insertVarRecord ident newRecord topTable
          setVarSymTableStack $ newTopTable : restOfStack
        Just existingRecord -> do
          let origTypeSpec = varTypeSpec existingRecord
          let dupTypeSpec = varTypeSpec newRecord
          let message = case (origTypeSpec, dupTypeSpec) of
                (DeclSpec, DeclSpec) -> "repeat declaration of local variable: "
                (ParamSpec _, _) -> "repeat declaration of parameter: "
                _ -> "variable declared with same identifier as parameter: "
          semanticError $ RepeatedDefinitionError
            (varDefnPos newRecord)
            (varDefnPos existingRecord)
            (message ++ prettify ident)

-- getRequiredFrameSize
-- Access the number of stack slots required to store the variables in the
-- variable symbol table atop the symbol table stack.
getRequiredFrameSize :: SemanticAnalysis FrameSize
getRequiredFrameSize
  = do
      symTableStack <- getVarSymTableStack1
      let (VarSymTable _ numSlots : _) = symTableStack
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
      state <- get
      return $ lookupProcStack (procSymTableStack state) ident
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
      state <- get
      return $ lookupVarStack (varSymTableStack state) ident
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
