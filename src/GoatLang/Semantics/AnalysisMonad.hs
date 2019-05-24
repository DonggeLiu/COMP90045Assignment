module GoatLang.Semantics.AnalysisMonad where


import Control.Monad.State

import Util.DiffList

import GoatLang.AST (Pos)
import GoatLang.Semantics.Error
import GoatLang.Semantics.SymbolTable


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

pushVarSymTable :: VarSymTable -> SemanticAnalysis ()
pushVarSymTable newSymTable
  = do
      state <- get
      put $ state { varSymTableStack = newSymTable : varSymTableStack state }

pushProcSymTable :: ProcSymTable -> SemanticAnalysis ()
pushProcSymTable newSymTable
  = do
      state <- get
      put $ state { procSymTableStack = newSymTable : procSymTableStack state }

popVarSymTable :: SemanticAnalysis VarSymTable
popVarSymTable
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topVarSymTable:restOfStack) = varSymTableStack state
      put $ state { varSymTableStack = restOfStack }
      return topVarSymTable

popProcSymTable :: SemanticAnalysis ProcSymTable
popProcSymTable
  = do
      state <- get
      -- NOTE: Runtime error if stack is empty:
      let (topProcSymTable:restOfStack) = procSymTableStack state
      put $ state { procSymTableStack = restOfStack }
      return topProcSymTable


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

