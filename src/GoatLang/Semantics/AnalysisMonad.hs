module GoatLang.Semantics.AnalysisMonad where

import Data.List (nub)
import Control.Monad.State

import Util.DiffList

-- We'll use a state monad to simplify semantic analysis
type SemanticAnalysis a
  = State SemanticAnalysisState a

data SemanticAnalysisState
  = SemanticAnalysisState { procSymTableStack :: [ProcSymTable]
                          , varSymTableStack :: [VarSymTable]
                          , semanticErrors :: DiffList SemanticError
                          }

data SemanticError
  = SemanticError Pos String
  | GlobalError String

semanticError :: SemanticError -> SemanticAnalysis
semanticError err
  = do
      state <- get
      put $ state {errors = errors `snoc` err}
