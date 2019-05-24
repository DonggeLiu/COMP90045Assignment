module GoatLang.Semantics.Error where

import GoatLang.AST

-- (TODO: Probably move types to GoatLang.Semantics.Errors along with
-- formatting functions for them?)
data SemanticError
  = SemanticError Pos String
  | GlobalError String
  | RepeatedDefinitionError Pos [Pos] String
  deriving (Show, Eq)

