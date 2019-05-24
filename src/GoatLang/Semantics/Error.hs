module GoatLang.Semantics.Error where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                 GOAT - Representing semantic errors
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

import GoatLang.AST (Pos)

data SemanticError
  = SemanticError Pos String
  | GlobalError String
  | RepeatedDefinitionError Pos [Pos] String
  deriving (Show, Eq)

