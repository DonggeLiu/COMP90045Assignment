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

import GoatLang.Syntax.AST (Pos)

data SemanticError
  = SemanticError Pos String
  | RepeatedDefinitionError Pos Pos String
  | GlobalError String

instance Show SemanticError where
  show (SemanticError pos string)
    = string ++ " at " ++ show pos
  show (GlobalError string)
    = string
  show (RepeatedDefinitionError pos oldPos string)
    = string ++ " at: " ++ show pos
      ++ "\norginal definition at: " ++ show oldPos
