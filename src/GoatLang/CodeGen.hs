module GoatLang.CodeGen where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - Oz Code Generator
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

import GoatLang.AST

type Register = Int

data InstrTree
  = InstrList [InstrTree]
  | InstrLeaf Instruction

data Instruction
  = IntConstInstr Register Int
  | FloatConstInstr Register Float
  | AddIntInstr Register Register Register

-- ABinExpr attr Add (AIntConst attr 1) (AIntConst attr 2)  -- AExpr -> 105-112          <- Chosen for now
genCodeExprInto :: Register -> Expr -> InstrTree
genCodeExprInto register (AIntConst attr i)
  = InstrLeaf $ IntConstInstr register i
genCodeExprInto register (AFloatConst attr f)
  = InstrLeaf $ FloatConstInstr register f
genCodeExprInto register (ABoolConst attr b)
  = case b of
      True -> InstrLeaf $ IntConstInstr register 1
      False -> InstrLeaf $ IntConstInstr register 0
genCodeExprInto register (ABinExpr attr Add l r)
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , instr register register (register + 1)
    ]
    where
      instr = lookupInstrBinOp Add l r
lookupInstrBinOp :: BinOp -> AExpr -> AExpr
  -> (Register -> Register -> Register -> Instruction)
lookupInstrBinOp

genCodeExprInto register (ABinExpr t Add (FloatConst l) (IntConst r))
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , AddIntInstr register register (register + 1)
    ]
