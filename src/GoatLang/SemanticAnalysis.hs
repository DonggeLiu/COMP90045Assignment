module GoatLang.SemanticAnalysis
where

import Control.Monad.State

import Util.DiffList

import GoatLang.AST
import GoatLang.AAST

-- TODO:
-- import GoatLang.Syntax.AST
-- import GoatLang.Syntax.Parser
-- import GoatLang.Syntax.Printer
-- import GoatLang.Syntax.Tokens
-- import GoatLang.Semantics.AAST
-- import GoatLang.Semantics.Errors
-- import GoatLang.Semantics.Static
-- import GoatLang.Semantics.Dynamic
-- import GoatLang.Semantics.CodeGen
-- import OzLang.Code -- Slots, Reg Instr
-- import OzLang.Print -- writing Oz programs





-- We'll use a state monad to simplify construction of Oz programs
type SemanticAnalysis a
  = State SemanticAnalysisState a

data SemanticAnalysisState
  = SemanticAnalysisState { procSymTableStack :: [ProcSymTable]
                          , varSymTableStack :: [VarSymTable]
                          , semanticErrors :: DiffList SemanticError
                          }

data SemanticError
  = SemanticError Pos String
  = GlobalError String

semanticError :: SemanticError -> SemanticAnalysis
semanticError error
  = do
      state <- get
      put $ state {errors = errors `snoc` error}





-- analyse
analyse :: GoatProgram -> Either [SemanticError] AGoatProgram
analyse goatProgram
  = if null errors then annotatedGoatProgram else errors
  where
    startState = SemanticAnalysisState { procSymTableStack = []
                                       , varSymTableStack = []
                                       , semanticErrors = mempty
                                       }
    
    -- run the analysis monad an extract any errors
    (aGoatProgram, endState) = runState (aGoatProgram goatProgram) startState
    errors = semanticErrors endState


aGoatProgram :: GoatProgram -> SemanticAnalysis AGoatProgram
aGoatProgram (GoatProgram procs)
  = do
      -- assertMainProc procs
      -- assertNoDupNames [ident | (Proc _ ident _ _ _) <- procs]
      pushProcSymTable (constructProcSymTable procs)
      aProcs <- mapM aProc procs
      popProcSymTable
      return $ AGoatProgram aProc

-- assertMainProc :: SemanticAnalysis ()
-- assertMainProc
  -- = do
      -- lookupProc "main"
      case result of
        Just _ -> return ()
        Nothing -> semanticError $ GlobalError "missing main procedure"



















getExprType :: VarSymTable -> Expr -> BaseType
getExprType _ (BoolConst _ _)
  = BoolType
getExprType _ (FloatConst _ _)
  = FloatType
getExprType _ (IntConst _ _)
  = IntType
getExprType varSymTable (BinExpr _ operator left right)
  | arithmetic operator = case types of
      (IntType, IntType) -> IntType
      otherwise -> FloatType
  | otherwise = BoolType
  where
    arithmetic = (`elem` [Add, Sub, Mul, Div])
    types = (getExprType varSymTable left, getExprType varSymTable right)

getExprType varSymTable (UnExpr _ operator operand)
  = getExprType varSymTable operand
getExprType varSymTable (ScalarExpr _ scalar)
  = getScalarType varSymTable scalar

getScalarType :: VarSymTable -> Scalar -> BaseType
getScalarType varSymTable (Single ident _)
  = varType $ lookupVarRecord varSymTable ident
getScalarType varSymTable (Array ident iExpr _)
  = varType $ lookupVarRecord varSymTable ident
getScalarType varSymTable (Matrix ident iExpr jExpr _)
  = varType $ lookupVarRecord varSymTable ident




-- -- genCodeBinOp
-- -- Action to generate code for an arbitrary binary operation from two registers
-- -- into a third destination register, given the known types of the values in the
-- -- two
-- genCodeBinOp :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType -> CodeGen ()

-- -- If both
-- genCodeBinOp destReg lReg rReg op IntType IntType
--   = instr $ (lookupOpInt op) destReg lReg rReg

-- genCodeBinOp destReg lReg rReg op BoolType BoolType
--   = instr $ (lookupOpBool op) destReg lReg rReg

-- genCodeBinOp destReg lReg rReg op FloatType FloatType
--   = instr $ (lookupOpReal op) destReg lReg rReg

-- -- Finally, maybe we have one int value and one real value:
-- genCodeBinOp destReg lReg rReg op lType rType
--   = do
--       -- ensure both arguments are formatted as reals
--       realify lReg lType
--       realify rReg rType
--       instr $ (lookupOpIntOrReal op) destReg lReg rReg

-- -- realify
-- -- Action to generate code to cast an integer value to a real, if necessary
-- -- (this is a noop if the BaseType is FloatType).
-- realify :: Reg -> BaseType -> CodeGen ()
-- realify _ FloatType
--   = return ()
-- realify register IntType
--   = instr $ IntToRealInstr register register
-- -- TODO (semantic analysis):
-- -- Annotate the AST with additional 'float cast' nodes to avoid the need for
-- -- this case. E.g. an annotated AST requiring a cast, such as for the
-- -- expression `4 + 0.2`, could look like this:
-- -- ```
-- -- Add FloatType
-- --   (FloatCast (IntConst 4))
-- --   (FloatConst 0.2)
-- -- ```
-- --
-- -- Then all we'd need would be:
-- --
-- -- ```
-- -- genCodeExprInto register (FloatCase expr)
-- --   = do
-- --       genCodeExprInto register expr
-- --       instr $ IntToRealInstr register register
-- -- ```

-- -- Look up the appropriate Oz Instruction for an int and a real argument
-- -- (NOTE: Equ and NEq are not allowed!)
-- lookupOpIntOrReal :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
-- lookupOpIntOrReal Add
--   = AddRealInstr
-- lookupOpIntOrReal Sub
--   = SubRealInstr
-- lookupOpIntOrReal Mul
--   = MulRealInstr
-- lookupOpIntOrReal Div
--   = DivRealInstr
-- lookupOpIntOrReal LTh
--   = LThRealInstr
-- lookupOpIntOrReal LEq
--   = LEqRealInstr
-- lookupOpIntOrReal GTh
--   = GThRealInstr
-- lookupOpIntOrReal GEq
--   = GEqRealInstr


-- -- There are only two cases for unary operations:
-- -- Logical `Not`:
-- genCodeExprInto reg (AUnExpr Not expr _)
--   = do
--       genCodeExprInto reg expr
--       instr $ NotInstr reg reg

-- -- And arithmetic `Neg` (which could be applied to either a real value or an
-- -- int value):
-- genCodeExprInto reg (AUnExpr _ expr attrs)
--   = do
--       genCodeExprInto reg expr
--       instr $ (unExprInstr attrs) reg reg

-- for semantic analysis:
-- let allSlots = take (declNumSlots dim) [startSlot..]

-- For semantic analysis:
-- case exprType of 
--   IntType -> instr $ (lookupOpInt op) reg reg (succ reg)
--   BoolType -> instr $ (lookupOpBool op) reg reg (succ reg)
--   FloatType -> instr $ (lookupOpReal op) reg reg (succ reg)
-- Look up the appropriate Oz Instruction for two int arguments (arithmetic
-- and comparisons are allowed)
lookupOpInt :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
lookupOpInt Add
  = AddIntInstr
lookupOpInt Sub
  = SubIntInstr
lookupOpInt Mul
  = MulIntInstr
lookupOpInt Div
  = DivIntInstr
lookupOpInt Equ
  = EquIntInstr
lookupOpInt NEq
  = NEqIntInstr
lookupOpInt LTh
  = LThIntInstr
lookupOpInt LEq
  = LEqIntInstr
lookupOpInt GTh
  = GThIntInstr
lookupOpInt GEq
  = GEqIntInstr

-- Look up the appropriate value for two boolean arguments (only comparisons
-- are allowed)
lookupOpBool :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
lookupOpBool Equ
  = EquIntInstr
lookupOpBool NEq
  = NEqIntInstr
lookupOpBool LTh
  = LThIntInstr
lookupOpBool LEq
  = LEqIntInstr
lookupOpBool GTh
  = GThIntInstr
lookupOpBool GEq
  = GEqIntInstr

-- Look up the appropriate Oz Instruction for two real arguments:
-- (arithmetic and comparisons are allowed)
lookupOpReal :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
lookupOpReal Add
  = AddRealInstr
lookupOpReal Sub
  = SubRealInstr
lookupOpReal Mul
  = MulRealInstr
lookupOpReal Div
  = DivRealInstr
lookupOpReal Equ
  = EquRealInstr
lookupOpReal NEq
  = NEqRealInstr
lookupOpReal LTh
  = LThRealInstr
lookupOpReal LEq
  = LEqRealInstr
lookupOpReal GTh
  = GThRealInstr
lookupOpReal GEq
  = GEqRealInstr
