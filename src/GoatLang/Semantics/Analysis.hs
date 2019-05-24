module GoatLang.Semantics.Analysis where

import Data.List (sort, group)

import GoatLang.AST
import GoatLang.Semantics.AAST
import GoatLang.Semantics.AnalysisMonad
import GoatLang.Semantics.SymbolTable
import GoatLang.Semantics.Error

import OzLang.Code

-- TODO:
-- restructure project along the lines of:
-- import GoatLang.Syntax.AST
-- import GoatLang.Syntax.Parser
-- import GoatLang.Syntax.Printer
-- import GoatLang.Syntax.Tokens
-- import GoatLang.Semantics.(...)


-- analyseFullProgram
-- Top level function: use the analysers defined below to convert a Goat Program
-- (an AST) into an annotated Goat Program (an AAST).
analyseFullProgram :: GoatProgram -> Either [SemanticError] AGoatProgram
analyseFullProgram goatProgram
  = analyse (aGoatProgram goatProgram)


-- ----------------------------------------------------------------------------
-- Static analysers for various program parts
-- ----------------------------------------------------------------------------

aGoatProgram :: GoatProgram -> SemanticAnalysis AGoatProgram
aGoatProgram (GoatProgram procs)
  = do
      -- assertNoDupNames [ident | (Proc _ ident _ _ _) <- procs]
      -- TODO: Maybe we could construct the symbol table monadicaly in an
      -- initial pass for easier error detection and reporting.
      pushProcSymTable (constructProcSymTable procs)
      -- assertMainProc procs -- one procedure should be called main (arity 0)
      aProcs <- mapM aProc procs
      popProcSymTable
      return $ AGoatProgram aProcs

-- TODO: Sticking to the plan, do error checking later.
-- 
-- assertNoDupNames :: [Id] -> SemanticAnalysis ()
-- assertNoDupNames ids
--   = do
--       let dupNames = filter (\ns -> length ns > 1) $ group $ sort ids
--       mapM_ reportDupNames dupNames

-- reportDupNames :: [Id] -> SemanticAnalysis ()
-- reportDupNames ids
--   = do
--       let (Id origPos name) = head ids
--       let repeats = [pos | (Id pos _) <- tail ids]
--       semanticError $ RepeatedDefinitionError origPos repeats
--         "Repeated definition(s) of identifier " ++ show name


-- assertMainProc :: [Proc] -> SemanticAnalysis ()
-- assertMainProc procs
--   = do
--       maybeMain <- lookupProc "main"
--       case maybeMain of
--         Just record -> if null (procParams record) then return ()
--           else semanticError $ GlobalError "main must not take arguments"
--         Nothing -> semanticError $ GlobalError "missing main procedure"

aProc :: Proc -> SemanticAnalysis AProc
aProc (Proc pos ident params decls stmts)
  = do
      -- compute the procedure's main attribute: required frame size
      let varSymTable = constructVarSymTable params decls
      let attrs = ProcAttr { procFrameSize = FrameSize $ numSlots varSymTable }
      
      -- set the environment and analyse the procedure's components
      pushVarSymTable varSymTable
      aParams <- mapM aParam params
      aDecls <- mapM aDecl decls
      aStmts <- mapM aStmt stmts
      popVarSymTable

      -- done!
      return $ AProc ident aParams aDecls aStmts attrs


aParam :: Param -> SemanticAnalysis AParam
aParam (Param pos passBy baseType ident@(Id _ name))
  = do
      -- TODO: Switch back to lookup by Id?
      -- TODO: Make sure this is actually in the table of course...
      Just varRecord <- lookupVar name
      let stackSlot = varStackSlot varRecord
      let attrs = ParamAttr { paramStackSlot = stackSlot }
      return $ AParam passBy baseType ident attrs

aDecl :: Decl -> SemanticAnalysis ADecl
aDecl (Decl pos baseType ident@(Id _ name) dim)
  = do
      Just varRecord <- lookupVar name
      let startSlot = varStackSlot varRecord
      let allSlots = take (numSlotsDim dim) [startSlot..]
      let attrs = DeclAttr { declStackSlots = allSlots }
      return $ ADecl baseType ident dim attrs

aStmt :: Stmt -> SemanticAnalysis AStmt
aStmt (Asg pos scalar expr)
  = do
      -- TODO: Type checking
      aScalar' <- aScalar scalar
      aExpr' <- aExpr expr
      return $ AAsg aScalar' aExpr'

aStmt (Read pos scalar)
  = do
      aScalar' <- aScalar scalar
      -- TODO: must be declared
      let (Id _ name) = scalarIdent scalar
      Just record <- lookupVar name
      let builtin = lookupReadBuiltin (varType record)
      let attrs = ReadAttr { readBuiltin = builtin }
      return $ ARead aScalar' attrs

aStmt (WriteExpr pos expr)
  = do
      aExpr' <- aExpr expr
      let builtin = lookupPrintBuiltin (getExprType aExpr')
      let attrs = WriteExprAttr { writeExprBuiltin = builtin }
      return $ AWriteExpr aExpr' attrs

aStmt (WriteString pos string)
  = return $ AWriteString string

aStmt (Call pos ident@(Id _ name) args)
  = do
      aArgs <- mapM aExpr args
      -- TODO: Check that procedure is defined
      Just record <- lookupProc name
      -- TODO: Check arity matches up
      -- TODO: Check types of arguments match params
      -- TODO: Check only scalars in reference param positions.
      let passBys = [ passBy | (Param _ passBy _ _) <- procParams record ]
      let attrs = CallAttr { callPassBys = passBys }
      return $ ACall ident aArgs attrs

aStmt (If pos cond thenStmts)
  = do
      aCond <- aExpr cond
      -- TODO: check boolean
      aThenStmts <- mapM aStmt thenStmts
      return $ AIf aCond aThenStmts

aStmt (IfElse pos cond thenStmts elseStmts)
  = do
      aCond <- aExpr cond
      -- TODO: check boolean
      aThenStmts <- mapM aStmt thenStmts
      aElseStmts <- mapM aStmt elseStmts
      return $ AIfElse aCond aThenStmts aElseStmts

aStmt (While pos cond doStmts)
  = do
      aCond <- aExpr cond
      -- TODO: check boolean
      aDoStmts <- mapM aStmt doStmts
      return $ AWhile aCond aDoStmts





-- ----------------------------------------------------------------------------
-- Mockups
-- ----------------------------------------------------------------------------

aExpr _
  = return $ AFloatCast (AIntConst 42)
aScalar (Single pos ident)
  = return $ ASingle ident $ SingleAttr { singlePassBy = Val
                                        , singleStackSlot = Slot 0
                                        }

getExprType :: AExpr -> BaseType
getExprType (AScalarExpr scalar)
  = error "not yet implemented" -- TODO
getExprType (ABoolConst _)
  = BoolType
getExprType (AFloatConst _)
  = FloatType
getExprType (AIntConst _)
  = IntType
getExprType (ABinExpr op lExpr rExpr attrs)
  = error "not yet implemented" -- TODO
getExprType (AUnExpr op expr attrs)
  = error "not yet implemented" -- TODO
getExprType (AFloatCast expr)
  = FloatType

lookupReadBuiltin BoolType
  = ReadBool
lookupReadBuiltin IntType
  = ReadInt
lookupReadBuiltin FloatType
  = ReadReal

lookupPrintBuiltin BoolType
  = PrintBool
lookupPrintBuiltin IntType
  = PrintInt
lookupPrintBuiltin FloatType
  = PrintReal

-- getExprType :: VarSymTable -> Expr -> BaseType
-- getExprType _ (BoolConst _ _)
--   = BoolType
-- getExprType _ (FloatConst _ _)
--   = FloatType
-- getExprType _ (IntConst _ _)
--   = IntType
-- getExprType varSymTable (BinExpr _ operator left right)
--   | arithmetic operator = case types of
--       (IntType, IntType) -> IntType
--       otherwise -> FloatType
--   | otherwise = BoolType
--   where
--     arithmetic = (`elem` [Add, Sub, Mul, Div])
--     types = (getExprType varSymTable left, getExprType varSymTable right)

-- getExprType varSymTable (UnExpr _ operator operand)
--   = getExprType varSymTable operand
-- getExprType varSymTable (ScalarExpr _ scalar)
--   = getScalarType varSymTable scalar

-- getScalarType :: VarSymTable -> Scalar -> BaseType
-- getScalarType varSymTable (Single ident _)
--   = varType $ lookupVarRecord varSymTable ident
-- getScalarType varSymTable (Array ident iExpr _)
--   = varType $ lookupVarRecord varSymTable ident
-- getScalarType varSymTable (Matrix ident iExpr jExpr _)
--   = varType $ lookupVarRecord varSymTable ident




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

-- -- For semantic analysis:
-- -- case exprType of
-- --   IntType -> instr $ (lookupOpInt op) reg reg (succ reg)
-- --   BoolType -> instr $ (lookupOpBool op) reg reg (succ reg)
-- --   FloatType -> instr $ (lookupOpReal op) reg reg (succ reg)
-- -- Look up the appropriate Oz Instruction for two int arguments (arithmetic
-- -- and comparisons are allowed)
-- lookupOpInt :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
-- lookupOpInt Add
--   = AddIntInstr
-- lookupOpInt Sub
--   = SubIntInstr
-- lookupOpInt Mul
--   = MulIntInstr
-- lookupOpInt Div
--   = DivIntInstr
-- lookupOpInt Equ
--   = EquIntInstr
-- lookupOpInt NEq
--   = NEqIntInstr
-- lookupOpInt LTh
--   = LThIntInstr
-- lookupOpInt LEq
--   = LEqIntInstr
-- lookupOpInt GTh
--   = GThIntInstr
-- lookupOpInt GEq
--   = GEqIntInstr

-- -- Look up the appropriate value for two boolean arguments (only comparisons
-- -- are allowed)
-- lookupOpBool :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
-- lookupOpBool Equ
--   = EquIntInstr
-- lookupOpBool NEq
--   = NEqIntInstr
-- lookupOpBool LTh
--   = LThIntInstr
-- lookupOpBool LEq
--   = LEqIntInstr
-- lookupOpBool GTh
--   = GThIntInstr
-- lookupOpBool GEq
--   = GEqIntInstr

-- -- Look up the appropriate Oz Instruction for two real arguments:
-- -- (arithmetic and comparisons are allowed)
-- lookupOpReal :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
-- lookupOpReal Add
--   = AddRealInstr
-- lookupOpReal Sub
--   = SubRealInstr
-- lookupOpReal Mul
--   = MulRealInstr
-- lookupOpReal Div
--   = DivRealInstr
-- lookupOpReal Equ
--   = EquRealInstr
-- lookupOpReal NEq
--   = NEqRealInstr
-- lookupOpReal LTh
--   = LThRealInstr
-- lookupOpReal LEq
--   = LEqRealInstr
-- lookupOpReal GTh
--   = GThRealInstr
-- lookupOpReal GEq
--   = GEqRealInstr
