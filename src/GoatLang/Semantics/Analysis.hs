module GoatLang.Semantics.Analysis where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                       GOAT - Static semantic analysis
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

import Data.List (sort, group)

import GoatLang.AST
import GoatLang.Semantics.AAST
import GoatLang.Semantics.AnalysisMonad
import GoatLang.Semantics.SymbolTable
import GoatLang.Semantics.Error

import OzLang.Code

-- Summary of TODO items from throughout file:
--
-- When we do semantic analysis:
--
-- - genCodeExprInto assumes that all expressions are completely well-types
--   Semantic analysis will need to come in and actually provide that guarantee.
--
-- - Of course, we will want to change from using a recursive function to
--   calculate expression types to precomputing these types during semantic
--   analysis and embedding them within the Expression AST nodes themselves.
--
-- - Idea: Annotate the AST with additional 'float cast' nodes to avoid having
--   a separate case for every operation when the arguments might be an int and
--   a float. E.g. an annotated AST requiring a cast, such as for the expression
--   `4 + 0.2`, could look as follows:
--   ```
--   Add FloatType
--     (FloatCast (IntConst 4))
--     (FloatConst 0.2)
--   ```

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

aExpr :: Expr -> SemanticAnalysis AExpr

aExpr (ScalarExpr pos scalar)
  = fmap AScalarExpr (aScalar scalar)

aExpr (BoolConst pos bool)
  = return $ ABoolConst bool

aExpr (FloatConst pos float)
  = return $ AFloatConst float

aExpr (IntConst pos int)
  = return $ AIntConst int

-- unary negation (must be bool) or negative (must be non-bool)
aExpr (UnExpr pos op expr)
  = do
      aExpr' <- aExpr expr
      let (instr, resultType) = lookupUnInstr op (getExprType aExpr')
      let attrs = UnExprAttr { unExprInstr = instr
                             , unExprResultType = resultType
                             }
      return $ AUnExpr op aExpr' attrs

aExpr (BinExpr pos op lExpr rExpr)
  = do
      aLExpr <- aExpr lExpr
      aRExpr <- aExpr rExpr
      let (instr, aLExpr', aRExpr', resType) = lookupBinInstr op aLExpr aRExpr
      let attrs = BinExprAttr { binExprInstr = instr
                              , binExprResultType = resType
                              }
      return $ ABinExpr op aLExpr' aRExpr' attrs

aScalar (Single pos ident@(Id _ name))
  = do
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (single), etc.
      let attrs = SingleAttr { singlePassBy = varPassBy record
                             , singleStackSlot = varStackSlot record
                             , singleBaseType = varType record
                             }
      return $ ASingle ident attrs
aScalar (Array pos ident@(Id _ name) exprI)
  = do
      -- TODO: assert index is int type
      aExprI <- aExpr exprI
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (array), etc.
      let attrs = ArrayAttr { arrayStartSlot = varStackSlot record
                            , arrayBaseType = varType record
                            }
      return $ AArray ident aExprI attrs
aScalar (Matrix pos ident@(Id _ name) exprI exprJ)
  = do
      -- TODO: assert indices are both int type
      aExprI <- aExpr exprI
      aExprJ <- aExpr exprJ
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (matrix), etc.
      let (Dim2 _ rowWidth) = varShape record
      let attrs = MatrixAttr { matrixStartSlot = varStackSlot record
                             , matrixRowWidth = rowWidth
                             , matrixBaseType = varType record
                             }
      return $ AMatrix ident aExprI aExprJ attrs



getExprType :: AExpr -> BaseType
getExprType (AScalarExpr scalar)
  = getScalarType scalar
getExprType (ABoolConst _)
  = BoolType
getExprType (AFloatConst _)
  = FloatType
getExprType (AIntConst _)
  = IntType
getExprType (ABinExpr op lExpr rExpr attrs)
  = binExprResultType attrs
getExprType (AUnExpr op expr attrs)
  = unExprResultType attrs
getExprType (AFloatCast expr)
  = FloatType

getScalarType :: AScalar -> BaseType
getScalarType (ASingle _ attrs)
  = singleBaseType attrs
getScalarType (AArray _ _ attrs)
  = arrayBaseType attrs
getScalarType (AMatrix _ _ _ attrs)
  = matrixBaseType attrs



lookupReadBuiltin :: BaseType -> BuiltinFunc
lookupReadBuiltin BoolType
  = ReadBool
lookupReadBuiltin IntType
  = ReadInt
lookupReadBuiltin FloatType
  = ReadReal

lookupPrintBuiltin :: BaseType -> BuiltinFunc
lookupPrintBuiltin BoolType
  = PrintBool
lookupPrintBuiltin IntType
  = PrintInt
lookupPrintBuiltin FloatType
  = PrintReal



type UnInstruction
  = Reg -> Reg -> Instruction
type BinInstruction
  = Reg -> Reg -> Reg -> Instruction

lookupUnInstr :: UnOp -> BaseType -> (UnInstruction, BaseType)
lookupUnInstr Not BoolType
  = (NotInstr, BoolType)
lookupUnInstr Neg IntType
  = (NegIntInstr, IntType)
lookupUnInstr Neg FloatType
  = (NegRealInstr, FloatType)
-- TODO: consider other (erroneous) types

lookupBinInstr :: BinOp -> AExpr -> AExpr
                  -> (BinInstruction, AExpr, AExpr, BaseType)
lookupBinInstr op lExpr rExpr
  = (instr, lExpr', rExpr', resultType)
  where
    -- find the types of both operands
    lType = getExprType lExpr
    rType = getExprType rExpr

    -- look up the approprate instruction for this combination of types
    (instr, lExpr', rExpr') = case (lType, rType) of
        (BoolType, BoolType) -> (lookupOpBool op, lExpr, rExpr)
        (IntType, IntType) -> (lookupOpInt op, lExpr, rExpr)
        (FloatType, FloatType) -> (lookupOpReal op, lExpr, rExpr)
        (FloatType, IntType) -> (lookupOpIntOrReal op, lExpr, rExprReal)
        (IntType, FloatType) -> (lookupOpIntOrReal op, lExprReal, rExpr)
    -- TODO: do proper checking / handle error cases (bad op for types 
    -- e.g. Equ for Float and Int or bad type combo e.g. Float and Bool)

    -- cast, if necessary
    lExprReal = AFloatCast lExpr
    rExprReal = AFloatCast rExpr

    -- and determine the result type of this operation
    resultType 
      | arithmetic op = case (lType, rType) of
          (IntType, IntType) -> IntType
          otherwise -> FloatType
      | otherwise = BoolType
    arithmetic = (`elem` [Add, Sub, Mul, Div])

-- Look up the appropriate Oz Instruction for an int and a real argument
-- (NOTE: Equ and NEq are not allowed!)
lookupOpIntOrReal :: BinOp -> BinInstruction
lookupOpIntOrReal Add
  = AddRealInstr
lookupOpIntOrReal Sub
  = SubRealInstr
lookupOpIntOrReal Mul
  = MulRealInstr
lookupOpIntOrReal Div
  = DivRealInstr
lookupOpIntOrReal LTh
  = LThRealInstr
lookupOpIntOrReal LEq
  = LEqRealInstr
lookupOpIntOrReal GTh
  = GThRealInstr
lookupOpIntOrReal GEq
  = GEqRealInstr

-- Look up the appropriate Oz Instruction for two int arguments (arithmetic
-- and comparisons are allowed)
lookupOpInt :: BinOp -> BinInstruction
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
lookupOpBool :: BinOp -> BinInstruction
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
lookupOpReal :: BinOp -> BinInstruction
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
