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

import GoatLang.Syntax.AST
import GoatLang.Semantics.AAST
import GoatLang.Semantics.AnalysisMonad
import GoatLang.Semantics.SymbolTable
import GoatLang.Semantics.Error

import OzLang.Code

-- Summary of TODO items from throughout file:
--
-- TODO:
-- 

-- analyseFullProgram
-- Top level function: use the analysers defined below to convert a Goat Program
-- (an AST) into an annotated Goat Program (an AAST).
analyseFullProgram :: GoatProgram -> Either [SemanticError] AGoatProgram
analyseFullProgram goatProgram
  = analyse (analyseGoatProgram goatProgram)


-- ----------------------------------------------------------------------------
-- Static analysers for various program parts
-- ----------------------------------------------------------------------------

analyseGoatProgram :: GoatProgram -> SemanticAnalysis AGoatProgram
analyseGoatProgram (GoatProgram procs)
  = do
      -- construct the global procedure table in a first pass over the program:
      pushProcSymTable
      mapM_ defineProc procs

      -- there must be a procedure of arity 0 called main:
      assertMainProc procs

      -- now, with this procedure symbol table, analyse the contents of each
      -- procedure definition
      aProcs <- mapM analyseProc procs
      
      -- we're done with the procedure symbol table (NOTE: there is only ever
      -- at most a single procedure table, but we use a stack to keep the
      -- analysis program general and clean, and would allow easy extensions to
      -- nested procedure definitions)
      popProcSymTable

      return $ AGoatProgram aProcs

defineProc :: Proc -> SemanticAnalysis ()
defineProc (Proc pos ident@(Id _ name) params _ _)
  = do
      let newRecord = ProcRecord { procParams = params
                                 , procDefnPos = pos
                                 }
      -- check to make sure a procedure of the same name has not been defined
      -- already (NOTE: will check ALL proc symbol tables; if we extend to allow
      -- nested procedure definitions then this may not be intended behaviour)
      maybeExistingRecord <- lookupProc name
      case maybeExistingRecord of
        Nothing -> return ()
        Just existingRecord -> semanticError $
          RepeatedDefinitionError 
            (procDefnPos newRecord)
            (procDefnPos existingRecord)
            ("repeat definition of procedure " ++ show name)
      -- define the procedure and continue to analyse the program
      -- (error recovery: just let the second definition override the first)
      addProcMapping name newRecord

assertMainProc :: [Proc] -> SemanticAnalysis ()
assertMainProc procs
  = do
      maybeMainRecord <- lookupProc "main"
      case maybeMainRecord of
        -- there must be a procedure called main:
        Nothing -> semanticError $ GlobalError $
          "missing main procedure definition"
        -- and, if it exists, it must have arity 0 (no parameters)
        Just record -> 
          if (null $ procParams record)
            then return ()
            else semanticError $ SemanticError
              (procDefnPos record)
              "main procedure must have no parameters"
        

analyseProc :: Proc -> SemanticAnalysis AProc
analyseProc (Proc pos ident params decls stmts)
  = do
      -- compute the procedure's main attribute: required frame size
      let varSymTable = constructVarSymTable params decls
      let attrs = ProcAttr { procFrameSize = FrameSize $ numSlots varSymTable }
      
      -- set the environment and analyse the procedure's components
      pushVarSymTable varSymTable
      aParams <- mapM analyseParam params
      aDecls <- mapM analyseDecl decls
      aStmts <- mapM analyseStmt stmts
      popVarSymTable

      -- done!
      return $ AProc ident aParams aDecls aStmts attrs


analyseParam :: Param -> SemanticAnalysis AParam
analyseParam (Param pos passBy baseType ident@(Id _ name))
  = do
      -- TODO: Switch back to lookup by Id?
      -- TODO: Make sure this is actually in the table of course...
      Just varRecord <- lookupVar name
      let stackSlot = varStackSlot varRecord
      let attrs = ParamAttr { paramStackSlot = stackSlot }
      return $ AParam passBy baseType ident attrs


analyseDecl :: Decl -> SemanticAnalysis ADecl
analyseDecl (Decl pos baseType ident@(Id _ name) dim)
  = do
      Just varRecord <- lookupVar name
      let startSlot = varStackSlot varRecord
      let allSlots = take (numSlotsDim dim) [startSlot..]
      let attrs = DeclAttr { declStackSlots = allSlots }
      return $ ADecl baseType ident dim attrs


analyseStmt :: Stmt -> SemanticAnalysis AStmt
analyseStmt (Asg pos scalar expr)
  = do
      aScalar <- analyseScalar scalar
      aExpr <- analyseExpr expr
      -- we may need to add a cast to the expression in case of float := int
      -- TODO: otherwise we could perform some type checking here.
      aExpr' <- case (scalarType aScalar, exprType aExpr) of
        (IntType, IntType) -> return aExpr
        (BoolType, BoolType) -> return aExpr
        (FloatType, IntType) -> return $ AFloatCast aExpr
        (FloatType, FloatType) -> return aExpr
      return $ AAsg aScalar aExpr'

analyseStmt (Read pos scalar)
  = do
      aScalar <- analyseScalar scalar
      let (Id _ name) = scalarIdent scalar
      Just record <- lookupVar name
      let builtin = lookupReadBuiltin (varType record)
      let attrs = ReadAttr { readBuiltin = builtin }
      return $ ARead aScalar attrs

analyseStmt (WriteExpr pos expr)
  = do
      aExpr <- analyseExpr expr
      let builtin = lookupPrintBuiltin (exprType aExpr)
      let attrs = WriteExprAttr { writeExprBuiltin = builtin }
      return $ AWriteExpr aExpr attrs

analyseStmt (WriteString pos string)
  = return $ AWriteString string

analyseStmt (Call pos ident@(Id _ name) args)
  = do
      aArgs <- mapM analyseExpr args
      -- TODO: Check that procedure is defined
      Just record <- lookupProc name
      -- TODO: Check arity matches up
      -- TODO: Check types of arguments match params
      -- TODO: Check only scalars in reference param positions.
      let passBys = [ passBy | (Param _ passBy _ _) <- procParams record ]
      let attrs = CallAttr { callPassBys = passBys }

      -- in the case of pass by value, introduce float casts if necessary:
      let aArgs' = zipWith castArg (procParams record) aArgs
      return $ ACall ident aArgs' attrs
    where
      castArg :: Param -> AExpr -> AExpr
      castArg (Param _ Val FloatType _) arg
        = case (exprType arg) of
            IntType -> AFloatCast arg
            FloatType -> arg
            -- BoolType -> -- ERROR!
      castArg _ arg
        = arg


analyseStmt (If pos cond thenStmts)
  = do
      aCond <- analyseExpr cond
      -- TODO: check boolean
      aThenStmts <- mapM analyseStmt thenStmts
      return $ AIf aCond aThenStmts

analyseStmt (IfElse pos cond thenStmts elseStmts)
  = do
      aCond <- analyseExpr cond
      -- TODO: check boolean
      aThenStmts <- mapM analyseStmt thenStmts
      aElseStmts <- mapM analyseStmt elseStmts
      return $ AIfElse aCond aThenStmts aElseStmts

analyseStmt (While pos cond doStmts)
  = do
      aCond <- analyseExpr cond
      -- TODO: check boolean
      aDoStmts <- mapM analyseStmt doStmts
      return $ AWhile aCond aDoStmts


analyseExpr :: Expr -> SemanticAnalysis AExpr

analyseExpr (ScalarExpr pos scalar)
  = fmap AScalarExpr (analyseScalar scalar)

analyseExpr (BoolConst pos bool)
  = return $ ABoolConst bool

analyseExpr (FloatConst pos float)
  = return $ AFloatConst float

analyseExpr (IntConst pos int)
  = return $ AIntConst int

-- unary negation (must be bool) or negative (must be non-bool)
analyseExpr (UnExpr pos op expr)
  = do
      aExpr <- analyseExpr expr
      let (instr, resultType) = lookupUnInstr op (exprType aExpr)
      let attrs = UnExprAttr { unExprInstr = instr
                             , unExprResultType = resultType
                             }
      return $ AUnExpr op aExpr attrs

analyseExpr (BinExpr pos op lExpr rExpr)
  = do
      aLExpr <- analyseExpr lExpr
      aRExpr <- analyseExpr rExpr
      let (instr, aLExpr', aRExpr', resType) = lookupBinInstr op aLExpr aRExpr
      let attrs = BinExprAttr { binExprInstr = instr
                              , binExprResultType = resType
                              }
      return $ ABinExpr op aLExpr' aRExpr' attrs


analyseScalar :: Scalar -> SemanticAnalysis AScalar
analyseScalar (Single pos ident@(Id _ name))
  = do
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (single), etc.
      let attrs = SingleAttr { singlePassBy = varPassBy record
                             , singleStackSlot = varStackSlot record
                             , singleBaseType = varType record
                             }
      return $ ASingle ident attrs
analyseScalar (Array pos ident@(Id _ name) exprI)
  = do
      -- TODO: assert index is int type
      aExprI <- analyseExpr exprI
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (array), etc.
      let attrs = ArrayAttr { arrayStartSlot = varStackSlot record
                            , arrayBaseType = varType record
                            }
      return $ AArray ident aExprI attrs
analyseScalar (Matrix pos ident@(Id _ name) exprI exprJ)
  = do
      -- TODO: assert indices are both int type
      aExprI <- analyseExpr exprI
      aExprJ <- analyseExpr exprJ
      Just record <- lookupVar name
      -- TODO: assert variable exists, correct shape (matrix), etc.
      let (Dim2 _ rowWidth) = varShape record
      let attrs = MatrixAttr { matrixStartSlot = varStackSlot record
                             , matrixRowWidth = rowWidth
                             , matrixBaseType = varType record
                             }
      return $ AMatrix ident aExprI aExprJ attrs

-- exprType
-- Infer the result type of an annotated expression. Not recursive, because we
-- already annotated each expression with enough information to know its result
-- type! Woo!
exprType :: AExpr -> BaseType
exprType (AScalarExpr scalar)
  = scalarType scalar
exprType (ABoolConst _)
  = BoolType
exprType (AFloatConst _)
  = FloatType
exprType (AIntConst _)
  = IntType
exprType (ABinExpr op lExpr rExpr attrs)
  = binExprResultType attrs
exprType (AUnExpr op expr attrs)
  = unExprResultType attrs
exprType (AFloatCast expr)
  = FloatType

-- scalarType
-- Infer the base type of an annotated scalar reference. It doesn't involve the
-- symbol table because we have already annotated each scalar with its base
-- type! Woo hoo!
scalarType :: AScalar -> BaseType
scalarType (ASingle _ attrs)
  = singleBaseType attrs
scalarType (AArray _ _ attrs)
  = arrayBaseType attrs
scalarType (AMatrix _ _ _ attrs)
  = matrixBaseType attrs


-- lookupReadBuiltin
-- Given a target type, what Oz builtin function should we use to Read one?
lookupReadBuiltin :: BaseType -> BuiltinFunc
lookupReadBuiltin BoolType
  = ReadBool
lookupReadBuiltin IntType
  = ReadInt
lookupReadBuiltin FloatType
  = ReadReal

-- lookupPrintBuiltin
-- Given a value type, what Oz builtin function should we use to Print one?
lookupPrintBuiltin :: BaseType -> BuiltinFunc
lookupPrintBuiltin BoolType
  = PrintBool
lookupPrintBuiltin IntType
  = PrintInt
lookupPrintBuiltin FloatType
  = PrintReal


-- Shortcuts for certain Oz instruction names which are really data constructor
-- functions requiring registers to be supplied before they become actual
-- instructions.
-- Instructions for unary operations (neg_int, neg_real, not) require two
-- registers: a target and a source.
type UnInstruction
  = Reg -> Reg -> Instruction
-- Instructions for unary operations (add_int, add_real, etc.) require three
-- registers: a target and two soruces (one for each operand).
type BinInstruction
  = Reg -> Reg -> Reg -> Instruction

-- lookupUnInstr
-- This is a lookup table to find the instructions corresponding to a particular
-- unary operation and argument type. It returns the instruction constructor and
-- also the BaseType representing the result of the instruction.
lookupUnInstr :: UnOp -> BaseType -> (UnInstruction, BaseType)
lookupUnInstr Not BoolType
  = (NotInstr, BoolType)
lookupUnInstr Neg IntType
  = (NegIntInstr, IntType)
lookupUnInstr Neg FloatType
  = (NegRealInstr, FloatType)
-- TODO: consider other (erroneous) types. They should return some kind of error
-- here (BEFORE runtime haha)

-- lookupBinInstr
-- This is a lookup table to find the instructions corresponding to a particular
-- binary operation and argument types. It returns the instruction constructor
-- and also the BaseType representing the result of the instruction. Since the
-- operation may require adding a cast to either input expression (such as for
-- `1 + 1.0` which should become `float(1) + 1.0`) we also return alternative
-- argument expressions (these will be the same as the input expressions, or
-- they will be the expression wrapped in an extra cast).
lookupBinInstr :: BinOp -> AExpr -> AExpr
                  -> (BinInstruction, AExpr, AExpr, BaseType)
lookupBinInstr op lExpr rExpr
  = (instr, lExpr', rExpr', resultType)
  where
    -- find the types of both operands
    lType = exprType lExpr
    rType = exprType rExpr

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

-- lookupOpIntOrReal
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

-- lookupOpInt
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

-- lookupOpReal
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

-- lookupOpBool
-- Look up the appropriate value for two boolean arguments (only comparisons
-- and logical relations are allowed)
-- Logical operations will actually be skipped at runtime for specific code
-- that creates short-circuit evaluation. For now, we use dummy instructions
-- `AndInstr` and `OrInstr` so that these expressions can still be annotated.
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
lookupOpBool And
  = AndInstr
lookupOpBool Or
  = OrInstr
