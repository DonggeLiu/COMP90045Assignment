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

import Control.Monad (when)

import GoatLang.Syntax.AST
import GoatLang.Syntax.Printer (format, prettify)

import GoatLang.Semantics.AAST
import GoatLang.Semantics.AnalysisMonad
import GoatLang.Semantics.SymbolTable
import GoatLang.Semantics.Error

import OzLang.Code


-- analyseFullProgram
-- Top level function: use the analysers defined below to convert a Goat Program
-- (an AST) into an annotated Goat Program (an AAST).
analyseFullProgram :: GoatProgram -> Either [SemanticError] AGoatProgram
analyseFullProgram goatProgram
  = analyse (analyseGoatProgram goatProgram)


-- ----------------------------------------------------------------------------
-- Static analysers for various program parts
-- ----------------------------------------------------------------------------

-- analyseGoatProgram
-- Complete a semantic analysis of a full goat program, ensuring that it meets
-- all static semantic requirements.
analyseGoatProgram :: GoatProgram -> SemanticAnalysis AGoatProgram
analyseGoatProgram (GoatProgram procs)
  = do
      -- construct the global procedure table in a first pass over the program's
      -- list of procedures
      pushProcSymTable
      mapM_ defineProc procs

      -- check: there must be a procedure with arity 0 (no params) called main
      assertMainProc

      -- now, with this procedure symbol table, analyse the contents of each
      -- procedure definition
      aProcs <- mapM analyseProc procs

      -- we're done with the procedure symbol table (NOTE: there is only ever
      -- at most a single procedure table, but we use a stack to keep the
      -- analysis program general and clean, and would allow easy extensions to
      -- nested procedure definitions)
      popProcSymTable

      -- analysis complete!
      return $ AGoatProgram aProcs

-- defineProc
-- Add a new procedure record to the currently active procedure table so that
-- future procedure calls can find out about its parameters.
-- Detects if a procedure of the same name has already been defined, reporting
-- an error and overwriting the previous definition.
defineProc :: Proc -> SemanticAnalysis ()
defineProc (Proc pos ident params _ _)
  = do
      let newRecord = ProcRecord { procParams = params
                                 , procDefnPos = pos
                                 }
      -- define this procedure within the curent scope, checking for repeated
      -- definitions
      addProcMapping ident newRecord

-- assertMainProc
-- Enforce the static requirement that a procedure called 'main' exists in the
-- program. This should be called after 'defineProc' ahs been used to set up
-- the procedure symbol table.
assertMainProc :: SemanticAnalysis ()
assertMainProc
  = do
      maybeMainRecord <- lookupProc (Id NoPos "main")
      case maybeMainRecord of
        -- there must be a procedure called main:
        Nothing -> semanticError $ GlobalError $
          "missing main procedure definition"
        -- and, if it exists, it must have arity 0 (no parameters)
        Just mainRecord -> assert (null $ procParams mainRecord) $
          SemanticError
            (procDefnPos mainRecord)
            "main procedure must have no parameters"

analyseProc :: Proc -> SemanticAnalysis AProc
analyseProc proc@(Proc pos ident params decls stmts)
  = do
      -- construct the local variable symbol table in a first pass over the
      -- params and declarations:
      pushVarSymTable
      aParams <- mapM declareAnalyseParam params
      aDecls <- mapM declareAnalyseDecl decls

      -- after allocating space for all params and decls on the stack (above),
      -- prepare the annotations for this procedure (the main attribute
      -- is the frame size)
      frameSize <- getRequiredFrameSize
      let attrs = ProcAttr { procFrameSize = frameSize }

      -- with this environment set up, analyse the procedure's component
      -- statements:
      aStmts <- mapM analyseStmt stmts

      -- we're finished with this var sym table; clear it for future procedures
      popVarSymTable

      -- done!
      return $ AProc ident aParams aDecls aStmts attrs


declareAnalyseParam :: Param -> SemanticAnalysis AParam
declareAnalyseParam param@(Param pos passBy baseType ident)
  = do
      -- allocate stack space for this local variable (all params only take
      -- a single stack slot)
      nextFreeSlot <- allocateStackSlots 1

      -- prepare the variable record for the symbol table
      let newRecord = VarRecord { varShape = Dim0
                                , varType = baseType
                                , varPassBy = passBy
                                , varStackSlot = nextFreeSlot
                                , varDefnPos = pos
                                }
      -- add this variable to the symbol table for the current scope, checking
      -- for duplicate definitions:
      addVarMapping ident newRecord
      -- now prepare the annotated param for code generation:
      let attrs = ParamAttr { paramStackSlot = nextFreeSlot
                            , paramPretty = prettify param
                            }
      return $ AParam passBy baseType ident attrs


declareAnalyseDecl :: Decl -> SemanticAnalysis ADecl
declareAnalyseDecl decl@(Decl pos baseType ident dim)
  = do
      -- allocate stack space for this local variable:
      let numRequiredSlots = numSlotsDim dim
      startSlot <- allocateStackSlots numRequiredSlots
      -- prepare the variable record for the symbol table
      let newRecord = VarRecord { varShape = dim
                                , varType = baseType
                                , varPassBy = Val
                                , varStackSlot = startSlot
                                , varDefnPos = pos
                                }
      -- add this variable to the symbol table for the current scope, checking
      -- for duplicate definitions:
      addVarMapping ident newRecord
      -- now prepare the annotated declaration for code generation:
      let allSlots = take (numRequiredSlots) [startSlot..]
      let attrs = DeclAttr { declStackSlots = allSlots
                           , declPretty = init $ prettify decl
                           }
      return $ ADecl baseType ident dim attrs
    where
      -- numSlotsDim
      -- Helper function: Calculate the size implied by the dimensionality
      numSlotsDim :: Dim -> Int
      numSlotsDim Dim0
        = 1
      numSlotsDim (Dim1 n)
        = n
      numSlotsDim (Dim2 n m)
        = n * m


analyseStmt :: Stmt -> SemanticAnalysis AStmt
analyseStmt asn@(Asg pos scalar expr)
  = do
      let attrs = AsgAttr { asgPretty = init $ prettify asn }
      aScalar <- analyseScalar scalar
      aExpr <- analyseExpr expr
      -- we may need to add a cast to the expression in case of float := int
      let lType = scalarType aScalar
      let rType = exprType aExpr
      aExpr' <- if (lType == FloatType && rType == IntType)
        then return $ AFloatCast aExpr
        else return $ aExpr

      -- we may only assign an expression with matching result type
      assert (scalarType aScalar == exprType aExpr') $ SemanticError pos $
        "incorrect expression result type for assignment (expected: " ++
        format (scalarType aScalar) ++ ", actual: " ++
        format (exprType aExpr') ++ ")"

      return $ AAsg aScalar aExpr' attrs

analyseStmt readscalar@(Read pos scalar)
  = do
      aScalar <- analyseScalar scalar
      let builtin = lookupReadBuiltin (scalarType aScalar)
      let attrs = ReadAttr { readBuiltin = builtin
                           , readPretty = init $ prettify readscalar
                           }
      return $ ARead aScalar attrs

analyseStmt writeexpr@(WriteExpr pos expr)
  = do
      aExpr <- analyseExpr expr
      let builtin = lookupPrintBuiltin (exprType aExpr)
      let attrs = WriteExprAttr { writeExprBuiltin = builtin
                                , writeExprPretty
                                    = init $ prettify writeexpr
                                }
      return $ AWriteExpr aExpr attrs

analyseStmt writestring@(WriteString pos string)
  = do
      let attrs = WriteStringAttr { writeStringPretty
                                      = init $ prettify writestring
                                  }

      return $ AWriteString string attrs

analyseStmt call@(Call pos ident args)
  = do
      aArgs <- mapM analyseExpr args
      maybeProcRecord <- lookupProc ident

      -- Pull the list of Params out of the ProcRecord, if it exists
      params <- case maybeProcRecord of
        Nothing -> do
          semanticError $ SemanticError pos $
            "call undefined procedure " ++ prettify ident
          -- error recovery: just assume the procedure takes no arguments
          return []
        Just procRecord -> return (procParams procRecord)

      -- Check that we have the same no. of args & params
      assert (length params == length args) $ SemanticError pos $
        "call with incorrect number of arguments (expected: " ++
        show (length params) ++ ", actual: " ++ show (length args) ++ ")"

      -- Get the Call Attributes
      let passBys = [ passBy | (Param _ passBy _ _) <- params]
      let attrs = CallAttr { callPassBys = passBys
                           , callPretty = init $ prettify call
                           }

      -- in the case of pass by value, introduce float casts if necessary:
      let aArgs' = zipWith castArg params aArgs

      -- Checks that parameters and arguments agree
      sequence $ zipWith assertParamMatchesArgs params aArgs'

      return $ ACall ident aArgs' attrs
    where
      castArg :: Param -> AExpr -> AExpr
      castArg (Param _ Val FloatType _) arg
        = case (exprType arg) of
              IntType -> AFloatCast arg
              _ -> arg
      castArg _ arg
        = arg


analyseStmt (If pos cond thenStmts)
  = do
      -- analyse condition type
      aCond <- analyseExpr cond
      assert (exprType aCond == BoolType) $ SemanticError pos $
        "incorrect type for condition (expected: " ++ format BoolType ++
        ", actual: " ++ format (exprType aCond) ++ ")"

      let attrs = IfAttr { ifPretty = prettify cond}
      aThenStmts <- mapM analyseStmt thenStmts
      return $ AIf aCond aThenStmts attrs

analyseStmt (IfElse pos cond thenStmts elseStmts)
  = do
      -- analyse condition type
      aCond <- analyseExpr cond
      assert (exprType aCond == BoolType) $ SemanticError pos $
        "incorrect type for condition (expected: " ++ format BoolType ++
        ", actual: " ++ format (exprType aCond) ++ ")"

      let attrs = IfElseAttr { ifElsePretty = prettify cond }
      aThenStmts <- mapM analyseStmt thenStmts
      aElseStmts <- mapM analyseStmt elseStmts
      return $ AIfElse aCond aThenStmts aElseStmts attrs

analyseStmt (While pos cond doStmts)
  = do
      -- analyse condition type
      aCond <- analyseExpr cond
      assert (exprType aCond == BoolType) $ SemanticError pos $
        "incorrect type for condition (expected: " ++ format BoolType ++
        ", actual: " ++ format (exprType aCond) ++ ")"

      let attrs = WhileAttr { whilePretty = prettify cond }
      aDoStmts <- mapM analyseStmt doStmts
      return $ AWhile aCond aDoStmts attrs


-- assertParamMatchesArgs
-- Checks the Argument has the same type as the Parameter, and that if the
-- Parameter is a pass by reference, then the argument is a Scalar.
assertParamMatchesArgs :: Param -> AExpr -> SemanticAnalysis ()
assertParamMatchesArgs (Param pos passBy baseType ident) arg
  = do
      assert ( baseType == exprType arg) $ SemanticError pos $
        "call with mismatched parameter and argument types (expected: " ++
        format baseType ++ ", actual: " ++ format (exprType arg) ++ ")"

      -- Now check that Parameters indicating pass by ref is a Scalar
      when (passBy == Ref) $ case arg of
          AScalarExpr _ -> return ()
          _ -> semanticError $ SemanticError pos $
            "passed non-scalar to reference parameter " ++ prettify ident

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
      let maybeInstrPair = lookupUnInstr op (exprType aExpr)
      attrs <- case maybeInstrPair of
        Nothing -> do
          semanticError $ SemanticError pos $ "cannot apply unary " ++
            "operator " ++ format op ++ " to operand of type " ++
            format (exprType aExpr)
          -- for error recovery, default values are given
          return $ UnExprAttr { unExprInstr = NegIntInstr
                              , unExprResultType = exprType aExpr
                              }
        Just (instr, resultType) -> return $
          UnExprAttr { unExprInstr = instr
                     , unExprResultType = resultType
                     }

      return $ AUnExpr op aExpr attrs

analyseExpr (BinExpr pos op lExpr rExpr)
  = do
      aLExpr <- analyseExpr lExpr
      aRExpr <- analyseExpr rExpr
      -- introduce casts, if necessary
      let (aLExpr', aRExpr') = castOperands op aLExpr aRExpr
      let lType = exprType aLExpr'
      let rType = exprType aRExpr'
      let maybeInstrPair = lookupBinInstr op lType rType
      attrs <- case maybeInstrPair of
        Nothing -> do
          semanticError $ SemanticError pos $ "cannot apply binary " ++
            "operator " ++ format op ++ " to operands of type " ++
            format lType ++ " and " ++ format rType
          return $ BinExprAttr { binExprInstr = SubIntInstr
                               , binExprResultType = lType
                               }
        Just (instr, resultType) -> return $
          BinExprAttr { binExprInstr = instr
                      , binExprResultType = resultType
                      }
      return $ ABinExpr op aLExpr' aRExpr' attrs


analyseScalar :: Scalar -> SemanticAnalysis AScalar
analyseScalar (Single pos ident)
  = do
      -- lookup the indentifier, hopefully if exists
      maybeRecord <- lookupVar ident
      record <- case maybeRecord of
        Just record -> return record
        Nothing -> do
          semanticError $ SemanticError pos $
            "reference undeclared single variable " ++ prettify ident
          -- error recovery: continue, assuming that the variable exists and
          -- has some default attributes:
          return $ dummyVarRecord { varShape = Dim0 }

      -- ensure that the dimensionality is correct (Dim0):
      record' <- case (varShape record) of
        Dim0 -> return $ record
        _ -> do
          semanticError $ SemanticError pos $
            "accessing array/matrix variable as if it were a single"
          -- error recovery: continue, assuming that it's a single.
          return $ record { varShape = Dim0 }

      let attrs = SingleAttr { singlePassBy = varPassBy record'
                             , singleStackSlot = varStackSlot record'
                             , singleBaseType = varType record'
                             }
      return $ ASingle ident attrs
analyseScalar (Array pos ident exprI)
  = do
      aExprI <- analyseExpr exprI
      assert (exprType aExprI == IntType) $ SemanticError pos $
        "array index must be an integer expression (received type: " ++
        format (exprType aExprI) ++ ")"

      -- lookup the indentifier, hopefully if exists
      maybeRecord <- lookupVar ident
      record <- case maybeRecord of
        Just record -> return record
        Nothing -> do
          semanticError $ SemanticError pos $
            "reference undeclared array variable " ++ prettify ident
          -- error recovery: continue, assuming that the array exists and
          -- has some default attributes:
          return $ dummyVarRecord { varShape = Dim1 1 }

      -- ensure that the dimensionality is correct (Dim1):
      record' <- case (varShape record) of
        (Dim1 _) -> return $ record
        _ -> do
          semanticError $ SemanticError pos $
            "accessing single/matrix variable as if it were an array"
          -- error recovery: continue, assuming that it's an array.
          return $ record { varShape = Dim1 1 }

      let attrs = ArrayAttr { arrayStartSlot = varStackSlot record'
                            , arrayBaseType = varType record'
                            }
      return $ AArray ident aExprI attrs
analyseScalar (Matrix pos ident exprI exprJ)
  = do
      -- analyse first index expression
      aExprI <- analyseExpr exprI
      assert (exprType aExprI == IntType) $ SemanticError pos $
        "first matrix index must be an integer expression (received type: " ++
        format (exprType aExprI) ++ ")"

      -- analyse second index expression
      aExprJ <- analyseExpr exprJ
      assert (exprType aExprJ == IntType) $ SemanticError pos $
        "second matrix index must be an integer expression (received type: " ++
        format (exprType aExprJ) ++ ")"

      -- lookup the indentifier, hopefully if exists
      maybeRecord <- lookupVar ident
      record <- case maybeRecord of
        Just record -> return record
        Nothing -> do
          semanticError $ SemanticError pos $
            "reference undeclared matrix variable " ++ prettify ident
          -- error recovery: continue, assuming that the matrix exists and
          -- has some default attributes:
          return $ dummyVarRecord { varShape = Dim2 1 1 }

      -- ensure that the dimensionality is correct (Dim2):
      record' <- case (varShape record) of
        (Dim2 _ _) -> return $ record
        _ -> do
          semanticError $ SemanticError pos $
            "accessing single/array variable as if it were a matrix"
          -- error recovery: continue, assuming that it's a matrix.
          return $ record { varShape = Dim2 1 1 }

      let (Dim2 _ rowWidth) = varShape record'
      let attrs = MatrixAttr { matrixStartSlot = varStackSlot record'
                             , matrixRowWidth = rowWidth
                             , matrixBaseType = varType record'
                             }
      return $ AMatrix ident aExprI aExprJ attrs


dummyVarRecord :: VarRecord
dummyVarRecord
  = VarRecord { varShape = Dim0
              , varType = IntType
              , varPassBy = Val
              , varStackSlot = Slot 0
              , varDefnPos = NoPos
              }


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
lookupUnInstr :: UnOp -> BaseType -> Maybe (UnInstruction, BaseType)
lookupUnInstr Not BoolType
  = Just (NotInstr, BoolType)
lookupUnInstr Neg IntType
  = Just (NegIntInstr, IntType)
lookupUnInstr Neg FloatType
  = Just (NegRealInstr, FloatType)
lookupUnInstr _ _
  = Nothing


-- castOperands
-- Compute an alternative version of the operands to a binary operation if they
-- will need to be cast to another type for that operation, or return the
-- operand expressions unchanged if they will not.
-- (An operand may require a cast to such as for `1 + 1.0` which should become
-- `float(1) + 1.0`)
castOperands :: BinOp -> AExpr -> AExpr -> (AExpr, AExpr)
castOperands op lExpr rExpr
  | op `elem` [Add, Sub, Mul, Div, LTh, LEq, GTh, GEq]
    = case (exprType lExpr, exprType rExpr) of
        -- arithmetic operations and comparison operations (other than = and !=)
        -- allow either of their operands to be promoted from IntType to
        -- FloatType if the other operand is a FloatType:
        (IntType, FloatType) -> (AFloatCast lExpr, rExpr)
        (FloatType, IntType) -> (lExpr, AFloatCast rExpr)
        -- any other type combination will not introduce a cast
        _ -> (lExpr, rExpr)
  -- other operations do not involve casts
  | otherwise = (lExpr, rExpr)


-- lookupBinInstr
-- Look up the appropriate Oz Instruction for two arguments of a particular type
-- It returns the instruction constructor and also the BaseType representing the
-- result of the operation.
lookupBinInstr :: BinOp -> BaseType -> BaseType
                  -> Maybe (BinInstruction, BaseType)

-- for int arguments, arithmetic and comparisons are allowed
lookupBinInstr Add IntType IntType
  = Just (AddIntInstr, IntType)
lookupBinInstr Sub IntType IntType
  = Just (SubIntInstr, IntType)
lookupBinInstr Mul IntType IntType
  = Just (MulIntInstr, IntType)
lookupBinInstr Div IntType IntType
  = Just (DivIntInstr, IntType)
lookupBinInstr Equ IntType IntType
  = Just (EquIntInstr, BoolType)
lookupBinInstr NEq IntType IntType
  = Just (NEqIntInstr, BoolType)
lookupBinInstr LTh IntType IntType
  = Just (LThIntInstr, BoolType)
lookupBinInstr LEq IntType IntType
  = Just (LEqIntInstr, BoolType)
lookupBinInstr GTh IntType IntType
  = Just (GThIntInstr, BoolType)
lookupBinInstr GEq IntType IntType
  = Just (GEqIntInstr, BoolType)

-- For float arguments, arithmetic and comparisons are allowed
lookupBinInstr Add FloatType FloatType
  = Just (AddRealInstr, FloatType)
lookupBinInstr Sub FloatType FloatType
  = Just (SubRealInstr, FloatType)
lookupBinInstr Mul FloatType FloatType
  = Just (MulRealInstr, FloatType)
lookupBinInstr Div FloatType FloatType
  = Just (DivRealInstr, FloatType)
lookupBinInstr Equ FloatType FloatType
  = Just (EquRealInstr, BoolType)
lookupBinInstr NEq FloatType FloatType
  = Just (NEqRealInstr, BoolType)
lookupBinInstr LTh FloatType FloatType
  = Just (LThRealInstr, BoolType)
lookupBinInstr LEq FloatType FloatType
  = Just (LEqRealInstr, BoolType)
lookupBinInstr GTh FloatType FloatType
  = Just (GThRealInstr, BoolType)
lookupBinInstr GEq FloatType FloatType
  = Just (GEqRealInstr, BoolType)


-- for boolean arguments only comparisons and logical relations are allowed
-- Logical operations will actually be skipped at runtime for specific code
-- that creates short-circuit evaluation. For now, we use dummy instructions
-- `AndInstr` and `OrInstr` so that these expressions can still be annotated.
lookupBinInstr Equ BoolType BoolType
  = Just (EquIntInstr, BoolType)
lookupBinInstr NEq BoolType BoolType
  = Just (NEqIntInstr, BoolType)
lookupBinInstr LTh BoolType BoolType
  = Just (LThIntInstr, BoolType)
lookupBinInstr LEq BoolType BoolType
  = Just (LEqIntInstr, BoolType)
lookupBinInstr GTh BoolType BoolType
  = Just (GThIntInstr, BoolType)
lookupBinInstr GEq BoolType BoolType
  = Just (GEqIntInstr, BoolType)
lookupBinInstr And BoolType BoolType
  = Just (AndInstr, BoolType)
lookupBinInstr Or BoolType BoolType
  = Just (OrInstr, BoolType)

-- Any other combination is not allowed
lookupBinInstr _ _ _
  = Nothing
