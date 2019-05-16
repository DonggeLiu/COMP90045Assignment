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
import GoatLang.OzCode


genCode :: GoatProgram -> InstrTree
genCode (GoatProgram [main])
  = InstrList [ InstrLeaf $ CallInstr (ProcLabel "main")
              , InstrLeaf HaltInstr
              , genCodeProc main
              ] 

-- TODO:
-- 3. Follow instructions in slack about starting milestone 2.

genCodeProc :: Proc -> InstrTree
genCodeProc (Proc (Id "main") [] [] stmts) -- TODO: allow declarations in main
  = InstrList [ InstrLabel $ ProcLabel "main"
              -- , InstrComment "prologue"
              -- TODO: annotate to figure out the required frame size
              -- , InstrLeaf $ PushStackFrameInstr (FrameSize ???)
              -- TODO: Copy parameters from registers to stack slots
              -- TODO: Initialise all local variables to 0.
              , InstrComment "procedure body"
              , InstrList $ map genCodeStmt stmts
              -- , InstrComment "epilogue"
              -- , InstrLeaf $ PopStackFrameInstr (FrameSize ???)
              , InstrLeaf ReturnInstr
              ]


genCodeStmt :: Stmt -> InstrTree
genCodeStmt (WriteExpr expr)
  = InstrList [ InstrComment "write <expr>"
              , genCodeExprInto (Reg 0) expr
              , InstrLeaf $ CallBuiltinInstr $ lookupPrintBuiltin expr
              ]
genCodeStmt (WriteString str)
  = InstrList [ InstrComment "write <string>"
              , InstrLeaf $ StringConstInstr (Reg 0) str
              , InstrLeaf $ CallBuiltinInstr PrintStr
              ]

lookupPrintBuiltin :: Expr -> BuiltinFunc
lookupPrintBuiltin expr
  = case getExprType expr of
      BoolType -> PrintBool
      FloatType -> PrintReal
      IntType -> PrintInt


genCodeExprInto :: Reg -> Expr -> InstrTree
genCodeExprInto register (IntConst int)
  = InstrLeaf $ IntConstInstr register int
genCodeExprInto register (FloatConst float)
  = InstrLeaf $ RealConstInstr register float
genCodeExprInto register (BoolConst True)
  = InstrLeaf $ IntConstInstr register 1
genCodeExprInto register (BoolConst False)
  = InstrLeaf $ IntConstInstr register 0

-- Treat non-strict operations specially:

-- 'And' will not actually use oz's 'and' instruction (which is strict).
-- Instead, we'll:
-- * Load the first operand into the target register.
-- * If false (0), we are done! Skip evaluating the second operand, and leave
--   the 0 in the target register.
-- * If true (1), we need to check the second operand. Load into target
--   register.
-- * Whatever the result, leave it in the target register (that's the result of
--   the And operation).
genCodeExprInto register (BinExpr And l r)
  = InstrList [ genCodeExprInto register l
              , InstrLeaf $ BranchOnFalseInstr register afterLabel
              , genCodeExprInto register r
              , InstrLabel afterLabel
              ]
  where
    afterLabel = BlockLabel 0 -- TODO: Get an actual new label

-- Or is similar (but we skip the second operand when the first is True,
-- rather than False).
genCodeExprInto register (BinExpr Or l r)
  = InstrList [ genCodeExprInto register l
              , InstrLeaf $ BranchOnTrueInstr register afterLabel
              , genCodeExprInto register r
              , InstrLabel afterLabel
              ]
  where
    afterLabel = BlockLabel 0 -- TODO: Get an actual new label

genCodeExprInto register (BinExpr op l r)
  = InstrList [ genCodeExprInto register l
              , genCodeExprInto (succ register) r
              , generateBinOpInstrTree register register (succ register) op lType rType
              ]
  where
    lType = getExprType l
    rType = getExprType r

genCodeExprInto register (UnExpr Not expr)
  = InstrList [ genCodeExprInto register expr
              , InstrLeaf $ NotInstr register register
              ]

genCodeExprInto register (UnExpr Neg expr)
  = InstrList [ genCodeExprInto register expr
              , InstrLeaf $ instruction register register
              ]
  where
    instruction = case getExprType expr of
      FloatType -> NegRealInstr
      IntType -> NegIntInstr

generateBinOpInstrTree :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType
  -> InstrTree

generateBinOpInstrTree destReg lReg rReg op IntType IntType
  = InstrLeaf $ (lookupOpInt op) destReg lReg rReg

generateBinOpInstrTree destReg lReg rReg op BoolType BoolType
  = InstrLeaf $ (lookupOpBool op) destReg lReg rReg

generateBinOpInstrTree destReg lReg rReg op FloatType FloatType
  = InstrLeaf $ (lookupOpReal op) destReg lReg rReg

-- maybe we have an int and a real
-- TODO: 
-- Annotate the AST with additional 'float cast' nodes to avoid this case
-- E.g.: `Add FloatType (FloatCast (IntConst 4)) (FloatConst 4.0)`.
generateBinOpInstrTree destReg lReg rReg op lType rType
  = InstrList [ realify lReg lType
              , realify rReg rType
              , InstrLeaf $ (lookupOpIntOrReal op) destReg lReg rReg
              ]

realify :: Reg -> BaseType -> InstrTree
realify _ FloatType
  = InstrList []
realify reg IntType
  = InstrLeaf $ IntToRealInstr reg reg

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

lookupOpIntOrReal :: BinOp -> (Reg -> Reg -> Reg -> Instruction)
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

getExprType :: Expr -> BaseType
getExprType (BoolConst _)
  = BoolType
getExprType (FloatConst _)
  = FloatType
getExprType (IntConst _)
  = IntType
getExprType (BinExpr operator left right)
  = case operator of
      Add -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Sub -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Mul -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      Div -> case getExprType left of
        FloatType -> FloatType
        otherwise -> case getExprType right of
          FloatType -> FloatType
          otherwise -> IntType
      otherwise -> BoolType
getExprType (UnExpr operator operand)
  = getExprType operand
-- getExprType (ScalarExpr scalar)
--   = getScalarType scalar

-- getScalarType :: Scalar -> BaseType
-- TODO: implement (using symbol table?)
