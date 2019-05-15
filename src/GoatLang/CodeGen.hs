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
genCodeExprInto register (BoolConst bool)
  = case bool of
      True -> InstrLeaf $ IntConstInstr register 1
      False -> InstrLeaf $ IntConstInstr register 0
genCodeExprInto register (BinExpr op l r)
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (succ register) r
    , generateBinOpInstrTree register register (succ register) op lType rType
    ]
    where
      lType = getExprType l
      rType = getExprType r

generateBinOpInstrTree :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType
  -> InstrTree
generateBinOpInstrTree destReg lReg rReg op lType rType
  = case (lType, rType) of
      (IntType, IntType) -> InstrLeaf intInstruction
      (BoolType, BoolType) -> InstrLeaf boolInstruction
      otherwise -> InstrList [ realify lReg lType
                             , realify rReg rType
                             , InstrLeaf floatInstruction
                             ]
      where
        floatInstruction = (lookupOpReal op) destReg lReg rReg
        intInstruction = (lookupOpInt op) destReg lReg rReg
        boolInstruction = (lookupOpBool op) destReg lReg rReg

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
lookupOpBool And
  = AndInstr
lookupOpBool Or
  = OrInstr
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
