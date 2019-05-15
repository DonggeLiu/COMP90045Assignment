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
  = InstrList [ InstrLeaf $ CallInstr (Label "proc_main")
              , InstrLeaf HaltInstr
              , genCodeProc main
              ] 

genCodeProc :: Proc -> InstrTree
genCodeProc (Proc (Id "main") [] [] stmts) -- TODO: allow declarations in main
  = InstrList [ InstrLabel $ ProcLabel "main"
              , InstrComment "prologue"
              -- TODO: annotate to figure out the required frame size
              -- , InstrLeaf $ PushStackFrameInstr (FrameSize ???)
              , InstrList $ map genCodeStmt stmts
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

genCodeExprInto :: Register -> Expr -> InstrTree
genCodeExprInto register (AIntConst attr i)
  = InstrLeaf $ IntConstInstr register i
genCodeExprInto register (AFloatConst attr f)
  = InstrLeaf $ FloatConstInstr register f
genCodeExprInto register (ABoolConst attr b)
  = case b of
      True -> InstrLeaf $ IntConstInstr register 1
      False -> InstrLeaf $ IntConstInstr register 0
genCodeExprInto register (ABinExpr attr op l r)
  = InstrList
    [ genCodeExprInto register l
    , genCodeExprInto (register + 1) r
    , generateBinOpInstrTree register register (register + 1) op lType rType
    ]
    where
      lType = getExprType l
      rType = getExprType r

generateBinOpInstrTree :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType
  -> InstrTree
generateBinOpInstrTree destReg lReg rReg op lType rType
  = case (lType, rType) of
      (IntType, IntType) -> InstrLeaf intInstruction
      otherwise -> InstrList [ realify lReg lType
                             , realify rReg rType
                             , InstrLeaf floatInstruction
                             ]
      where
        floatInstruction = (lookupOpFloat op) destReg lReg rReg
        intInstruction = (lookupOpInt op) destReg lReg rReg

realify :: Reg -> BaseType -> InstrTree
realify _ FloatType
  = InstrList []
realify reg IntType
  = InstrLeaf $ IntToRealInstr reg reg

lookupOpFloat :: BinOp -> Reg -> Reg -> Reg -> Instruction
lookupOpFloat Add
  = AddRealInstr
lookupOpFloat Sub
  = SubRealInstr
lookupOpFloat Mul
  = MulRealInstr
lookupOpFloat Div
  = DivRealInstr

lookupOpInt :: BinOp -> Reg -> Reg -> Reg -> Instruction
lookupOpInt Add
  = AddIntInstr
lookupOpInt Sub
  = SubIntInstr
lookupOpInt Mul
  = MulIntInstr
lookupOpInt Div
  = DivIntInstr

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
getExprType (ScalarExpr scalar)
  = getScalarType scalar

-- getScalarType :: Scalar -> BaseType
-- TODO: implement (using symbol table?)
