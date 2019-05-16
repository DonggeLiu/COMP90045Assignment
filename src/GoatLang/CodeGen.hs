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

import Control.Monad (mapM_)
import Control.Monad.State

import GoatLang.AST
import GoatLang.OzCode


-- Summary of TODO items from throughout file:

-- Some time, if we have time:
-- - Switch to using a difflist instead of instruction tree

-- When we do semantic analysis:
-- - Of course, we will want to change from using a recursive function to
--   calculate expression types to precomputing these types and embedding them
--   within the Expression AST nodes themselves.
-- - Idea: Annotate the AST with additional 'float cast' nodes to avoid having
--   a separate case for every operation when the arguments might be float and
--   int? E.g.: `Add FloatType (FloatCast (IntConst 4)) (FloatConst 4.0)`.


-- ----------------------------------------------------------------------------
-- CodeGen State Monad
-- ----------------------------------------------------------------------------

type LabelCounter
  = Int
data CodeGenState
  = CodeGenState LabelCounter InstrTree
type CodeGen a
  = State CodeGenState a


-- genCode
-- Using the code generators, convert a Goat Program into a list of Oz
-- Instructions.
genCode :: GoatProgram -> InstrTree
genCode prog
  = instrs
  where
    (_, CodeGenState _ instrs)
      = runState (genCodeGoatProgram prog) (CodeGenState 0 (InstrList []))


-- Internal helper functions for interfacing with the CodeGen state:

getLabelCounter :: CodeGen LabelCounter
getLabelCounter
  = do
      CodeGenState labelCounter _ <- get
      return labelCounter
incLabelCounter :: CodeGen ()
incLabelCounter
  = do
      CodeGenState labelCounter instrs <- get
      put $ CodeGenState (labelCounter + 1) instrs

appendInstr :: InstrTree -> CodeGen ()
appendInstr instrTree
  = do
      CodeGenState labelCounter (InstrList instrs) <- get
      put $ CodeGenState labelCounter (InstrList (instrs ++ [instrTree]))


-- Helper functions for accessing state of CodeGen monad

getNewBlockLabel :: CodeGen Label
getNewBlockLabel
  = do
      labelCounter <- getLabelCounter
      incLabelCounter
      return (BlockLabel labelCounter)

instr :: Instruction -> CodeGen ()
instr instruction
  = appendInstr $ InstrLeaf instruction

label :: Label -> CodeGen ()
label lab
  = appendInstr $ InstrLabel lab

comment :: String -> CodeGen ()
comment text
  = appendInstr $ InstrComment text




-- ----------------------------------------------------------------------------
-- Code generators
-- ----------------------------------------------------------------------------

genCodeGoatProgram :: GoatProgram -> CodeGen ()
genCodeGoatProgram (GoatProgram [main])
  = do
      instr $ CallInstr (ProcLabel "main")
      instr $ HaltInstr
      genCodeProc main



genCodeProc :: Proc -> CodeGen ()
genCodeProc (Proc (Id "main") [] [] stmts) -- TODO: allow declarations in main
  = do
      label $ ProcLabel "main"
      -- comment "prologue"
      -- TODO: annotate to figure out the required frame size
      -- instr $ PushStackFrameInstr (FrameSize ???)
      -- TODO: Copy parameters from registers to stack slots
      -- TODO: Initialise all local variables to 0.
      comment "procedure body"
      mapM_ genCodeStmt stmts
      -- comment "epilogue"
      -- instr $ PopStackFrameInstr (FrameSize ???)
      instr ReturnInstr


genCodeStmt :: Stmt -> CodeGen ()
genCodeStmt (WriteExpr expr)
  = do
      comment "write <expr>"
      genCodeExprInto (Reg 0) expr
      instr $ CallBuiltinInstr $ lookupPrintBuiltin expr

genCodeStmt (WriteString str)
  = do
      comment "write <string>"
      instr $ StringConstInstr (Reg 0) str
      instr $ CallBuiltinInstr PrintStr

lookupPrintBuiltin :: Expr -> BuiltinFunc
lookupPrintBuiltin expr
  = case getExprType expr of
      BoolType -> PrintBool
      FloatType -> PrintReal
      IntType -> PrintInt


genCodeExprInto :: Reg -> Expr -> CodeGen ()
genCodeExprInto register (IntConst int)
  = instr $ IntConstInstr register int
genCodeExprInto register (FloatConst float)
  = instr $ RealConstInstr register float
genCodeExprInto register (BoolConst True)
  = instr $ IntConstInstr register 1
genCodeExprInto register (BoolConst False)
  = instr $ IntConstInstr register 0

-- Treat non-strict operations specially:

-- 'And' will not actually use oz's 'and' instruction (which is strict).
-- Instead, we'll:
-- - Load the first operand into the target register.
-- - If false (0), we are done! Skip evaluating the second operand, and leave
--   the 0 in the target register.
-- - If true (1), we need to check the second operand. Load into target
--   register.
-- - Whatever the result, leave it in the target register (that's the result of
--   the And operation).
genCodeExprInto register (BinExpr And l r)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto register l
      instr $ BranchOnFalseInstr register afterLabel
      genCodeExprInto register r
      label afterLabel

-- Or is similar (but we skip the second operand when the first is True,
-- rather than False).
genCodeExprInto register (BinExpr Or l r)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto register l
      instr $ BranchOnTrueInstr register afterLabel
      genCodeExprInto register r
      label afterLabel

genCodeExprInto register (BinExpr op l r)
  = do
      genCodeExprInto register l
      genCodeExprInto (succ register) r
      genCodeBinOp register register (succ register) op lType rType
  where
    lType = getExprType l
    rType = getExprType r

genCodeExprInto register (UnExpr Not expr)
  = do
      genCodeExprInto register expr
      instr $ NotInstr register register

genCodeExprInto register (UnExpr Neg expr)
  = do
      genCodeExprInto register expr
      instr $ instruction register register
  where
    instruction = case getExprType expr of
      FloatType -> NegRealInstr
      IntType -> NegIntInstr

genCodeBinOp :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType -> CodeGen ()

genCodeBinOp destReg lReg rReg op IntType IntType
  = instr $ (lookupOpInt op) destReg lReg rReg

genCodeBinOp destReg lReg rReg op BoolType BoolType
  = instr $ (lookupOpBool op) destReg lReg rReg

genCodeBinOp destReg lReg rReg op FloatType FloatType
  = instr $ (lookupOpReal op) destReg lReg rReg

-- maybe we have an int and a real
-- TODO (semantic analysis): 
-- Annotate the AST with additional 'float cast' nodes to avoid this case
-- E.g.: `Add FloatType (FloatCast (IntConst 4)) (FloatConst 4.0)`.
genCodeBinOp destReg lReg rReg op lType rType
  = do
      realify lReg lType
      realify rReg rType
      instr $ (lookupOpIntOrReal op) destReg lReg rReg

realify :: Reg -> BaseType -> CodeGen ()
realify _ FloatType
  = return ()
realify reg IntType
  = instr $ IntToRealInstr reg reg

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
