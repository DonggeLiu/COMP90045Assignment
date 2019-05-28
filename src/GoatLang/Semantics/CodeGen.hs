module GoatLang.Semantics.CodeGen where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
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

import Control.Monad (mapM_, when)

import GoatLang.Syntax.AST
import GoatLang.Semantics.AAST
import GoatLang.Semantics.CodeGenMonad
import OzLang.Code


-- genCodeFullProgram
-- Top level function: use the code generators defined below to convert an
-- annotated Goat Program (an AAST) into an Oz Program (a list of Oz lines).
genCodeFullProgram :: AGoatProgram -> OzProgram
genCodeFullProgram goatProgram
  = genCode (genCodeGoatProgram goatProgram)


-- ----------------------------------------------------------------------------
-- Code generators
-- ----------------------------------------------------------------------------

-- genCodeGoatProgram
-- Action to generate instructions for a full Goat Program.
genCodeGoatProgram :: AGoatProgram -> CodeGen ()
genCodeGoatProgram (AGoatProgram procs)
  = do
      instr $ CallInstr (ProcLabel "main")
      instr $ HaltInstr
      mapM_ genCodeProc procs


-- genCodeProc
-- Action to generate instructions for a single Goat Procedure, including
-- the procedure's prologue and epilogue.
genCodeProc :: AProc -> CodeGen ()
genCodeProc (AProc (Id _ procName) params decls stmts attrs)
  = do
      label $ ProcLabel procName
      comment "prologue"
      instr $ PushStackFrameInstr (procFrameSize attrs)
      sequence_ $ zipWith genCodeRetrieveParamFrom [Reg 0..] params
      mapM_ genCodeInitVar decls
      comment "procedure body"
      mapM_ genCodeStmt stmts
      comment "epilogue"
      instr $ PopStackFrameInstr (procFrameSize attrs)
      instr ReturnInstr


-- genCodeRetrieveParamFrom
-- Retrieve the value of a passed argument from a register into the
-- appropriate stack slot corresponding to the paramater (local variable).
genCodeRetrieveParamFrom :: Reg -> AParam -> CodeGen ()
genCodeRetrieveParamFrom reg (AParam _ _ _ attrs)
  = do
      comment $ "retrieve " ++ (paramPretty attrs)
      instr $ StoreInstr (paramStackSlot attrs) reg


-- genCodeInitVar
-- Action to generate code to initialise a local variable to 0.
genCodeInitVar :: ADecl -> CodeGen ()
genCodeInitVar (ADecl baseType _ dim attrs)
  = do
      comment $ "initialise " ++ (declPretty attrs)
      -- load the 'zero' value of the correct type into an unused (!) register
      case baseType of
        FloatType -> instr $ RealConstInstr (Reg 0) 0.0
        otherwise -> instr $ IntConstInstr (Reg 0) 0
      -- then store it into the slot (or slots) spanned by this local variable
      mapM_ (\slot -> instr (StoreInstr slot (Reg 0))) (declStackSlots attrs)


-- genCodeStmt
-- Action to generate code for a single Goat Statement (may be an atomic or
-- composite statement).
genCodeStmt :: AStmt -> CodeGen ()

genCodeStmt (AWriteExpr expr attrs)
  = do
      comment $ writeExprPretty attrs
      genCodeExprInto (Reg 0) expr
      instr $ CallBuiltinInstr (writeExprBuiltin attrs)

genCodeStmt (AWriteString str attrs)
  = do
      comment $ writeStringPretty attrs
      instr $ StringConstInstr (Reg 0) str
      instr $ CallBuiltinInstr PrintStr

genCodeStmt stmt@(ARead scalar attrs)
  = do
      comment $ readPretty attrs
      instr $ CallBuiltinInstr (readBuiltin attrs)
      genCodeStore scalar (Reg 0)

genCodeStmt stmt@(AAsg scalar expr attrs)
  = do
      comment $ asgPretty attrs
      genCodeExprInto (Reg 0) expr
      genCodeStore scalar (Reg 0)

genCodeStmt (AIf cond thenStmts attrs)
  = do
      comment $ "if " ++ ifPretty attrs
      genCodeExprInto (Reg 0) cond
      fiLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) fiLabel
      mapM_ genCodeStmt thenStmts
      label $ fiLabel

genCodeStmt (AIfElse cond thenStmts elseStmts attrs)
  = do
      comment $ "if-else " ++ ifElsePretty attrs
      genCodeExprInto (Reg 0) cond
      elseLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) elseLabel
      mapM_ genCodeStmt thenStmts
      fiLabel <- getNewBlockLabel
      instr $ BranchUncondInstr fiLabel
      label $ elseLabel
      mapM_ genCodeStmt elseStmts
      label $ fiLabel

genCodeStmt (AWhile cond stmts attrs)
  = do
      comment $ "while " ++ whilePretty attrs
      whileLabel <- getNewBlockLabel
      label $ whileLabel
      genCodeExprInto (Reg 0) cond
      odLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) odLabel
      mapM_ genCodeStmt stmts
      instr $ BranchUncondInstr whileLabel
      label $ odLabel

genCodeStmt stmt@(ACall (Id _ procName) args attrs)
  = do
      comment $ callPretty attrs
      sequence_ $ zipWith3 genCodeArgInto [Reg 0..] (callPassBys attrs) args
      instr $ CallInstr $ ProcLabel procName

-- genCodeStore
-- Generate the code to store the value in a register into the stack slot
-- corresponding to a scalar.
genCodeStore :: AScalar -> Reg -> CodeGen ()

-- Single variable scalars may have been passed by reference, in which case we
-- need to store them indirectly using the _address_ in the local stack slot.
-- In contrast for scalars passed by value, we simply store the value from the
-- register to the stack slot.
genCodeStore (ASingle _ attrs) reg
  = case (singlePassBy attrs) of
      Val -> instr $ StoreInstr (singleStackSlot attrs) reg
      Ref -> do
          instr $ LoadInstr (succ reg) (singleStackSlot attrs)
          instr $ StoreIndirectInstr (succ reg) reg

-- Arrays and Matrices cannot be passed by reference but we need to calculate
-- the correct stack slot based on an offset given by the index expression(s).
genCodeStore scalar reg
  = do
      genCodeOffsetAddrInto (succ reg) scalar
      instr $ StoreIndirectInstr (succ reg) reg


-- genCodeArgInto
-- Generate the code to load the argument value given by expr (or reference,
-- as per param) into the given register.
genCodeArgInto :: Reg -> PassBy -> AExpr -> CodeGen ()

-- It's easy to prepare something to be passed by value, we can just use
-- genCodeExprInto:
genCodeArgInto reg Val expr
  = genCodeExprInto reg expr

-- For pass by reference, the expression must be an lvalue, and we need to
-- ensure that we load its address into the register instead.

-- For Single variables, the stack slot may ALREADY hold an address; in which
-- case we can just copy that address into the register. If it's a value we
-- need to load its address instead.
genCodeArgInto reg Ref (AScalarExpr (ASingle _ attrs))
  = case (singlePassBy attrs) of
      Val -> instr $ LoadAddressInstr reg (singleStackSlot attrs)
      Ref -> instr $ LoadInstr reg (singleStackSlot attrs)

-- For non-Single variables (Arrays/Matrices) they must already be values (Goat
-- does not allow Array and Matrix variables to be passed by ref). Thus we just
-- need to calculate the address and load it into the register.
genCodeArgInto reg Ref (AScalarExpr scalar)
  = genCodeOffsetAddrInto reg scalar


-- genCodeOffsetAddrInto
-- Given an Array or Matrix scalar, evaluate the one or two indices to determine
-- the offset and load the address at this offset into the given register.
genCodeOffsetAddrInto :: Reg -> AScalar -> CodeGen ()
genCodeOffsetAddrInto reg (AArray _ exprI attrs)
  = do
      -- calculate offset into a register
      genCodeExprInto reg exprI
      -- put the start slot address in the next register
      instr $ LoadAddressInstr (succ reg) (arrayStartSlot attrs)
      -- calculate address of indexed scalar into target register
      instr $ SubOffsetInstr reg (succ reg) reg

genCodeOffsetAddrInto reg (AMatrix _ exprI exprJ attrs)
  = do
      -- start by calculating the offset into a register
      -- first, the offset to the start of the indexed row
      instr $ IntConstInstr reg (matrixRowWidth attrs)
      genCodeExprInto (succ reg) exprI
      instr $ MulIntInstr reg reg (succ reg)
      -- plus the offset within that row
      genCodeExprInto (succ reg) exprJ
      instr $ AddIntInstr reg reg (succ reg)
      -- then, put the matrix's start slot address in the next register
      instr $ LoadAddressInstr (succ reg) (matrixStartSlot attrs)
      -- calculate address of indexed scalar into target register
      instr $ SubOffsetInstr reg (succ reg) reg


-- genCodeExprInto register
-- Action to generate code that will get the result of an expression into a
-- target register. Will not touch registers below this target register.
genCodeExprInto :: Reg -> AExpr -> CodeGen ()

-- Base cases 1-3: float, int and bool constants:
genCodeExprInto reg (AIntConst int)
  = instr $ IntConstInstr reg int
genCodeExprInto reg (AFloatConst float)
  = instr $ RealConstInstr reg float
-- In Oz we represent True as the integer 1, and false as the integer 0
genCodeExprInto reg (ABoolConst True)
  = instr $ IntConstInstr reg 1
genCodeExprInto reg (ABoolConst False)
  = instr $ IntConstInstr reg 0

-- Base case 4: Scalar expressions:

-- For scalar expressions of Single variables we need to be careful about
-- whether the stack slot holds a reference (for pass by reference variables)
-- or a value (for pass by value variables and local variables).
genCodeExprInto reg (AScalarExpr (ASingle _ attrs))
  = case (singlePassBy attrs) of
      Val -> instr $ LoadInstr reg (singleStackSlot attrs)
      Ref -> do
          instr $ LoadInstr reg (singleStackSlot attrs)
          instr $ LoadIndirectInstr reg reg

-- All non-Single variables (Arrays/Matrices) must already be values, but we
-- will need to generate code for caulcating the scalar address offset from the
-- variable's start address by the result of some integer expression(s).
-- Luckily, for both Arrays an Matrices, the pattern of loading the value given
-- the offset address is similar. See `genCodeOffsetAddrInto` for computing the
-- offset address.
genCodeExprInto reg (AScalarExpr scalar)
  = do
      -- go and calculate the offset-address into the register
      genCodeOffsetAddrInto reg scalar
      -- then load into the register the value stored at that address
      instr $ LoadIndirectInstr reg reg


-- Recursive case 1: Float cast involving nested expression:
genCodeExprInto reg (AFloatCast expr)
  = do
      genCodeExprInto reg expr
      instr $ IntToRealInstr reg reg

-- Recursive case 2: unary operations involving nested expressions:
genCodeExprInto reg (AUnExpr _ expr attrs)
  = do
      genCodeExprInto reg expr
      instr $ (unExprInstr attrs) reg reg

-- Recursive case 3: binary operations (non-strict or strict)

-- First, treat the non-strict operations specially:

-- 'And' will not actually use oz's 'and' instruction (which is strict).
-- Instead, we'll:
-- 1. Load the first operand into the target register.
-- 2. If false (0), we are done! Skip evaluating the second operand, and leave
--    the 0 in the target register.
-- 3. If true (1), we need to check the second operand. Load into target
--    register.
-- 4. Whatever the result, leave it in the target register (it's the result of
--    the And operation).
genCodeExprInto reg (ABinExpr And l r _)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto reg l
      instr $ BranchOnFalseInstr reg afterLabel
      genCodeExprInto reg r
      label afterLabel

-- Or is similar (but we skip the second operand when the first is True,
-- rather than False):
genCodeExprInto reg (ABinExpr Or l r _)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto reg l
      instr $ BranchOnTrueInstr reg afterLabel
      genCodeExprInto reg r
      label afterLabel

-- All other expressions are strict, so we can safely load the operands into
-- two registers then just perform the appropriate operation:
genCodeExprInto reg (ABinExpr _ lExpr rExpr attrs)
  = do
      genCodeExprInto reg lExpr
      genCodeExprInto (succ reg) rExpr
      instr $ (binExprInstr attrs) reg reg (succ reg)
