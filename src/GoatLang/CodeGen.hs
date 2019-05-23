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

import Control.Monad (mapM_, when)
import Control.Monad.State

import Util.DiffList

import GoatLang.AST
import GoatLang.OzCode
import GoatLang.SymbolTable
import GoatLang.PrettyPrint


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


-- ----------------------------------------------------------------------------
-- CodeGen State Monad
-- ----------------------------------------------------------------------------

-- We'll use a state monad to simplify construction of Oz programs
type CodeGen a
  = State CodeGenState a

-- The state monad will store a label counter (an int) and a (difference) list
-- of OzLines (instructions or peudo-instructions; see OzCode module).
data CodeGenState
  = CodeGenState LabelCounter (DiffList OzLine)

type LabelCounter
  = Int


-- genCode
-- Top level function: use the code generators defined below to convert a Goat
-- Program (an AST) into an Oz Program (a list of Oz lines).
genCode :: GoatProgram -> OzProgram
genCode goatProgram
  = ozProgram
  where
    -- start with label counter at 0 and an empty list of program lines
    start = CodeGenState 0 (mempty :: DiffList OzLine)

    -- run the code generators an extract the completed difference list of lines
    ((), CodeGenState _ lines) = runState (genCodeGoatProgram goatProgram) start

    -- convert the result into an Oz program
    ozProgram = OzProgram (listify lines)



-- Internal helper functions for interacting directly with the CodeGen state:

-- getLabelCounter
-- Extract the label counter from the monad's state and return.
getLabelCounter :: CodeGen LabelCounter
getLabelCounter
  = do
      CodeGenState labelCounter _ <- get
      return labelCounter

-- incLabelCounter
-- Add 1 to the label counter in the monad's state.
incLabelCounter :: CodeGen ()
incLabelCounter
  = do
      CodeGenState labelCounter instrs <- get
      put $ CodeGenState (labelCounter + 1) instrs

-- appendLine
-- Append a new oz line to the monad's (difference) list of oz lines
-- (conceptually adding it to the end of the program-so-far).
appendLine :: OzLine -> CodeGen ()
appendLine line
  = do
      CodeGenState labelCounter lines <- get
      put $ CodeGenState labelCounter (lines `snoc` line)
      -- NOTE: snoc is 'backwards cons': append a single thing to a diff list


-- External helper functions for indirectly using the CodeGen state:

-- getNewBlockLabel
-- Increment the internal label counter and return a fresh block label.
getNewBlockLabel :: CodeGen Label
getNewBlockLabel
  = do
      labelCounter <- getLabelCounter
      incLabelCounter
      return (BlockLabel labelCounter)

-- instr
-- Append an Instruction to the program's lines.
instr :: Instruction -> CodeGen ()
instr instruction
  = appendLine $ Instr instruction

-- label
-- Append a Label to the program's lines (as a pseudo-instruction).
label :: Label -> CodeGen ()
label lab
  = appendLine $ Label lab

-- comment
-- Append a Comment to the program's lines (as a pseudo-instruction).
comment :: String -> CodeGen ()
comment text
  = appendLine $ Comment text




-- ----------------------------------------------------------------------------
-- Code generators
-- ----------------------------------------------------------------------------

-- genCodeGoatProgram
-- Action to generate instructions for a full Goat Program.
genCodeGoatProgram :: GoatProgram -> CodeGen ()
genCodeGoatProgram (GoatProgram procs)
  = do
      let procSymTable = constructProcSymTable procs
      instr $ CallInstr (ProcLabel "main")
      instr $ HaltInstr
      mapM_ (genCodeProc procSymTable) procs


-- genCodeProc
-- Action to generate instructions for a single Goat Procedure, including
-- the procedure's prologue and epilogue.
genCodeProc :: ProcSymTable -> Proc -> CodeGen ()
genCodeProc procSymTable (Proc ident@(Id procName _) params decls stmts _)
  = do
      let procRecord = lookupProcRecord procSymTable ident
      let varSymTable = procVarSymTable procRecord
      let frameSize = procFrameSize procRecord
      label $ ProcLabel procName
      comment "prologue"
      instr $ PushStackFrameInstr frameSize
      sequence_ $
        zipWith (genCodeRetrieveParamFrom varSymTable) [Reg 0..] params
      mapM_ (genCodeInitVar varSymTable) decls
      comment "procedure body"
      mapM_ (genCodeStmt procSymTable varSymTable) stmts
      comment "epilogue"
      instr $ PopStackFrameInstr frameSize
      instr ReturnInstr


-- genCodeRetrieveParamFrom
-- Retrieve the value of a passed argument from a register into the
-- appropriate stack slot corresponding to the paramater (local variable).
genCodeRetrieveParamFrom :: VarSymTable -> Reg -> Param -> CodeGen ()
genCodeRetrieveParamFrom symTable reg (Param _ _ ident@(Id name _) _)
  = do
      comment $ "retrieving " ++ name
      let slot = varStackSlot $ lookupVarRecord symTable ident
      instr $ StoreInstr slot reg


-- genCodeInitVar
-- Action to generate code to initialise a local variable to 0.
genCodeInitVar :: VarSymTable -> Decl -> CodeGen ()
genCodeInitVar symTable decl@(Decl baseType ident dim _)
  = do
      comment $ "initialising " ++ (init $ prettify decl)
      let slot = varStackSlot $ lookupVarRecord symTable ident
      case baseType of
        FloatType -> instr $ RealConstInstr (Reg 0) 0.0
        otherwise -> instr $ IntConstInstr (Reg 0) 0
      -- then store it into the slot (or slots) spanned by this local variable
      let startSlot = varStackSlot $ lookupVarRecord symTable ident
      let allSlots = take (numSlotsDim dim) [startSlot..]
      mapM_ (\slot -> instr (StoreInstr slot (Reg 0))) allSlots


-- genCodeStmt
-- Action to generate code for a single Goat Statement (may be an atomic or
-- composite statement).
genCodeStmt :: ProcSymTable -> VarSymTable -> Stmt -> CodeGen ()

genCodeStmt _ varSymTable stmt@(WriteExpr expr _)
  = do
      comment $ init $ prettify stmt
      genCodeExprInto varSymTable (Reg 0) expr
      case getExprType varSymTable expr of
        BoolType -> instr $ CallBuiltinInstr PrintBool
        IntType -> instr $ CallBuiltinInstr PrintInt
        FloatType -> instr $ CallBuiltinInstr PrintReal

genCodeStmt _ _ stmt@(WriteString str _)
  = do
      comment $ init $ prettify stmt
      instr $ StringConstInstr (Reg 0) str
      instr $ CallBuiltinInstr PrintStr

genCodeStmt _ varSymTable stmt@(Read scalar _)
  = do
      comment $ init $ prettify stmt
      let record = lookupVarRecord varSymTable (scalarIdent scalar)
      case varType record of
        BoolType -> instr $ CallBuiltinInstr ReadBool
        IntType -> instr $ CallBuiltinInstr ReadInt
        FloatType -> instr $ CallBuiltinInstr ReadReal
      genCodeStore varSymTable scalar (Reg 0)

genCodeStmt _ varSymTable stmt@(Asg scalar expr _)
  = do
      comment $ init $ prettify stmt
      genCodeExprInto varSymTable (Reg 0) expr
      let record = lookupVarRecord varSymTable (scalarIdent scalar)
      when (varType record == FloatType) $
        realify (Reg 0) (getExprType varSymTable expr)
      genCodeStore varSymTable scalar (Reg 0)

genCodeStmt procSymTable varSymTable (If cond thenStmts _)
  = do
      comment $ "if " ++ prettify cond
      genCodeExprInto varSymTable (Reg 0) cond
      fiLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) fiLabel
      mapM_ (genCodeStmt procSymTable varSymTable) thenStmts
      label $ fiLabel

genCodeStmt procSymTable varSymTable (IfElse cond thenStmts elseStmts _)
  = do
      comment $ "if-else " ++ prettify cond
      genCodeExprInto varSymTable (Reg 0) cond
      elseLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) elseLabel
      mapM_ (genCodeStmt procSymTable varSymTable) thenStmts
      fiLabel <- getNewBlockLabel
      instr $ BranchUncondInstr fiLabel
      label $ elseLabel
      mapM_ (genCodeStmt procSymTable varSymTable) elseStmts
      label $ fiLabel

genCodeStmt procSymTable varSymTable (While cond stmts _)
  = do
      comment $ "while " ++ prettify cond
      whileLabel <- getNewBlockLabel
      label $ whileLabel
      genCodeExprInto varSymTable (Reg 0) cond
      odLabel <- getNewBlockLabel
      instr $ BranchOnFalseInstr (Reg 0) odLabel
      mapM_ (genCodeStmt procSymTable varSymTable) stmts
      label $ odLabel

genCodeStmt procSymTable varSymTable stmt@(Call ident@(Id procName _) args _)
  = do
      comment $ init $ prettify stmt
      let procRecord = lookupProcRecord procSymTable ident
      let params = procParams procRecord
      sequence_ $ zipWith3 (genCodeArgInto varSymTable) [Reg 0..] params args
      instr $ CallInstr $ ProcLabel procName

-- genCodeStore
-- Generate the code to store the value in a register into the stack slot
-- corresponding to a scalar.
genCodeStore :: VarSymTable -> Scalar -> Reg -> CodeGen ()

-- Single variable scalars may have been passed by reference, in which case we
-- need to store them indirectly using the _address_ in the local stack slot.
-- In contrast for scalars passed by value, we simply store the value from the
-- register to the stack slot.
genCodeStore varSymTable (Single ident _) reg
  = do
      let record = lookupVarRecord varSymTable ident
      let passBy = varPassBy record
      let slot = varStackSlot record
      let nextReg = succ reg
      case passBy of
        Val -> instr $ StoreInstr slot reg
        Ref -> do
            instr $ LoadInstr nextReg slot
            instr $ StoreIndirectInstr nextReg reg

-- Arrays and Matrices cannot be passed by reference but we need to calculate
-- the correct stack slot based on an offset given by the index expression(s).
genCodeStore varSymTable scalar reg
  = do
      genCodeOffsetAddrInto varSymTable (succ reg) scalar
      instr $ StoreIndirectInstr (succ reg) reg


-- genCodeArgInto
-- Generate the code to load the expression (that is referenced by Param)
-- into the given register.
genCodeArgInto :: VarSymTable -> Reg -> Param -> Expr -> CodeGen ()

-- It's easy to prepare something to be passed by value, we can just use
-- genCodeExprInto:
genCodeArgInto varSymTable reg (Param Val _ _ _) expr
  = genCodeExprInto varSymTable reg expr

-- For pass by reference, the expression must be an lvalue, and we need to
-- ensure that we load its address into the register instead.

-- For Single variables, the stack slot may ALREADY hold an address; in which
-- case we can just copy that address into the register. If it's a value we
-- need to load its address instead.
genCodeArgInto varSymTable reg (Param Ref _ _ _) (ScalarExpr _ (Single ident _))
  = do
      let record = lookupVarRecord varSymTable ident
      let slot = varStackSlot record
      let localPassBy = varPassBy record
      case localPassBy of
        Val -> instr $ LoadAddressInstr reg slot
        Ref -> instr $ LoadInstr reg slot

-- For non-Single variables (Arrays/Matrices) they must already be values (Goat
-- does not allow Array and Matrix variables to be passed by ref). Thus we just
-- need to calculate the address and load it into the register.
genCodeArgInto varSymTable reg (Param Ref _ _ _) (ScalarExpr _ scalar)
  = genCodeOffsetAddrInto varSymTable reg scalar


-- genCodeOffsetAddrInto
-- Given an Array or Matrix scalar, evaluate the one or two indices to determine
-- the offset and load the address at this offset into the given register.
genCodeOffsetAddrInto :: VarSymTable -> Reg -> Scalar -> CodeGen ()
genCodeOffsetAddrInto varSymTable reg (Array ident exprI _)
  = do
      let record = lookupVarRecord varSymTable ident
      -- calculate offset into next register
      genCodeExprInto varSymTable reg exprI
      -- put the start slot address in the next register
      let startSlot = varStackSlot record
      instr $ LoadAddressInstr (succ reg) startSlot
      -- calculate address of indexed scalar
      instr $ SubOffsetInstr reg (succ reg) reg

genCodeOffsetAddrInto varSymTable reg (Matrix ident exprI exprJ _)
  = do
      let record = lookupVarRecord varSymTable ident
      -- start by calculating the offset into a register
      -- first, the column offset
      let (Dim2 _ cols) = varShape record
      instr $ IntConstInstr reg cols
      genCodeExprInto varSymTable (succ reg) exprI
      instr $ MulIntInstr reg reg (succ reg)
      -- plus the row offset
      genCodeExprInto varSymTable (succ reg) exprJ
      instr $ AddIntInstr reg reg (succ reg)
      -- then, put the matrix's start slot address in the next register
      let startSlot = varStackSlot record
      instr $ LoadAddressInstr (succ reg) startSlot
      -- calculate address of indexed scalar
      instr $ SubOffsetInstr reg (succ reg) reg


-- genCodeExprInto register
-- Action to generate code that will get the result of an expression into this
-- register.
genCodeExprInto :: VarSymTable -> Reg -> Expr -> CodeGen ()
-- TODO: The following code assumes that the expression is well-typed. Semantic
-- analysis will need to come in and actually provide that guarantee at some
-- point.

-- Base cases: float, int and bool constants:

genCodeExprInto _ register (IntConst _ int)
  = instr $ IntConstInstr register int

genCodeExprInto _ register (FloatConst _ float)
  = instr $ RealConstInstr register float

-- In Oz we represent True as the integer 1, and false as the integer 0
genCodeExprInto _ register (BoolConst _ True)
  = instr $ IntConstInstr register 1
genCodeExprInto _ register (BoolConst _ False)
  = instr $ IntConstInstr register 0


-- Recursive cases: binary and unary operations involving nested expressions:

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
genCodeExprInto varSymTable register (BinExpr _ And l r)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto varSymTable register l
      instr $ BranchOnFalseInstr register afterLabel
      genCodeExprInto varSymTable register r
      label afterLabel

-- Or is similar (but we skip the second operand when the first is True,
-- rather than False):
genCodeExprInto varSymTable register (BinExpr _ Or l r)
  = do
      afterLabel <- getNewBlockLabel
      genCodeExprInto varSymTable register l
      instr $ BranchOnTrueInstr register afterLabel
      genCodeExprInto varSymTable register r
      label afterLabel

-- All other expressions are strict, so we can safely load the operands into
-- two registers then just perform the appropriate operation:
genCodeExprInto varSymTable register (BinExpr _ op lExpr rExpr)
  = do
      genCodeExprInto varSymTable register lExpr
      genCodeExprInto varSymTable (succ register) rExpr
      genCodeBinOp register register (succ register) op lType rType
  where
    lType = getExprType varSymTable lExpr
    rType = getExprType varSymTable rExpr

-- There are only two cases for unary operations:
-- Logical `Not`:
genCodeExprInto varSymTable register (UnExpr _ Not expr)
  = do
      genCodeExprInto varSymTable register expr
      instr $ NotInstr register register

-- And arithmetic `Neg` (which could be applied to either a real value or an
-- int value):
genCodeExprInto varSymTable register (UnExpr _ Neg expr)
  = do
      genCodeExprInto varSymTable register expr
      instr $ instruction register register
  where
    instruction = case getExprType varSymTable expr of
      FloatType -> NegRealInstr
      IntType -> NegIntInstr


-- Scalar expressions:

-- For scalar expressions of Single variables we need to be careful about
-- whether the stack slot holds a reference (for pass by reference variables)
-- or a value (for pass by value variables and local variables).
genCodeExprInto varSymTable reg (ScalarExpr _ (Single ident _))
  = do
      let record = lookupVarRecord varSymTable ident
      let slot = varStackSlot record
      let passBy = varPassBy record
      case passBy of
        Val -> instr $ LoadInstr reg slot
        Ref -> do
            instr $ LoadInstr reg slot
            instr $ LoadIndirectInstr reg reg

-- All non-Single variables (Arrays/Matrices) must already be values, but we
-- will need to generate code for caulcating the scalar address offset from the
-- variable's start address by the result of some integer expression(s).
-- Luckily, for both Arrays an Matrices, the pattern of loading the value given
-- the offset address is similar. See `genCodeOffsetAddrInto` for computing the
-- offset address.
genCodeExprInto varSymTable reg (ScalarExpr _ scalar)
  = do
      -- go and calculate the offset-address into the register
      genCodeOffsetAddrInto varSymTable reg scalar
      -- then load into the register the value stored at that address
      instr $ LoadIndirectInstr reg reg


-- genCodeBinOp
-- Action to generate code for an arbitrary binary operation from two registers
-- into a third destination register, given the known types of the values in the
-- two
genCodeBinOp :: Reg -> Reg -> Reg -> BinOp -> BaseType -> BaseType -> CodeGen ()

-- If both
genCodeBinOp destReg lReg rReg op IntType IntType
  = instr $ (lookupOpInt op) destReg lReg rReg

genCodeBinOp destReg lReg rReg op BoolType BoolType
  = instr $ (lookupOpBool op) destReg lReg rReg

genCodeBinOp destReg lReg rReg op FloatType FloatType
  = instr $ (lookupOpReal op) destReg lReg rReg

-- Finally, maybe we have one int value and one real value:
genCodeBinOp destReg lReg rReg op lType rType
  = do
      -- ensure both arguments are formatted as reals
      realify lReg lType
      realify rReg rType
      instr $ (lookupOpIntOrReal op) destReg lReg rReg

-- realify
-- Action to generate code to cast an integer value to a real, if necessary
-- (this is a noop if the BaseType is FloatType).
realify :: Reg -> BaseType -> CodeGen ()
realify _ FloatType
  = return ()
realify register IntType
  = instr $ IntToRealInstr register register
-- TODO (semantic analysis):
-- Annotate the AST with additional 'float cast' nodes to avoid the need for
-- this case. E.g. an annotated AST requiring a cast, such as for the
-- expression `4 + 0.2`, could look like this:
-- ```
-- Add FloatType
--   (FloatCast (IntConst 4))
--   (FloatConst 0.2)
-- ```
--
-- Then all we'd need would be:
--
-- ```
-- genCodeExprInto register (FloatCase expr)
--   = do
--       genCodeExprInto register expr
--       instr $ IntToRealInstr register register
-- ```


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

-- Look up the appropriate Oz Instruction for an int and a real argument
-- (NOTE: Equ and NEq are not allowed!)
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

-- Recursively (re-)compute the type of
-- TODO: Of course, we will want to change from using a recursive function to
-- calculate expression types to precomputing these types during semantic
-- analysis and embedding them within the Expression AST nodes themselves.
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
