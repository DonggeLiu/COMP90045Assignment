module GoatLang.Parser where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                      GOAT - LANGUAGE PARSER USING PARSEC
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

import Text.Parsec
import Text.Parsec.Expr

import Util.Combinators (withPosition)

import GoatLang.AST
import GoatLang.Token

-- ----------------------------------------------------------------------------
-- Program parsing
-- ----------------------------------------------------------------------------

-- parseProgram
-- entry-point: Parse a full Goat program from a source code String. The
-- filepath is only used for error messages, not for IO, and can be "".
parseProgram :: FilePath -> String -> Either ParseError GoatProgram
parseProgram filePath input
  = parse pFullProgram filePath input

-- pFullProgram
-- top level parser for an entire program (including eating leading whiteSpace
-- as required by parsec's lexeme parser approach, and requiring no trailing
-- garbage after the program is parsed):
pFullProgram :: Parser GoatProgram
pFullProgram
  = between whiteSpace eof pGoatProgram

-- after that, we'll just need (roughly) one parser per grammar non-terminal
-- (see grammar.txt in the same directory as this file).

-- GOAT        -> PROC+
pGoatProgram :: Parser GoatProgram
pGoatProgram
  = do
      procs <- many1 pProc
      return (GoatProgram procs)


-- PROC        -> "proc" id "(" PARAMS ")" DECL* "begin" STMT+ "end"
-- PARAMS      -> (PARAM ",")* PARAM | ε
pProc :: Parser Proc
pProc
  = withPosition ( do
      reserved "proc"
      ident <- pIdent
      params <- parens (commaSep pParam)
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (Proc ident params decls stmts)
    ) <?> "at least one procedure definition"
-- pProc is only ever called by pGoatProgram, and will only ever fail without
-- consuming any input if the file is blank; thus we can say 'expecting at
-- least one procedure definition' to give the programmer a hint about this 
-- requirement

pIdent :: Parser Id
pIdent
  = ( withPosition $ do
      name <- identifier
      return $ Id name
    ) <?> "identifier"

-- PARAM       -> PASSBY TYPE id
pParam :: Parser Param
pParam
  = (withPosition $ do
      passBy <- pPassBy
      baseType <- pBaseType
      ident <- pIdent
      return (Param passBy baseType ident)
  ) <?> "parameter"

-- PASSBY      -> "val" | "ref"
pPassBy :: Parser PassBy
pPassBy
  =   (reserved "val" >> return Val)
  <|> (reserved "ref" >> return Ref)

-- TYPE        -> "bool" | "float" | "int"
pBaseType :: Parser BaseType
pBaseType
  =   (reserved "bool"  >> return BoolType)
  <|> (reserved "float" >> return FloatType)
  <|> (reserved "int"   >> return IntType)

-- DECL        -> TYPE id DIM ";"
pDecl :: Parser Decl
pDecl
  = do
      declPos <- getPosition
      baseType <- pBaseType
      ident <- pIdent
      dim <- pDim
      semi
      return (Decl baseType ident dim declPos)
  <?> "declaration"

-- DIM         -> ε | "[" int  "]" | "[" int  "," int  "]"
pDim :: Parser Dim
pDim
  = do
      -- see 'suffixMaybe' combinator definition and motivation, far below
      size <- suffixMaybe integer
      case size of
        Nothing    -> return (Dim0)
        Just [n]   -> return (Dim1 n)
        Just [n,m] -> return (Dim2 n m)


-- STMT        -> ASGN | READ | WRITE | CALL | IF_OPT_ELSE | WHILE
pStmt :: Parser Stmt
pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIfOptElse, pWhile]
  <?> "statement (or, possibly, end-of-block keyword)"
-- NOTE: Unfortunately, due to the way 'identifier' is implemented and some
-- weird things about the internals of Parsecs, the "expected <end-of-block
-- keyword>" context is lost when parsec is parsing a block of statements
-- that is missing its end-of-block keyword, but followed by another reserved
-- word (such as a missing "fi" followed by an "od" or "end").
-- The simplest fix is to recreate the expected message here in the statement
-- parser. Note it's not always correct to say that we are expecting a keyword!
-- For example in a block with NO statements, we are not (yet) expecting the
-- end-of-block keyword. Thus we qualify the message with 'possibly' in the
-- hope that it will be more helpful to the programmer than the (incomplete)
-- "expecting statement".

-- Each of these statement helper parsers also return Stmts:
pAsg, pRead, pWrite, pCall, pIfOptElse, pWhile :: Parser Stmt

-- ASGN        -> SCALAR ":=" EXPR ";"
pAsg
  = do
      pos <- getPosition
      scalar <- pScalar
      reservedOp ":="
      expr <- pExpr
      semi
      return (Asg scalar expr pos)

-- READ        -> "read" SCALAR ";"
pRead
  = do
      pos <- getPosition
      reserved "read"
      scalar <- pScalar
      semi
      return (Read scalar pos)

-- WRITE       -> "write" EXPR_OR_STR ";"
-- EXPR_OR_STR -> EXPR | string
pWrite
  = do
      pos <- getPosition
      reserved "write"
      exprOrStr <- (fmap Left pExpr) <|> (fmap Right stringLiteral)
      semi
      case exprOrStr of
        Left  expr   -> return (WriteExpr expr pos)
        Right string -> return (WriteString string pos)

-- CALL        -> "call" id "(" EXPRS ")" ";"
-- EXPRS       -> (EXPR ",")* EXPR | ε
pCall
  = do
      callPos <- getPosition
      reserved "call"
      ident <- pIdent
      args <- parens (commaSep pExpr)
      semi
      return (Call ident args callPos)

-- IF_OPT_ELSE -> "if" EXPR "then" STMT+ OPT_ELSE "fi"
-- OPT_ELSE    -> "else" STMT+ | ε
pIfOptElse
  = do
      pos <- getPosition
      reserved "if"
      cond <- pExpr
      reserved "then"
      thenStmts <- many1 pStmt
      maybeElseStmts <- optionMaybe (reserved "else" >> many1 pStmt)
      reserved "fi"
      case maybeElseStmts of
        Nothing        -> return (If cond thenStmts pos)
        Just elseStmts -> return (IfElse cond thenStmts elseStmts pos)

-- WHILE       -> "while" EXPR "do" STMT+ "od"
pWhile
  = do
      pos <- getPosition
      reserved "while"
      cond <- pExpr
      reserved "do"
      doStmts <- many1 pStmt
      reserved "od"
      return (While cond doStmts pos)


-- SCALAR      -> id SUBSCRIPT
-- SUBSCRIPT   -> ε | "[" EXPR "]" | "[" EXPR "," EXPR "]"
pScalar :: Parser Scalar
pScalar
  = do
      pos <- getPosition
      ident <- pIdent
      -- see 'suffixMaybe' combinator definition and motivation, below
      subscript <- suffixMaybe pExpr
      case subscript of
        Nothing    -> return (Single ident pos)
        Just [i]   -> return (Array  ident i pos)
        Just [i,j] -> return (Matrix ident i j pos)
    <?> "scalar (variable element)"


-- Now, for capturing the similarity that exists between the SUBSCRIPT and DIM
-- rules!
--
-- The Grammar rules:
--
--   (1) DIM         -> ε | "[" int  "]" | "[" int  "," int  "]"
--   (2) SUBSCRIPT   -> ε | "[" EXPR "]" | "[" EXPR "," EXPR "]"
--
-- obviously have very similar structure. They are both of the form:
--
--   S_a             -> ε | "[" a "]" | "[" a "," a "]"
--
-- where a is either an int or an expression.
--
-- It's be nice to capture this similarity in some kind of parser combinator
-- (parametrised by a parser for a) to avoid repeating code!
--
-- Well, we can make one! Here it is:

-- suffixMaybe
-- (optionally) parse a suffix using provided parser and return either
-- * Nothing (if no suffix present), or
-- * Just [x] or Just [x,y] with the 1 or 2 results (if present)
suffixMaybe :: (Parser a) -> Parser (Maybe [a])
suffixMaybe parser
  = (optionMaybe $ brackets $ commaSepMN 1 2 parser)
  <?> "suffix (such as [0, 1])"
-- This parser uses commaSepMN, a general parser combinator defined in the
-- spirit of parsec's commaSep and commaSep1. It's defined in Util.Combinators.


-- ----------------------------------------------------------------------------
-- Expression Parsing
-- ----------------------------------------------------------------------------

-- Grammar for expressions:
--
-- NOTE: precedence and associativity rules not encoded in grammar; see spec.
-- in fact this grammar was left ambiguous (for brevity)
--
-- EXPR        -> SCALAR | CONST | "(" EXPR ")" | EXPR BINOP EXPR | UNOP EXPR
-- CONST       -> int | float | BOOL | string
-- BINOP       -> "+"  | "-"  | "*"  | "/"
--              | "="  | "!=" | "<=" | "<"  | ">"  | ">="
--              | "&&" | "||"
-- UNOP        -> "!"  | "-"
--


-- pExpr
-- Parse an expression into a tree with operations as nodes and terms as
-- leaves. We use parsec's buildeExpressionParser to automatically construct
-- this parser according to the rules for operations and the term parser below.
pExpr :: Parser Expr
pExpr
  = buildExpressionParser operatorTable pTerm
  <?> "expression"


-- pTerm
-- parse an expression 'term' (non-operation)
pTerm :: Parser Expr
pTerm
  =   parens pExpr
  <|> pScalarExpr
  <|> pIntOrFloatConst  -- integers and floats share a common prefix!
  <|> pBoolConst
  <?> "term (variable, literal, or parenthesised expression)"

pScalarExpr :: Parser Expr
pScalarExpr
  = do
      pos <- getPosition
      scalar <- pScalar
      return $ ScalarExpr pos scalar

pIntOrFloatConst :: Parser Expr
pIntOrFloatConst
  = do
      pos <- getPosition
      numberLiteral <- integerOrFloat
      case numberLiteral of
        Left int  -> return $ IntConst pos int
        Right flt -> return $ FloatConst pos flt

pBoolConst :: Parser Expr
pBoolConst
  = do
      pos <- getPosition
      choice [ reserved "true"  >> return (BoolConst pos True)
             , reserved "false" >> return (BoolConst pos False)
             ]

-- operatorTable
-- encodes rules for parsing operations
operatorTable = [ [ prefix "-" Neg ]
                , [ binary "*" Mul, binary "/" Div ]
                , [ binary "+" Add, binary "-" Sub ]
                , [ relation "=" Equ, relation "!=" NEq
                  , relation "<" LTh, relation "<=" LEq
                  , relation ">" GTh, relation ">=" GEq
                  ]
                , [ prefix "!" Not ]
                , [ binary "&&" And ]
                , [ binary "||" Or ]
                ]

-- The following helper functions help define the rows in the operator table:

-- prefix
-- shortcut to building table entries for unary prefix operators
prefix name op
  = Prefix (pUnOp name op)

-- binary
-- shortcut to building table entried for binary infix operators
-- (left associative)
binary name op
  = Infix (pBinOp name op) AssocLeft

-- prefix
-- shortcut to building table entried for binary infix relational operators
-- (not assocative)
relation name op
  = Infix (pBinOp name op) AssocNone

-- pUnOp
-- shortcut to build a parser for a particular expression operator
pUnOp :: String -> UnOp -> Parser (Expr -> Expr)
pUnOp name op
  =  do
      pos <- getPosition
      reservedOp name
      return $ UnExpr pos op

pBinOp :: String -> BinOp -> Parser (Expr -> Expr -> Expr)
pBinOp name op
  = do
      pos <- getPosition
      reservedOp name
      return $ BinExpr pos op