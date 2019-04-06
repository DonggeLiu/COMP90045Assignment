module GoatParser where

import GoatAST

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

type Parser a
   = Parsec String Int a

-- ----------------------------------------------------------------------------
-- Token parser generation

reservedNames = [ "begin", "bool", "call", "do", "else", "end", "false", "fi"
                , "float", "if", "int", "od", "proc", "read", "ref", "then"
                , "true", "val", "while", "write"
                ]
reservedOpNames = [ ":=" -- assignment
                  -- other operators:
                  , "||"
                  , "&&"
                  , "!"
                  , "=", "!=", "<", "<=", ">", ">="
                  , "+", "-"
                  , "*", "/"
                  , "-"
                  ]
languageDef
  = emptyDef { Token.commentLine     = "#"
             , Token.identStart      = letter                  -- [a-zA-Z]
             , Token.identLetter     = alphaNum <|> oneOf "_'" -- [a-zA-Z0-9_']
             , Token.opLetter        = oneOf "<-&*!|/>+=:"
             , Token.reservedNames   = reservedNames
             , Token.reservedOpNames = reservedOpNames
             }

lexer = Token.makeTokenParser languageDef

-- Token and lexer help us define a bunch of helpful parsers for tokens:

semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace
comma      = Token.comma      lexer -- parse a single comma
identifier = Token.identifier lexer -- parses an identifier

reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- partses an operator

lexeme     = Token.lexeme     lexer -- parse and then consume trailling spaces
parens     = Token.parens     lexer -- parse inside parens
brackets   = Token.brackets   lexer -- parse inside brackets
commaSep   = Token.commaSep   lexer -- parse a comma-separated list


-- TODO: replace these with our own parsers that do not permit non-digits
-- like in 0x42 and 3.14e-7 (and escape sequences for strings)
integer       = Token.integer       lexer
float         = Token.float         lexer
stringLiteral = Token.stringLiteral lexer
-- E.g.:
-- digit = oneOf "0123456789"
-- integer = (lexeme $ many1 digit) >>= (\s -> return (read s :: Int))
-- float   = (lexeme $ (many1 digit >> char "." >> many1 digit))
-- Actually Token.decimal behaves like we want integer to behave.

-- ----------------------------------------------------------------------------
-- Program parsing

parseGoatProgram :: Parser GoatProgram
parseGoatProgram
  = do
      whiteSpace
      procs <- many1 pProc
      eof
      return (GoatProgram procs)

pProc :: Parser Proc
pProc
  = do
      reserved "proc"
      name <- identifier
      params <- parens (commaSep pParam)
      decls <- many pDecl
      reserved "begin"
      stmts <- many1 pStmt
      reserved "end"
      return (Proc name params decls stmts)

pParam :: Parser Param
pParam
  = do
      passBy <- pPassBy
      baseType <- pBaseType
      name <- identifier
      return (Param passBy baseType name)

pPassBy :: Parser PassBy
pPassBy
  =   reserved "val" >> return Val
  <|> reserved "ref" >> return Ref

pBaseType :: Parser BaseType
pBaseType
  =   reserved "bool"  >> return BoolType
  <|> reserved "float" >> return FloatType
  <|> reserved "int"   >> return IntType

pDecl :: Parser Decl
pDecl
  = do
      baseType <- pBaseType
      name <- identifier
      dim <- pDim
      return (Decl baseType name dim)

-- this is a trickier one...?
pDim :: Parser Dim
pDim
  = do
      -- TODO:
      -- return Dim0 or Dim1 n or Dim2 n m
      return Dim0

pStmt :: Parser Stmt
pAgn, pRead, pWrite, pCall, pIf, pWhile :: Parser Stmt
pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIf, pWhile]

pAsg
  = do
      var <- pVar
      reservedOp ":="
      expr <- pExpr
      semi
      return (Asg var expr)
pRead
  = do
      reserved "read"
      var <- pVar
      semi
      return (Read var)
pWrite
  = do
      reserved "write"
      expr <- pExpr
      semi
      return (Write expr)
pCall
  = do
      reserved "call"
      name <- identifier
      args <- parens (commaSep pExpr)
      semi
      return (Call name args)
pIf
  = do
      -- TODO:
      return ()
pWhile
  = do
      reserved "while"
      expr <- pExpr
      reserved "do"
      stmts <- many1 stmt -- TODO: check many1 right!?
      reserved "od"
      return (While expr stmt)

pVar :: Parser Var
pVar
  = do
    -- TODO:
    return ()


-- assumes a function named `pExpr :: Parser Expr` is defined below

-- ----------------------------------------------------------------------------
-- Expression Parsing

