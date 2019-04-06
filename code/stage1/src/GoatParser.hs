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
                  -- other operators: (arranged by precedence low to high)
                  , "||"                            -- disjunction (left assoc)
                  , "&&"                            -- conjunction (left assoc)
                  , "!"                             -- negation (unary, prefix)
                  , "=", "!=", "<", "<=", ">", ">=" -- relational (non-assoc)
                  , "+", "-"                        -- arithmetic binary ops
                  , "*", "/"                        -- (all left assoc)
                  , "-"                             -- negative (unary, prefix)
                  ]
languageDef
  = emptyDef { Token.commentLine     = "#"
             , Token.identStart      = letter                  -- [a-zA-Z]
             , Token.identLetter     = alphaNum <|> oneOf "_'" -- [a-zA-Z0-9_']
             , Token.opLetter        = oneOf ":=|&!<>+-*/"     -- [:=|&!<>+-*/]
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

-- parseProgram
-- top level parser for an entire program, including (eating leading whiteSpace 
-- as required by parsec's lexeme parser approach, and requiring no trailing 
-- input after the program is parsed):
parseProgram :: Parser GoatProgram
parseProgram
  = between whiteSpace eof parseGoatProgram

-- after that, we'll just need (roughly) one parser per grammar non-terminal
-- (see grammar.txt).

-- GOAT       -> PROC+
pGoatProgram :: Parser GoatProgram
pGoatProgram
  = do
      procs <- many1 pProc
      return (GoatProgram procs)

-- PROC       -> "proc" id "(" PARAMS ")" DECL* "begin" STMT+ "end"
-- PARAMS     -> (PARAM ",")* PARAM | ε               <-- `commaSep` combinator
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

-- PARAM      -> PASSBY TYPE id
pParam :: Parser Param
pParam
  = do
      passBy <- pPassBy
      baseType <- pBaseType
      name <- identifier
      return (Param passBy baseType name)

-- PASSBY     -> "val" | "ref"
pPassBy :: Parser PassBy
pPassBy
  =   reserved "val" >> return Val -- TODO: Is there a cleaner way to go from
  <|> reserved "ref" >> return Ref -- reserved words to constants?
                                   -- Consider instancing the Read typeclass?

-- TYPE       -> "bool" | "float" | "int"
pBaseType :: Parser BaseType
pBaseType
  =   reserved "bool"  >> return BoolType  -- TODO: See above.
  <|> reserved "float" >> return FloatType
  <|> reserved "int"   >> return IntType

-- DECL       -> TYPE id DIM ";"
pDecl :: Parser Decl
pDecl
  = do
      baseType <- pBaseType
      name <- identifier
      dim <- pDim -- see the parser pDim much later in this file
      semi
      return (Decl baseType name dim)


-- STMT       -> ASGN | READ | WRITE | CALL | IF | WHILE 
pStmt :: Parser Stmt
pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIf, pWhile]

-- Each of these statement helper parsers also return Stmts:
pAgn, pRead, pWrite, pCall, pIf, pWhile :: Parser Stmt

-- ASGN       -> VAR ":=" EXPR ";"
pAsg
  = do
      var <- pVar
      reservedOp ":="
      expr <- pExpr
      semi
      return (Asg var expr)

-- READ       -> "read" VAR ";"
pRead
  = do
      reserved "read"
      var <- pVar
      semi
      return (Read var)

-- WRITE      -> "write" EXPR ";"
pWrite
  = do
      reserved "write"
      expr <- pExpr
      semi
      return (Write expr)

-- CALL       -> "call" id "(" EXPRS ")" ";"
-- EXPRS      -> (EXPR ",")* EXPR | ε                 <-- `commaSep` combinator
pCall
  = do
      reserved "call"
      name <- identifier
      args <- parens (commaSep pExpr)
      semi
      return (Call name args)

-- IF         -> "if" EXPR "then" STMT+ OPT_ELSE "fi" 
-- OPT_ELSE   -> "else" STMT+ | ε
pIf
  = do
      reserved "if"
      cond <- pExpr
      reserved "then"
      thenStmts <- many1 pStmt
      maybeElseStmts <- optionMaybe (reserved "else" >> many1 pStmt)
      reserved "fi"
      case maybeElseStmts of
        Nothing        -> return (If cond thenStmts)
        Just elseStmts -> return (IfElse cond thenStmts elseStmts)

-- WHILE      -> "while" EXPR "do" STMT+ "od"
pWhile
  = do
      reserved "while"
      cond <- pExpr
      reserved "do"
      stmts <- many1 pStmt
      reserved "od"
      return (While cond stmts)



-- Now, for capturing the similarity that exists between the VAR and DIM rules:
-- 
-- The Grammar rules:
-- 
--   (1) DIM        -> ε | "[" int  "]" | "[" int  "," int  "]"
--   (2) SUBSCRIPT  -> ε | "[" EXPR "]" | "[" EXPR "," EXPR "]"
--
-- obviously have very similar structure. They are both of the form:
--
--   Z_a            -> ε | "[" a "]" | "[" a "," a "]"
--
-- where a is either an int or an expression.
--
-- It's be nice to capture this similarity in some kind of parser combinator
-- (parametrised by a parser for a) to avoid repeating code!
--
-- Note that (1) represents possibly resizing a variable according to some
-- integer dimensions (simensionality, or shape), while (2) represents looking 
-- within a possibly large structure for a partciular element (indexing or 
-- subscripting):
--
--     X[12,7]---------------->                 .----------------------.
--     |                      |                 |     |                |
--     |                      |                 |     v                |
--     |                      |                 |---->X[3,2]           |
--     |                      |                 |                      |
--     |                      |                 |                      |
--     |                      |                 |                      |
--     V______________________|                 |______________________|
--               (1)                                      (2)
--
-- In the first case, we are 'zooming out' the variable to become an array /
-- matrix of variables. In the second case, we are 'zooming in' to a particular 
-- element of such an array / matrix.
-- 
-- So... we shall name this [,] construct our 'zoom'!..  ...!?
--                                                       any other suggestions?
-- 
--   ZOOM_a         -> ε | "[" a "]" | "[" a "," a "]"
-- 
-- Left-factoring these productions leads to the following:
-- 
--   ZOOM_a         -> ε | "[" a ZOOM1_a "]"
--   ZOOM1_a        -> "," a | ε
-- 
-- We propose the following parser combinator; the 'zoom parser':


-- zoom
-- "To _zoom_ a parser, possibly look for brackets containing one occurrence of 
-- the thing, followed by a possible second occurrence."
-- Return either Nothing or Just [x] or Just [x,y] with the 0, 1 or 2 results.
zoom :: (Parser a) -> Parser (Maybe [a])
zoom parser
  = optionMaybe $ brackets (parser <:> optionList (comma >> parser))
--                                 /   `--,-----------------------'
--                              .-'      ;
-- (<:>) 'applicative cons' operator     |
-- mnemonic shortcut for using cons as an|applicative function
(<:>) x xs            --                 |
  = (:) <$> x <*> xs  --                 |
                      --                 |
-- optionList ---------------------------'
-- Apply a parser at most once, and keep its result in a list
-- [] for failure (without consuming input), or [result] for success
-- (This is very like Parsec's optionMaybe, but with [] and [result]
-- instead of Nothing and Just result)
optionList parser
  = option [] $ (:[]) <$> parser



-- DIM        -> ε | "[" int  "]" | "[" int  "," int  "]"
pDim :: Parser Dim
pDim
  = do
      size <- zoom integer
      case size of
        Nothing    -> return (Dim0)
        Just [n]   -> return (Dim1 n)
        Just [n,m] -> return (Dim2 n m)

-- VAR        -> id SUBSCRIPT
-- SUBSCRIPT  -> ε | "[" EXPR "]" | "[" EXPR "," EXPR "]"
pVar :: Parser Var
pVar
  = do
      name <- identifier
      subscript <- zoom pExpr
      case subscript of
        Nothing    -> return (Var0 name)
        Just [i]   -> return (Var1 name i)
        Just [i,j] -> return (Var2 name i j)

-- 
-- Notes:
-- * Using a list to represent 0, 1, or 2 return values is not so great. It'd 
--   perhaps be cleaner to use a new type for this purpose?
--   `data Zoom a = Zoom0 | Zoom1 a | Zoom2 a a` ? The definitions of Var and 
--   Dim could be refactored to have an accompanying `Zoom Expr` or `Zoom Int` 
--   too.
-- 


-- The above assumes a function named `pExpr :: Parser Expr` is defined below.


-- ----------------------------------------------------------------------------
-- Expression Parsing

