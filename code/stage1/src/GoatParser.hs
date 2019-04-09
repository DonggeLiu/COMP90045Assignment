module GoatParser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Control.Applicative (liftA2)


import GoatAST


type Parser a
   = Parsec String () a

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
stringLiteral = Token.stringLiteral lexer -- the name 'string' is taken
-- E.g.:                                  -- by Text.Parsec.Char I think.
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
  = between whiteSpace eof pGoatProgram

-- after that, we'll just need (roughly) one parser per grammar non-terminal
-- (see grammar.txt).

-- GOAT        -> PROC+
pGoatProgram :: Parser GoatProgram
pGoatProgram
  = do
      procs <- many1 pProc
      return (GoatProgram procs)

-- PROC        -> "proc" id "(" PARAMS ")" DECL* "begin" STMT+ "end"
-- PARAMS      -> (PARAM ",")* PARAM | ε               <-- `commaSep` combinator
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

-- PARAM       -> PASSBY TYPE id
pParam :: Parser Param
pParam
  = do
      passBy <- pPassBy
      baseType <- pBaseType
      name <- identifier
      return (Param passBy baseType name)

-- PASSBY      -> "val" | "ref"
pPassBy :: Parser PassBy
pPassBy
  =   (reserved "val" >> return Val) -- TODO: Is there a cleaner way to go from
  <|> (reserved "ref" >> return Ref) -- reserved words to constants?
                                   -- Consider instancing the Read typeclass?

-- TYPE        -> "bool" | "float" | "int"
pBaseType :: Parser BaseType
pBaseType
  =   (reserved "bool"  >> return BoolType)  -- TODO: See above.
  <|> (reserved "float" >> return FloatType)
  <|> (reserved "int"   >> return IntType)

-- DECL        -> TYPE id DIM ";"
pDecl :: Parser Decl
pDecl
  = do
      baseType <- pBaseType
      name <- identifier
      dim <- pDim
      semi
      return (Decl baseType name dim)

-- DIM         -> ε | "[" int  "]" | "[" int  "," int  "]"
pDim :: Parser Dim
pDim
  = do
      -- see 'suffixMaybe' combinator definition and motivation, far below
      -- TODO: FIX THIS UGLY HACK --. we need our version of integer to return
      -- Int directly?             ,'--------------------------------.
      size <- suffixMaybe (integer >>= (\i -> return (fromIntegral i)))
      case size of
        Nothing    -> return (Dim0)
        Just [n]   -> return (Dim1 n)
        Just [n,m] -> return (Dim2 n m)



-- STMT        -> ASGN | READ | WRITE | CALL | IF_OPT_ELSE | WHILE
pStmt :: Parser Stmt
pStmt
  = choice [pAsg, pRead, pWrite, pCall, pIfOptElse, pWhile]

-- Each of these statement helper parsers also return Stmts:
pAsg, pRead, pWrite, pCall, pIfOptElse, pWhile :: Parser Stmt

-- ASGN        -> VAR ":=" EXPR ";"
pAsg
  = do
      var <- pVar
      reservedOp ":="
      expr <- pExpr
      semi
      return (Asg var expr)

-- READ        -> "read" VAR ";"
pRead
  = do
      reserved "read"
      var <- pVar
      semi
      return (Read var)

-- WRITE       -> "write" EXPR ";"
pWrite
  = do
      reserved "write"
      expr <- pExpr
      semi
      return (Write expr)

-- CALL        -> "call" id "(" EXPRS ")" ";"
-- EXPRS       -> (EXPR ",")* EXPR | ε                 <-- `commaSep` combinator
pCall
  = do
      reserved "call"
      name <- identifier
      args <- parens (commaSep pExpr)
      semi
      return (Call name args)

-- IF_OPT_ELSE -> "if" EXPR "then" STMT+ OPT_ELSE "fi"
-- OPT_ELSE    -> "else" STMT+ | ε
pIfOptElse
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

-- WHILE       -> "while" EXPR "do" STMT+ "od"
pWhile
  = do
      reserved "while"
      cond <- pExpr
      reserved "do"
      doStmts <- many1 pStmt
      reserved "od"
      return (While cond doStmts)


-- VAR         -> id SUBSCRIPT
-- SUBSCRIPT   -> ε | "[" EXPR "]" | "[" EXPR "," EXPR "]"
pVar :: Parser Var
pVar
  = do
      name <- identifier
      -- see 'suffixMaybe' combinator definition and motivation, below
      subscript <- suffixMaybe pExpr
      case subscript of
        Nothing    -> return (Var0 name)
        Just [i]   -> return (Var1 name i)
        Just [i,j] -> return (Var2 name i j)



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
  = optionMaybe $ brackets $ commaSepMN 1 2 parser

-- This parser uses commaSepMN, a general parser combinator in the spirit of
-- parsec's commaSep and commaSep1. It's defined using the following general
-- combinators:

-- manyMN
-- Inspired by parsec's many and many1 combinators which gives us a way to
-- express Kleene * and +, this parser combinator gives us a way to express
-- minimum/maximum repetitions (that is, the {m, n} construct from regular
-- expressions).
-- Parse at least m and at most n occurrences of whatever p is looking for;
-- returning the result as a list.
-- Note: There may be more instances of p after this parser completes if it
-- found exactly n, these will be left to the next parser to parse (or reject).
manyMN :: Int -> Int -> (Parser a) -> (Parser [a])
manyMN m n p
  | m<0 || n<m = error "manyMN expects 0 <= m <= n"
  | n == 0     = return []
  | m == 0     = option [] $ p <:> manyMN 0 (n-1) p
  | otherwise  = p <:> manyMN (m-1) (n-1) p

-- sepByMN
-- Insipired by parsec's sepBy1 and sepBy, this combinator allows us to
-- look for a list of results from parser `p`, parsing a `sep` in between
-- each one.
-- It mainly relies on `manyMN (m-1) (n-1) (sep >> p)' but we must take
-- some care to treat the first instance correctly (it may be not allowed,
-- optional, or mandatory depending on m and n) and set off the right call
-- to manyMN.
sepByMN :: (Parser a) -> Int -> Int -> (Parser b) -> (Parser [b])
sepByMN sep m n p
  | m<0 || n<m = error "sepByMN expects 0 <= m <= n"
  | n == 0     = return []
  | m == 0     = option [] $ p <:> manyMN 0 (n-1) (sep >> p)
  | otherwise  = p <:> manyMN (m-1) (n-1) (sep >> p)

-- commaSepMN
-- Inspired by parsec's commaSep parser combinator and reflecting common
-- usage of sepByMN, we provide this alias function to parse a comma-separated
-- list of some parser, between some minimum and maximum length.
commaSepMN :: Int -> Int -> (Parser a) -> Parser [a]
commaSepMN
  = sepByMN comma

-- (<:>)
-- 'applicative cons' operator - A mnemonic shortcut for using cons as an
-- applicative function
(<:>)
  = liftA2 (:) -- liftA2: it's just like: (<:>) a_x a_xs = (:) <$> a_x <*> a_xs





-- The above assumes a function named `pExpr :: Parser Expr` is defined below.

-- A dummy-definition for now:
pExpr = do
  var <- pVar
  return (VarExpr var)

-- ----------------------------------------------------------------------------
-- Expression Parsing

-- Parsing an expression for an arithmetic operator

pExpr :: Parser Expr
pExpr 
  = buildExpressionParser operatorTable pTerm

-- Defining the operator precedence, associativity and constructors for arithmetic
-- operators
operatorTable = [ [ prefix "-" Neg ]       
                , [ binary "*" Mul, binary "/" Div ]
                , [ binary "+" Add, binary "-" Sub ]
                , [ relation "=" Equ, relation "!=" NEq
                  , relation "<" LTh, relation "<=" LEq
                  , relation ">" GTh, relation ">=" GEq
                  ]
                , [ Prefix "!" Not ]
                , [ binary "&&" And ]
                , [ binary "||" Or ]
                ]

prefix name op
  = Prefix (reservedOp name  >> return (UnExpr op))

binary name op
  = Infix (reservedOp name >> return (BinExpr op)) AssocLeft

relation name op
  = Infix (reservedOp name   >> return (BinExpr op )) AssocNone

-- Defining the terms for Boolean operators
pBoolConst 
  =   (reserved "true"  >> return (BoolConst True ))
  <|> (reserved "false" >> return (BoolConst False))


pTerm = parens pExpr
      <|> pVarExpr
      <|> pBoolConst
      <|> pIntConst
      <|> pFloatConst
      <|> pStrConst

pIntConst = fmap IntConst integer

pFloatConst = fmap FloatConst float

pStrConst = fmap StrConst stringLiteral

pVarExpr = fmap VarExpr pVar
