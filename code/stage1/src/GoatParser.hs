module GoatParser where

import GoatAST

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

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



-- ----------------------------------------------------------------------------
-- Expression Parsing

-- Parsing an expression for an arithmetic operator
arithmeticExpr :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm


-- Parsing an expression for a boolean operator
bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

-- Defining the operator precedence, associativity and constructors for arithmetic
-- operators
aOperators = [ [  Prefix (reservedOp "-"   >> return (UnOp Neg        ))        ]
             , [  Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft
                , Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft ]
             , [  Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft
                , Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft ]
              ]

-- Defining the operator precedence, associativity and constructors for boolean
-- operators
bOperators = [ [Prefix (reservedOp "!"  >> return (UnOp Not      ))          ]
             , [Infix  (reservedOp "&&" >> return (BinOp And     )) AssocLeft]
             , [Infix  (reservedOp "||" >> return (BinOp Or      )) AssocLeft]
             ]

-- Defining the terms for arithmetic operators
aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

-- Defining the terms for Boolean operators
bTerm =  parens bExpression
      <|> (reserved "true"  >> return (BoolType True ))
      <|> (reserved "false" >> return (BoolType False))
      <|> rExpression

-- Parser to handle relational expressions
rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2


relation =  (reservedOp "="  >> return (BinOp Equ ))
        <|> (reservedOp "!=" >> return (BinOp NEq ))
        <|> (reservedOp "<"  >> return (BinOp LTh ))
        <|> (reservedOp "<=" >> return (BinOp LEq ))
        <|> (reservedOp ">"  >> return (BinOp GTh ))
        <|> (reservedOp ">=" >> return (BinOp GEq ))