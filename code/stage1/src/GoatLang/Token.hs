module GoatLang.Token where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Util.Combinators

import GoatLang.AST

-- ----------------------------------------------------------------------------
-- Token parser generation

reservedNames = [ "begin", "bool", "call", "do", "else", "end", "false", "fi"
                , "float", "if", "int", "od", "proc", "read", "ref", "then"
                , "true", "val", "while", "write"
                ]
reservedOpNames = [ ":=" -- assignment
                  -- other operators: (arranged by precedence, high to low)
                  , "-"                             -- negative (unary, prefix)
                  , "*", "/"                        -- arithmetic binary ops
                  , "+", "-"                        -- (all left assoc)
                  , "=", "!=", "<", "<=", ">", ">=" -- relational (non-assoc)
                  , "!"                             -- negation (unary, prefix)
                  , "&&"                            -- conjunction (left assoc)
                  , "||"                            -- disjunction (left assoc)
                  ]
languageDef
  = emptyDef { Token.commentLine     = "#"
             , Token.identStart      = letter                  -- [a-zA-Z]
             , Token.identLetter     = alphaNum <|> oneOf "_'" -- [a-zA-Z0-9_']
             , Token.opLetter        = oneOf ":=|&!<>+-*/"     -- [:=|&!<>+-*/]
             , Token.reservedNames   = reservedNames           -- see above.
             , Token.reservedOpNames = reservedOpNames         -- see above.
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


-- Unfortunately, the out-of-box parsers for natural integers, floats, and
-- string literals are too permissive and allow literals that would be legal
-- in Haskell, but are not in Goat, such as 0x42, 3.14e2 and "hello\n" (with 
-- an actual newline '\n', as opposed to '\' followed by 'n'.)
-- 
-- Furthermore, natural/integer/decimal return an Integer rather than an Int,
-- and float returns a Double (despite its name) rather than a Float.
-- 
-- So, we will define our own lexeme parsers for parsing integers, floats and 
-- string literals!

-- integer
-- lexeme parser for non-negative decimal integers
integer :: Parser Int
integer
  = lexeme integer'

-- integer'
-- (non-lexeme) parser for non-negative decimal integers
integer' :: Parser Int
integer'
  = fmap read (many1 digit)


-- float
-- lexeme parser for plain floating point literals (without exponentials)
float :: Parser Float
float
  = lexeme float'

-- float'
-- (non-lexeme) parser for plain floating point literals (without exponentials)
float' :: Parser Float
float'
  = fmap read (many1 digit <++> (char '.' <:> many1 digit))


-- integers and floats share a common prefix, so this combination
-- parser may be useful in situations where we are expecting either
-- an integer or a float to appear:

-- integerOrFloat
-- lexeme parser for either an integer literal (left) or float literal (right)
integerOrFloat :: Parser (Either Int Float)
integerOrFloat
  = lexeme integerOrFloat'

-- integerOrFloat'
-- (non-lexeme) parser for either an int literal (left) or float literal (right)
integerOrFloat' :: Parser (Either Int Float)
integerOrFloat'
  = do
      whole <- many1 digit
      maybeDotFrac <- optionMaybe (char '.' <:> many1 digit)
      case maybeDotFrac of
        Nothing      -> return $ Left  $ read whole
        Just dotFrac -> return $ Right $ read (whole ++ dotFrac)


-- stringLiteral
-- lexeme parser for a string literal without internal newlines or tabs
stringLiteral :: Parser String
stringLiteral
  = lexeme stringLiteral'

-- stringLiteral
-- lexeme parser for a string literal without internal newlines or tabs
stringLiteral'
  = do
      char '"'
      contents <- many (noneOf "\"\n\t")
      char '"'
      return contents


-- Finally, we'll also want to define an enhanced version of commaSep
-- (based on the enhanced sepByMN combinator from Util.Combinators):

-- commaSepMN
-- Inspired by parsec's commaSep parser combinator and reflecting common
-- usage of sepByMN, we provide this alias function to parse a comma-separated
-- list of some parser, between some minimum and maximum length.
commaSepMN :: Int -> Int -> (Parser a) -> Parser [a]
commaSepMN
  = sepByMN comma
