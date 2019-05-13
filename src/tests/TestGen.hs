import Data.Either (isLeft)

import Test.HUnit

import Text.Parsec (parse, eof, ParseError)

import GoatLang.AST
import GoatLang.Parser
import GoatLang.PrettyPrint
import GoatLang.Token

-- A ParserUnitTest is a collection of test cases assocated with a single parser.
data ParserUnitTest node
  = ParserUnitTest (Parser node) [ParserTestCase node]

-- A ParserTestCase is an expected output corresponding to a list of inputs.
data ParserTestCase node
  = ParserTestCase (ParseResult node) [GoatInput]

-- A ParseResult corresponds to either a parsing error or an ASTNode.
data ParseResult node
  = ParseFailure
  | ParseSuccess node

type GoatInput = String

generateParserUnitTest :: (Eq node, Show node) => ParserUnitTest node -> Test
generateParserUnitTest (ParserUnitTest parser cases)
  = TestList (map (generateTestCase parser) cases)

generateTestCase :: (Eq node, Show node) => Parser node -> ParserTestCase node
  -> Test
generateTestCase parser (ParserTestCase result inputs)
  = TestList (map (generateAssertion parser result) inputs)

generateAssertion :: (Eq node, Show node) => Parser node -> ParseResult node
  -> GoatInput -> Test
generateAssertion parser result input
  = case result of
      ParseFailure -> TestCase $
        assertBool "" $ isLeft $ getParseResult parser input
      ParseSuccess node -> TestCase $ assertEqual "" (Right node) $
        getParseResult parser input

getParseResult :: Parser node -> GoatInput -> Either ParseError node
getParseResult parser input
  = parse (do {result <- parser; eof; return result}) "" input

--------------------------------------------------------------------------------

integerTest :: ParserUnitTest Int
integerTest
  = ParserUnitTest integer [
      ParserTestCase (ParseSuccess 42)
        ["42", "042", "0042", "00042"]
    , ParserTestCase (ParseSuccess 0)
        ["0", "00", "000"]
    , ParserTestCase (ParseSuccess 1234567890)
        ["01234567890"]
    , ParserTestCase ParseFailure
        ["", "4 2", "4,2", "0x42", "0xff", "4.2", "4."]
    ]

integerOrFloatTest :: ParserUnitTest (Either Int Float)
integerOrFloatTest
  = ParserUnitTest integerOrFloat [
      ParserTestCase (ParseSuccess $ Left 1234567890)
        ["01234567890"]
    , ParserTestCase (ParseSuccess $ Left 42)
        ["42", "042", "0042", "00042"]
    , ParserTestCase (ParseSuccess $ Left 0)
        ["0", "00", "000"]
    , ParserTestCase (ParseSuccess $ Right 12345.06789)
        ["12345.06789", "0012345.06789000"]
    , ParserTestCase (ParseSuccess $ Right 0.0)
        ["0.0", "00.00", "000.000"]
    , ParserTestCase ParseFailure
        [ "", "4 2", "4,2", "0x42", "0xff", ".0", "4.", "4.2.", "4.2.3"
        , "0.", "0. 0", "1e3", "1E3", "1E-3", "1E+4", "0x42"
        ]
    ]

stringLiteralTest :: ParserUnitTest String
stringLiteralTest
  = ParserUnitTest stringLiteral
    [ ParserTestCase (ParseSuccess "hello") ["\"hello\""]
    , ParserTestCase (ParseSuccess "hello\n") ["\"hello\\n\""]
    , ParserTestCase (ParseSuccess "hello\\t") ["\"hello\\t\""]
    , ParserTestCase (ParseSuccess "hello\\") ["\"hello\\\""]
    , ParserTestCase (ParseSuccess "hello\\world") ["\"hello\\world\""]
    , ParserTestCase ParseFailure
      ["\"hello\n\"", "\"hello\t\"", "\"hello", "hello\"", "\"hello\"\""]
    ]

pExprTest :: ParserUnitTest Expr
pExprTest
  = ParserUnitTest pExpr
    [ ParserTestCase ParseFailure
      [ "+", "-", "*", "/", "=", ">", ">=", "<", "<=", "&&", "||", "!"
      , "1 2", "3.14 2.72", "1 +", "1 -", "1 *", "1 /", "1 =", "1 >", "1 >="
      , "1 <", "1 <=", "1 &&", "1 ||", "1 !", "true 2", "true false", "true +"
      , "true -", "true *", "true /", "true =", "true >", "true >=", "true <"
      , "true <=", "true &&", "true ||", "true !", "1 = 2 = 3", "1 < 2 < 3"
      , "1 < 2 = 3", "1 = 2 < 3", "1 <= 2 <= 3", "1 >= 2 >= 3", "1 + + 3"
      , "true && && false", "true || || false", "true || && false", "1 true"
      , "true 1"]
    ]

pAsgTest :: ParserUnitTest Stmt
pAsgTest
  = ParserUnitTest pAsg
    [ ParserTestCase ParseFailure
      [ "var = 1;", "var :=;", ":= 42;", "var := read;"
      , "foo < bar := true;", "var +:= 1;", "\"var\" := true;", "true := var;"
      , "[] := 42;", "[var] := 42;", "var[] := 42;", "var[][] := 42;"
      , "var[[0]] := 42;", "var := call factorial(n);", "var := factorial(n);"
      , "var := main();", "var := +;", "-var := 42;", "!var := false;"
      ]
    ]

main
  = runTestTT $ TestList [
      generateParserUnitTest integerTest
    , generateParserUnitTest integerOrFloatTest
    , generateParserUnitTest stringLiteralTest
    , generateParserUnitTest pExprTest
    , generateParserUnitTest pAsgTest
    ]
