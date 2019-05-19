import Data.Either (isLeft)

import Test.HUnit

import Text.Parsec (parse, eof, ParseError)

import Util.CodeWriter

import GoatLang.AST
import GoatLang.Parser
import GoatLang.PrettyPrint
import GoatLang.Token



-- A ParserUnitTest is a list of test cases assocated with a single parser.
data ParserUnitTest a
  = ParserUnitTest (Parser a) [ParserTestCase a]

-- A ParserTestCase is an expected output corresponding to a list of inputs.
data ParserTestCase a
  = ParserTestCase (ParseResult a) [GoatInput]

-- A ParseResult corresponds to either a parsing error or an ASTNode.
data ParseResult a
  = ParseFailure
  | ParseSuccess a

type GoatInput = String

-- A WriterUnitTest is a list of test cases assocated with a single writer.
data WriterUnitTest a
  = WriterUnitTest (a -> CodeWriter ()) [WriterTestCase a]

-- A ParserTestCase is an expected pretty output corresponding to an AST node.
data WriterTestCase a
  = WriterTestCase GoatOutput a

type GoatOutput = String

generateParserUnitTest :: (Eq a, Show a) => ParserUnitTest a -> Test
generateParserUnitTest (ParserUnitTest parser cases)
  = TestList (map (generateParserTestCase parser) cases)

generateParserTestCase :: (Eq a, Show a) => Parser a
  -> ParserTestCase a -> Test
generateParserTestCase parser (ParserTestCase result inputs)
  = TestList (map (generateParserAssertion parser result) inputs)

generateParserAssertion :: (Eq a, Show a) => Parser a
  -> ParseResult a -> GoatInput -> Test
generateParserAssertion parser result input
  = case result of
      ParseFailure -> TestCase $
        assertBool "" $ isLeft $ getParseResult parser input
      ParseSuccess result -> TestCase $ assertEqual "" (Right result) $
        getParseResult parser input

getParseResult :: Parser a -> GoatInput -> Either ParseError a
getParseResult parser input
  = parse (do {result <- parser; eof; return result}) "" input

generateWriterUnitTest :: (Eq a, Show a) => WriterUnitTest a -> Test
generateWriterUnitTest (WriterUnitTest writer cases)
  = TestList (map (generateWriterTestCase writer) cases)

generateWriterTestCase :: (Eq a, Show a) => (a -> CodeWriter ())
  -> WriterTestCase a -> Test
generateWriterTestCase writer (WriterTestCase expected input)
  = TestCase (assertEqual "" expected (writeCode $ writer input))

--------------------------------------------------------------------------------

integerTest :: ParserUnitTest Int
integerTest
  = ParserUnitTest integer
    [ ParserTestCase (ParseSuccess 42)
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
  = ParserUnitTest integerOrFloat
    [ ParserTestCase (ParseSuccess $ Left 1234567890)
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

pParamTest :: ParserUnitTest Param
pParamTest
  = ParserUnitTest pParam
    [ ParserTestCase (ParseSuccess $ Param Val BoolType (Id "a"))
      ["val bool a", "val  bool  a", "val\tbool\ta", "val\nbool\na"]
    , ParserTestCase ParseFailure
      [ "val val var", "val ref var", "ref val var", "ref ref var"
      , "bool val var", "bool ref var", "float val var", "float ref var"
      , "int val var", "int ref var  ", "VAL bool var", "REF bool var"
      , "Val bool var", "Ref bool var", "vAL bool var", "rEF bool var"
      , "bool bool var", "val' bool var", "ref' bool var", "val _ var"
      , "val bool _", "val _ _", "val bool true", "val bool \"hello\""
      , "val bool 42", "val bool 3.14"
      ]
    ]

pDimTest :: ParserUnitTest Dim
pDimTest
  = ParserUnitTest pDim
    [ ParserTestCase ParseFailure
      ["[]", "[][]", "[,]", "[ , ]", "[42][]", "[42,]", "[42, ]", "[][42]"
      , "[,42]", "[ ,42]", "[[]]", "[[]][[]]", "[[],[]]", "[][[]]", "[,[]]"
      , "[ ,[]]", "[[]][]", "[[],]", "[[], ]", "[[42]]", "[[42]][[42]]"
      , "[[42],[42]]", "[42][[42]]", "[42,[42]]", "[[42]][42]", "[[42],42]"
      , "[\"hello\"]", "[\"hello\"][\"harald\"]"
      , "[\"hello\",\"harald\"]", "[yarra][trams]", "[yarra,trams]"
      , "[\"COMP\"][90045]", "[\"COMP\",90045]", "[90045][\"COMP\"]"
      , "[90045,\"COMP\"]", "[1][2][3]", "[1,2,3]", "[3.14]", "[3.14][42]"
      , "[3.14,42]", "[42][3.14]", "[42,3.14]", "[3.14][2.72]", "[3.14,[2.72]"
      , "[42.]", "[.42]", "[42hello]", "[42_]", "[42+]"
      ]
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
      , "true 1"
      ]
    ]


pDeclTest :: ParserUnitTest Decl
pDeclTest
  = ParserUnitTest pDecl
    [ ParserTestCase ParseFailure
      [ "bool;", "bool _;", "bool ';", "bool (;", "bool );"
      , "bool [;", "bool ];", "bool +;", "bool -;", "bool *;", "bool /;"
      , "bool !;", "bool ||;", "bool &&;", "bool =;", "bool !=;", "bool <;"
      , "bool <=;", "bool >;", "bool >=;", "bool \"var\";", "bool _var;"
      , "bool 'var;", "bool 0var;", "bool var", "float;", "float _;", "float ';"
      , "float (;", "float );", "float [;", "float ];", "float +;", "float -;"
      , "float *;", "float /;", "float !;", "float ||;", "float &&;", "float =;"
      , "float !=;", "float <;", "float <=;", "float >;", "float >=;"
      , "float \"var\";", "float _var;", "float 'var;", "float 0var;"
      , "float var", "int;", "int _;", "int ';", "int (;", "int );"
      , "int [;", "int ];", "int +;", "int -;", "int *;", "int /;", "int !;"
      , "int ||;", "int &&;", "int =;", "int !=;", "int <;", "int <=;", "int >;"
      , "int >=;", "int \"var\";", "int _var;", "int 'var;", "int 0var;"
      , "int var", "bool begin;", "bool bool;", "bool call;", "bool do;"
      , "bool else;", "bool end;", "bool false;", "bool fi;", "bool float;"
      , "bool if;", "bool int;", "bool od;", "bool proc;", "bool read;"
      , "bool ref;", "bool then;", "bool true;", "bool val;", "bool while;"
      , "bool write;", "float begin;", "float bool;", "float call;", "float do;"
      , "float else;", "float end;", "float false;", "float fi;", "float float;"
      , "float if;", "float int;", "float od;", "float proc;", "float read;"
      , "float ref;", "float then;", "float true;", "float val;", "float while;"
      , "float write;", "int begin;", "int bool;", "int call;", "int do;"
      , "int else;", "int end;", "int false;", "int fi;", "int float;"
      , "int if;", "int int;", "int od;", "int proc;", "int read;", "int ref;"
      , "int then;", "int true;", "int val;", "int while;", "int write;"
      ]
    ]


writeDimTest :: WriterUnitTest Dim
writeDimTest
  = WriterUnitTest writeDim
    [ WriterTestCase "" Dim0
    , WriterTestCase "[0]" (Dim1 0)
    , WriterTestCase "[1]" (Dim1 1)
    , WriterTestCase "[1, 0]" (Dim2 1 0)
    , WriterTestCase "[10, 20]" (Dim2 10 20)
    , WriterTestCase "[1, 0]" (Dim2 1 0)
    , WriterTestCase "[10, 20]" (Dim2 10 20)
    ]

writeParamTest :: WriterUnitTest Param
writeParamTest
  = WriterUnitTest writeParam
    [ WriterTestCase "val bool a" (Param Val BoolType (Id "a"))
    , WriterTestCase "ref bool alt" (Param Ref BoolType (Id "alt"))
    , WriterTestCase "val int bc'" (Param Val IntType (Id "bc'"))
    , WriterTestCase "ref float ___aleph" (Param Ref FloatType (Id "___aleph"))
    ]

writeDeclTest :: WriterUnitTest Decl
writeDeclTest
  = WriterUnitTest writeDecl -- no indentation
    [ WriterTestCase "bool i[1, 2];\n"  (Decl BoolType (Id "i") (Dim2 1 2))
    , WriterTestCase "int action[1];\n" (Decl IntType (Id "action") (Dim1 1))
    , WriterTestCase "float boolean;\n" (Decl FloatType (Id "boolean") (Dim0))
    ]

writeScalarTest :: WriterUnitTest Scalar
writeScalarTest
  = WriterUnitTest writeScalar
    [ WriterTestCase "x" (Single (Id "x"))
    , WriterTestCase "x[1]" (Array (Id "x") (IntConst 1))
    , WriterTestCase "x[2, 3.0]" (Matrix (Id "x") (IntConst 2) (FloatConst 3.0))
    ]

writeStmtTest :: WriterUnitTest Stmt
writeStmtTest
  = WriterUnitTest writeStmt -- no indentation
    [ WriterTestCase "call f();\n" (Call (Id "f") [])
    , WriterTestCase "call f(1);\n" (Call (Id "f") [IntConst 1])
    , WriterTestCase "call f(1, 2);\n" (Call (Id "f") [IntConst 1, IntConst 2])
    , WriterTestCase "x := 42;\n" (Asg (Single (Id "x")) (IntConst 42))
    , WriterTestCase "x[1] := 42;\n" (Asg (Array (Id "x") (IntConst 1)) (IntConst 42))
    , WriterTestCase "x[1, 2] := 42;\n" (Asg (Matrix (Id "x") (IntConst 1) (IntConst 2)) (IntConst 42))
    , WriterTestCase "read x;\n" (Read (Single (Id "x")))
    , WriterTestCase "read x[1];\n" (Read (Array (Id "x") (IntConst 1)))
    , WriterTestCase "read x[1, 2];\n" (Read (Matrix (Id "x") (IntConst 1) (IntConst 2)))
    , WriterTestCase "write x;\n" (WriteExpr $ ScalarExpr (Single (Id "x")))
    , WriterTestCase "write x[1];\n" (WriteExpr $ ScalarExpr (Array (Id "x") (IntConst 1)))
    , WriterTestCase "write x[1, 2];\n" (WriteExpr $ ScalarExpr (Matrix (Id "x") (IntConst 1) (IntConst 2)))
    -- TODO: Test compound statement writing
    ]


writeExprTest :: WriterUnitTest Expr
writeExprTest
  = WriterUnitTest writeExpr
    -- literals should use correct lit writers
    [ WriterTestCase "0" (IntConst 0)
    , WriterTestCase "1" (IntConst 1)
    , WriterTestCase "0.0" (FloatConst 0.0)
    , WriterTestCase "0.0000042" (FloatConst 4.2E-6) -- no exponential
    , WriterTestCase "true" (BoolConst True)
    , WriterTestCase "false" (BoolConst False)
    -- and scalars too
    , WriterTestCase "x" (ScalarExpr (Single (Id "x")))
    , WriterTestCase "x[1]" (ScalarExpr (Array (Id "x") one))
    , WriterTestCase "x[1, 1]" (ScalarExpr (Matrix (Id "x") one one))
    -- binexprs should print without parens if their arguments are not binexprs
    , WriterTestCase "1 + 1" (BinExpr Add one one)
    , WriterTestCase "1 - -1" (BinExpr Sub one (UnExpr Neg one))
    , WriterTestCase "1 * 1" (BinExpr Mul one one)
    , WriterTestCase "1 / 1" (BinExpr Div one one)
    , WriterTestCase "1 < 1" (BinExpr LTh one one)
    , WriterTestCase "1 <= 1" (BinExpr LEq one one)
    , WriterTestCase "1 = 1" (BinExpr Equ one one)
    , WriterTestCase "1 != 1" (BinExpr NEq one one)
    , WriterTestCase "1 > 1" (BinExpr GTh one one)
    , WriterTestCase "1 >= 1" (BinExpr GEq one one)
    , WriterTestCase "1 && 1" (BinExpr And one one)
    , WriterTestCase "1 || !1" (BinExpr Or one (UnExpr Not one))
    , WriterTestCase "-1 || 1" (BinExpr Or (UnExpr Neg one) one)
    -- binexprs of binexprs SHOULD print parens
    , WriterTestCase "1 + (1 * 1)" (BinExpr Add one (BinExpr Mul one one))
    , WriterTestCase "(1 + 1) * 1" (BinExpr Mul (BinExpr Add one one) one)
    -- but not nested unexprs/consts
    , WriterTestCase "(-x + !1) * 1" (BinExpr Mul (BinExpr Add (UnExpr Neg var) (UnExpr Not one)) one)
    ]
    where
      one = IntConst 1
      var = ScalarExpr (Single (Id "x"))

writeIntLitTest :: WriterUnitTest Int
writeIntLitTest
  = WriterUnitTest writeIntLit
    [ WriterTestCase "0" 0
    , WriterTestCase "1" 1
    , WriterTestCase "42" 42
    ]

writeFloatLitTest :: WriterUnitTest Float
writeFloatLitTest
  = WriterUnitTest writeFloatLit
    [ WriterTestCase "0.0" 0.0
    , WriterTestCase "4.2" 4.2
    , WriterTestCase "0.0000042" 4.2E-6
    , WriterTestCase "420000.0" 4.2E5
    ]

writeBoolLitTest :: WriterUnitTest Bool
writeBoolLitTest
  = WriterUnitTest writeBoolLit
    [ WriterTestCase "true" True
    , WriterTestCase "false" False
    ]

writeStringLitTest :: WriterUnitTest String
writeStringLitTest
  = WriterUnitTest writeStringLit
    -- \n should go to \ and n
    [ WriterTestCase "\"hello, world!\\n\"" "hello, world!\n"
    -- \ should go to \
    , WriterTestCase "\"hello, \\world!\"" "hello, \\world!"
    -- tabs and other esc. sequences are not allowed by parser!
    ]

main
  = runTestTT $ TestList
    [ generateParserUnitTest integerTest
    , generateParserUnitTest integerOrFloatTest
    , generateParserUnitTest stringLiteralTest
    , generateParserUnitTest pParamTest
    , generateParserUnitTest pDimTest
    , generateParserUnitTest pAsgTest
    , generateParserUnitTest pExprTest
    , generateParserUnitTest pDeclTest

    , generateWriterUnitTest writeDimTest
    , generateWriterUnitTest writeParamTest
    , generateWriterUnitTest writeDeclTest
    , generateWriterUnitTest writeScalarTest
    , generateWriterUnitTest writeStmtTest
    , generateWriterUnitTest writeExprTest
    , generateWriterUnitTest writeIntLitTest
    , generateWriterUnitTest writeBoolLitTest
    , generateWriterUnitTest writeFloatLitTest
    , generateWriterUnitTest writeStringLitTest
    ]
