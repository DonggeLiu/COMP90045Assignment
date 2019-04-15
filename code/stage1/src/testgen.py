PARSE_TESTS = \
[ ("integer",
    [ ("42", ["42", "042", "0042", "00042"])
    , ("0", ["0", "00", "000"])
    , ("1234567890", ["01234567890"])
    , ("ERROR", ["", "4 2", "4,2", "0x42", "0xff", "4.2", "4."])
    ])
, ("integerOrFloat",
    [ ("Left 1234567890", ["01234567890"])
    , ("Left 42", ["42", "042", "0042", "00042"])
    , ("Left 0", ["0", "00", "000"])
    , ("Right 12345.06789", ["12345.06789", "0012345.06789000"])
    , ("Right 0.0", ["0.0", "00.00", "000.000"])
    , ("ERROR", [ "", "4 2", "4,2", "0x42", "0xff", ".0", "4.", "4.2.", "4.2.3"
                , "0.", "0. 0", "1e3", "1E3", "1E-3", "1E+4", "0x42"
                ])
    ])
, ("stringLiteral",
    [ (r'"hello"', [r'\"hello\"'])
    , (r'"hello\n"', [r'\"hello\\n\"'])
    , (r'"hello\\t"', [r'\"hello\\t\"'])
    , ("ERROR", [r'\"hello\n\"', r'\"hello\t\"', r'\"hello', r'hello\"', r'\"hello\"\"'])
    ])
, ("pParam",
    [ ('Param Val BoolType "a"', ["val bool a", "val  bool  a"
                                 , "val\\tbool\\ta"
                                 , "val\\nbool\\na"
                                 ])
    , ("ERROR", ["val val var", "val ref var", "ref val var", "ref ref var"
                , "bool val var", "bool ref var", "float val var"
                , "float ref var", "int val var", "int ref var  "
                , "VAL bool var", "REF bool var", "Val bool var"
                , "Ref bool var", "vAL bool var", "rEF bool var"
                , "bool bool var", "val' bool var", "ref' bool var"
                , "val _ var", "val bool _", "val _ _", "val bool true"
                , r'val bool \"hello\"', "val bool 42", "val bool 3.14"
                ])
    ])
, ("pDecl",
    [ ("ERROR", ["bool;", "bool _;", "bool ';", "bool (;", "bool );"
    , "bool [;", "bool ];", "bool +;", "bool -;", "bool *;", "bool /;"
    , "bool !;", "bool ||;", "bool &&;", "bool =;", "bool !=;", "bool <;"
    , "bool <=;", "bool >;", "bool >=;", "bool \\\"var\\\";", "bool _var;"
    , "bool 'var;", "bool 0var;", "bool var", "float;", "float _;", "float ';"
    , "float (;", "float );", "float [;", "float ];", "float +;", "float -;"
    , "float *;", "float /;", "float !;", "float ||;", "float &&;", "float =;"
    , "float !=;", "float <;", "float <=;", "float >;", "float >=;"
    , "float \\\"var\\\";", "float _var;", "float 'var;", "float 0var;"
    , "float var", "int;", "int _;", "int ';", "int (;", "int );"
    , "int [;", "int ];", "int +;", "int -;", "int *;", "int /;", "int !;"
    , "int ||;", "int &&;", "int =;", "int !=;", "int <;", "int <=;", "int >;"
    , "int >=;", "int \\\"var\\\";", "int _var;", "int 'var;", "int 0var;"
    , "int var", "bool begin;", "bool bool;", "bool call;", "bool do;"
    , "bool else;", "bool end;", "bool false;", "bool fi;", "bool float;"
    , "bool if;", "bool int;", "bool od;", "bool proc;", "bool read;"
    , "bool ref;", "bool then;", "bool true;", "bool val;", "bool while;"
    , "bool write;", "float begin;", "float bool;", "float call;", "float do;"
    , "float else;", "float end;", "float false;", "float fi;", "float float;"
    , "float if;", "float int;", "float od;", "float proc;", "float read;"
    , "float ref;", "float then;", "float true;", "float val;", "float while;"
    , "float write;", "int begin;", "int bool;", "int call;", "int do;"
    , "int else;", "int end;", "int false;", "int fi;", "int float;", "int if;"
    , "int int;", "int od;", "int proc;", "int read;", "int ref;", "int then;"
    , "int true;", "int val;", "int while;", "int write;" ])
    ])
, ("pDim",
    [("ERROR", ["[]", "[][]", "[,]", "[ , ]", "[42][]", "[42,]", "[42, ]", "[][42]"
    , "[,42]", "[ ,42]", "[[]]", "[[]][[]]", "[[],[]]", "[][[]]", "[,[]]"
    , "[ ,[]]", "[[]][]", "[[],]", "[[], ]", "[[42]]", "[[42]][[42]]"
    , "[[42],[42]]", "[42][[42]]", "[42,[42]]", "[[42]][42]", "[[42],42]"
    , "[\\\"hello\\\"]", "[\\\"hello\\\"][\\\"harald\\\"]"
    , "[\\\"hello\\\",\\\"harald\\\"]", "[yarra][trams]", "[yarra,trams]"
    , "[\\\"COMP\\\"][90045]", "[\\\"COMP\\\",90045]", "[90045][\\\"COMP\\\"]"
    , "[90045,\\\"COMP\\\"]", "[1][2][3]", "[1,2,3]", "[3.14]", "[3.14][42]"
    , "[3.14,42]", "[42][3.14]", "[42,3.14]", "[3.14][2.72]", "[3.14,[2.72]"
    , "[42.]", "[.42]", "[42hello]", "[42_]", "[42+]"])
    ])
, ("pAsg",
    [("ERROR", ["var = 1;", "var :=;", ":= 42;", "var := read;"
    , "foo < bar := true;", "var +:= 1;", "\\\"var\\\" := true;", "true := var;"
    , "[] := 42;", "[var] := 42;", "var[] := 42;", "var[][] := 42;"
    , "var[[0]] := 42;", "var := call factorial(n);", "var := factorial(n);"
    , "var := main();", "var := +;", "-var := 42;", "!var := false;"])
    ])
, ("pExpr",
    [("ERROR", ["+", "-", "*", "/", "=", ">", ">=", "<", "<=", "&&", "||", "!"
    , "1 2", "3.14 2.72", "1 +", "1 -", "1 *", "1 /", "1 =", "1 >", "1 >="
    , "1 <", "1 <=", "1 &&", "1 ||", "1 !", "true 2", "true false", "true +"
    , "true -", "true *", "true /", "true =", "true >", "true >=", "true <"
    , "true <=", "true &&", "true ||", "true !", "1 = 2 = 3", "1 < 2 < 3"
    , "1 < 2 = 3", "1 = 2 < 3", "1 <= 2 <= 3", "1 >= 2 >= 3", "1 + + 3"
    , "true && && false", "true || || false", "true || && false", "1 true"
    , "true 1"])
    ])
]

"""
writeStmtWith indentation (Write expr)
  = indentation >> write "write" >> space >> writeExpr expr >> endLine
"""
PRINT_TESTS = \
[ ("writeF",
    [ ("", "Dim0"), ("[0]", "Dim1 0"), ("[1]", "Dim1 1")
    , ("[1, 0]", "Dim2 1 0"), ("[10, 20]", "Dim2 10 20")
    , ("[1, 0]", "Dim2 1 0"), ("[10, 20]", "Dim2 10 20")
    , ("val bool a", "Param Val BoolType \"a\"")
    , ("ref bool alt", "Param Ref BoolType \"alt\"")
    , ("val int bc'", "Param Val IntType \"bc'\"")
    , ("ref float ___aleph", "Param Ref FloatType \"___aleph\"")
    ])
, ("(writeDeclWith $ return ())",  # no indentation
    [ ("bool i[1, 2];\\n", "Decl BoolType \"i\" (Dim2 1 2)")
    , ("int action[1];\\n", "Decl IntType \"action\" (Dim1 1)")
    , ("float boolean;\\n", "Decl FloatType \"boolean\" (Dim0)")
    ])
, ("writeVar",
    [ ("x", "Var0 \"x\""), ("x[1]", "Var1 \"x\" (IntConst 1)")
    , ("x[2, 3.0]", "Var2 \"x\" (IntConst 2) (FloatConst 3.0)")
    ])
, ("(writeStmtWith $ return ())",  # no indentation
    [ ("call f();\\n", "Call \"f\" []")
    , ("call f(1);\\n", "Call \"f\" [IntConst 1]")
    , ("call f(1, 2);\\n", "Call \"f\" [IntConst 1, IntConst 2]")
    , ("x := 42;\\n", "Asg (Var0 \"x\") (IntConst 42)")
    , ("x[1] := 42;\\n", "Asg (Var1 \"x\" (IntConst 1)) (IntConst 42)")
    , ("x[1, 2] := 42;\\n", "Asg (Var2 \"x\" (IntConst 1) (IntConst 2)) (IntConst 42)")
    , ("read x;\\n", "Read (Var0 \"x\")")
    , ("read x[1];\\n", "Read (Var1 \"x\" (IntConst 1))")
    , ("read x[1, 2];\\n", "Read (Var2 \"x\" (IntConst 1) (IntConst 2))")
    , ("write x;\\n", "Write $ VarExpr (Var0 \"x\")")
    , ("write x[1];\\n", "Write $ VarExpr (Var1 \"x\" (IntConst 1))")
    , ("write x[1, 2];\\n", "Write $ VarExpr (Var2 \"x\" (IntConst 1) (IntConst 2))")
    , # TODO: Test compound statement writing
    ])
, ("writeExpr",
    [ (r'\"hello, world!\\n\"', "StrConst \"hello, world!\\n\"")
    # TODO: Test expression writing
    ])
]


HEAD = """
import Data.Either (isLeft)
import Test.HUnit

import Text.Parsec (parse, eof, ParseError)
import Util.StringBuilder

import GoatLang.AST
import GoatLang.Token
import GoatLang.Parser
import GoatLang.PrettyPrint

parseString :: (Eq a) => Parser a -> String -> Either ParseError a
parseString parser string
  = parse (do {result <- parser; eof; return result}) "" string
parseError :: (Eq a) => Parser a -> String -> Bool
parseError parser string
  = isLeft $ parseString parser string
stringBuild :: (a -> StringBuilder) -> a -> String
stringBuild writer ast
  = buildString $ writer ast

tests
  = test [ "hello" ~: "hello" ~: "hello" ~=? "hello" -- dummy test"""
TAIL = """         ]

main = runTestTT tests
"""

def test(name, call, result):
    return f'         , "{name}" ~: "" ~: {result} ~=? ({call})'

def main():
    print(HEAD)
    num = 1
    for parser, tests in PARSE_TESTS:
        for expected, inputs in tests:
            if expected == "ERROR":
                for string in inputs:
                    call = f'parseError {parser} "{string}"'
                    print(test(f"test{num}", call, "True"))
                    num += 1
            else:
                for string in inputs:
                    call = f'parseString {parser} "{string}"'
                    print(test(f"test{num}", call, f'(Right ({expected}))'))
                    num += 1
    for writer, tests in PRINT_TESTS:
        for expected, ast in tests:
            call = f'stringBuild {writer} ({ast})'
            print(test(f"test{num}", call, f'"{expected}"'))
            num += 1
    print(TAIL)

if __name__ == '__main__':
    main()
