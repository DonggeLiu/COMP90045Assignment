TESTS = \
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
    [ ('Param Val Bool "a"', [])
    , ("ERROR", ["val val var", "val ref var", "ref val var", "ref ref var", "bool val var", "bool ref var", "float val var", "float ref var", "int val var", "int ref var  ", "VAL bool var", "REF bool var", "Val bool var", "Ref bool var", "vAL bool var", "rEF bool var", "bool bool var", "val' bool var", "ref' bool var", "val _ var", "val bool _", "val _ _", "val bool true", r'val bool \"hello\"', "val bool 42", "val bool 3.14"])
    ])
]

HEAD = """
import Data.Either (isLeft)
import Test.HUnit

import Text.Parsec (parse, eof, ParseError)

import GoatLang.Token
import GoatLang.Parser

parseString :: (Eq a) => Parser a -> String -> Either ParseError a
parseString parser string
  = parse (do {result <- parser; eof; return result}) "" string
parseError :: (Eq a) => Parser a -> String -> Bool
parseError parser string
  = isLeft $ parseString parser string

tests
  = test [ "hello" ~: "hello" ~: "hello" ~=? "hello" -- dummy test
"""
TAIL = """         ]

main = runTestTT tests
"""

def test(name, call, result):
    msg = 'msg'
    return f'         , "{name}" ~: "{msg}" ~: {result} ~=? ({call})'


def main():
    print(HEAD)
    num = 0
    for parser, tests in TESTS:
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
    print(TAIL)

if __name__ == '__main__':
    main()