# COMP90045Assignment

## Members

| Name                      | Username   | Email                            |
| ------------------------- | ---------- | -------------------------------- |
| Matthew Farrugia-Roberts  | farrugiam  | farrugiam@student.unimelb.edu.au |
| Dongge Liu                | donggel    | donggel@student.unimelb.edu.au   |
| Mariam Shahid             | mariams    | mariams@student.unimelb.edu.au   |
| David Stern               | dibstern   | dstern@student.unimelb.edu.au    |
| Alan Ung                  | alanu      | alanu@student.unimelb.edu.au     |

## Project structure

Here's a map (:: (a -> b) -> [a] -> [b]) of the repository:

```
COMP90045Assignment/
├── README.md                       # <-- YOU ARE HERE
├── Members.txt                     # list of well-chosen team members
├── code/                           # code, organised by project stage:
│   ├── kid/...                       # provided starter code
│   └── stage1/                       # STAGE 1 CODE
│       ├── Goat.cabal, Setup.hs        # This is a cabal project (see below)
│       └── src/                        # code goes in here:
│           ├── Goat.hs                   # main module; compiler entry-point
|           ├── GoatLang/                 # parser, pretty-printer, ast
|           |   ├── grammar.txt             # describes Goat program syntax
|           |   ├── AST.hs                  # language structure definitions
|           |   ├── Token.hs                # lexeme parsers
|           |   ├── Parser.hs               # program, statement, expr parsers
|           |   ├── PrettyPrint.hs          # converting programs to strings
|           |   └── ...                     # (we may need to create more)
|           ├── Util/...                  # non-Goat-specific utilities
|           |   ├── Combinators.hs          # additional parser combinators
|           |   ├── StringBuilder.hs        # efficient monadic string building
|           |   ├── DiffList.hs             # difference list implementation
|           |   └── ...                     # (we may need to create more)
|           ├── gentests.py               # input/output lists for unit tests
│           └── Makefile                  # submission requires a Makefile
└── spec/...                        # project specifications
```

### Building

Development can be done with cabal. Inside `code/stage1/`:

* `cabal build` compiles source code and places executable in (ignored) directory `dist/`.
* `cabal run -- <arguments>` runs `Goat` executable with command-line arguments `<arguments>`.
* `cabal repl` fires up GHCi.
* `cabal help` for more.

Or, just work in `code/stage1/src/` and run `make`.

### Testing

For now:

* `cd` to `code/stage1/src/`.
* Add tests to `gentests.py`.
* Run `make tests`.


## Stage 1 - Goat parser and pretty-printer

### Timeline

#### Planning & Preparation Week: Tuesday 26th - Tuesday 2nd

- Understand Theory (LL, LR)
- Understand Parsec
- Break up work into smaller subtasks, planning the implementation week
- Review Kid Parser
- Create a Kid Pretty-Printer
- Plan Paired Programming
- Ask Harald Clarifying Questions

This was mostly accomplished, with the following plan for implementation week:

#### Implementation Week: Tuesday 2nd-Tuesday 9th

- [x] Task 0: Create Goat Grammar
- [x] Task 1: Abstract Syntax Tree
- [ ] Task 2a: Parsec parser (work underway on branch `parser`)
- [x] Task 2b: Pretty-Printer (see PR#4)
- [ ] Task 3: Testing parser and pretty-printer

We'll reflect at the Tuesday 9th Meeting: Decide on next steps, how to finish it off

#### Review Week: Tuesday 9th-Monday 15th

- [x] Complete parser
  - [x] Lexeme parsers (string, int, float)
  - [x] Language parsers (still TODO (if time): manyMN etc. to use count)
  - [x] Expression parser
- [x] Testing (we didn't get time to unit test ALL of the parsers and printers, but got the main ones and also ran lots of ad-hoc black box tests of the whole parser/pretty-printer combo comparing our output with other teams').
- [x] Error reporting
- [ ] Experiment with bonus-worthy ideas
- [x] Review Submission
- [x] Submit

## Stage 3 - Goat compiler

(Specification to be released around Easter/mid-semester break...)
