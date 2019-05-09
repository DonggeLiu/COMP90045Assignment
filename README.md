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
├── spec/...                        # project specifications
├── oz/...                          # provided C code for an Oz emulator
└── src/                            # our project code!
    ├── Makefile                    # run `make` to build Goat compiler
    ├── _SubmissionMakefile         # submission requires a makefile too
    ├── Goat.hs                     # main module; compiler entry-point
    ├── GoatLang/                   # Goat syntax: parser, pretty-printer, ast
    │   ├── grammar.txt               # describes Goat program syntax
    │   ├── AST.hs                    # language structure definitions
    │   ├── Token.hs                  # lexeme parsers for
    │   ├── Parser.hs                 # program, statement, expr parsers
    │   ├── PrettyPrint.hs            # converting programs to strings
    │   └── ...                       # (we may need to create more)
    ├── OzLang/...                 # Code for targeting Oz, in another module?
    ├── Util/                      # non-Goat-specific utilities
    │   ├── Combinators.hs           # additional parser combinators
    │   ├── DiffList.hs              # difference list implementation
    │   ├── StringBuilder.hs         # efficient monadic string building
    |   └── ...                      # (we may need to create more)
    └── tests/                     # unit and integration tests
        ├── samples/...              # various sample Goat programs
        ├── testall.sh               # script to test with all samples/*.gt
        └── testgen.py               # input/output lists for unit tests
```

### Building

We cut out cabal; just build using `make` inside `src/` or use `ghc`/`ghci` directly.

### Testing

For now:

* `cd` to `src/`.
* Add unit tests to `tests/testgen.py`.
* Add integration tests:
  - For pretty-printing tests, add `TEST.gt` and `TEST.gt.pp` somewhere inside `tests/samples`
  - For syntax error tests, add `TEST.gt.bad` (and, optionally, `TEST.gt.bad.out`) somewhere inside `tests/samples`.
* Run `make tests` (or just `make utests` or `make itests`).
* Probably run `make clean` afterwards :)

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

#### Preparation week: Thursday 2nd-Monday 6th

TODO: Meet and decide on how to approach the project

#### Week 1: Tuesday 7th-Monday 13th
#### Week 2: Tuesday 14th-Monday 20th
#### Week 3: Tuesday 21st-Monday 27th
#### Submission: Tuesday 28th, Wednesday 29th.
