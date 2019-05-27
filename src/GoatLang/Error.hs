module GoatLang.Error where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 3
--
--                 GOAT - Representing compile errors
--
-- Well-chosen team name:              pli-dream-team-twentee-nineteen
-- Well-chosen team members:
-- * Alan Ung                          alanu
-- * David Stern                       dibstern
-- * Dongge Liu                        donggel
-- * Mariam Shahid                     mariams
-- * Matthew Farrugia-Roberts          farrugiam
--
-- ----------------------------------------------------------------------------

import Text.Parsec (SourcePos, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Error

import Util.ColourParTTY
import Util.CodeWriter

-- ----------------------------------------------------------------------------
-- Error data types
-- ----------------------------------------------------------------------------

-- We'll need one type to represent a position in the Goat source file, (or no 
-- position, such as for global errors or mocked-up AST nodes for testing).
data Pos
  = NoPos
  | Pos Int Int FilePath -- A position has a lineNum :: Int, a colNum :: Int,
                         -- and a sourceFileName :: FilePath (NOTE: FilePath
                         -- is just a Prelude type alias for String, and here
                         -- the string is ONLY used in displayed errors (no IO)
instance Eq Pos where
  -- All positions should compare equal, so that we can sensibly compare errors
  -- by their messages and AST nodes by their other components:
  (==) _ _
    = True
instance Show Pos where
  -- We want to be able to show positions in source files for our internal 
  -- representation of errors and AST nodes. NOTE: We'll use another, different 
  -- representation for displaying error messages to the user (see `writePos`).
  show NoPos
    = "NoPos"
  show (Pos line col src)
    = "Pos " ++ show line ++ ":" ++ show col

-- We'll need some new data types to represent various syntactic and semantic
-- errors, too! Each error has at least a messagem and may also have one or
-- more source positions used while displaying the message:
data SyntaxError
  = SyntaxError Pos String                 -- generic syntax error (from parsec)
  deriving (Show, Eq)

data SemanticError
  = SemanticError Pos String               -- generic semantic error
  | RepeatedDefinitionError Pos Pos String -- specifically for duplicate defns
  | GlobalError String                     -- positionless errors (e.g. no main)
  deriving (Show, Eq)


-- ----------------------------------------------------------------------------
-- Converting syntax errors and positions from Parsec types
-- ----------------------------------------------------------------------------

-- fromParsecError
-- Create a custom Syntax Error given a ParsecError.
fromParsecError :: ParseError -> SyntaxError
fromParsecError parsecError
  = SyntaxError goatPos goatMessage
  where
      -- convert the parsec SourcePos into our own position representation
      goatPos = fromParsecPosition (errorPos parsecError)
      
      -- use Parsec's showErrorMessages function to get a proper error message,
      -- it requires a bunch of template strings be provided, to form the full
      -- message, and starts with an unnecessary newline (hence `tail`):
      goatMessage = tail $ showErrorMessages
        "or" "unknown parse error" "expecting" "unexpected" "end of input"
        (errorMessages parsecError)

-- fromParsecPosition
-- Convert a parsec SourcePos into our own position representation
fromParsecPosition :: SourcePos -> Pos
fromParsecPosition pos
  = Pos (sourceLine pos) (sourceColumn pos) (sourceName pos)



-- ----------------------------------------------------------------------------
-- Displaying errors to the user
-- ----------------------------------------------------------------------------

-- prettifySyntaxError
-- 
prettifySyntaxError :: String -> SyntaxError -> String
prettifySyntaxError sourceCode (SyntaxError pos msg)
  = writeCode $ do
      -- start by giving the position of the error
      writePos pos >> newline
      -- then attempt to display the error within the source program, with
      -- 3 lines of pre-context and 3 columns leading up to the error position:
      -- (sometimes the parsec position is off by a column or two)
      writeContext pos 3 2 red1 sourceCode
      -- and, of course, display the error message describing the problem:
      line $ write msg

-- prettifySemanticError
-- 
prettifySemanticError :: String -> SemanticError -> String
prettifySemanticError sourceCode (SemanticError pos msg)
  = writeCode $ do
      -- start by giving the position of the error
      writePos pos >> newline
      -- then attempt to display the error within the source program, with
      -- 3 lines of pre-context and 1 column leading up to the error position:
      writeContext pos 3 1 red1 sourceCode
      -- and, of course, display the error message describing the problem:
      line $ write msg
prettifySemanticError _ (GlobalError msg)
  = writeCode $ do
      -- no position to show 
      write "global error" >> colon >> newline
      -- just display the message:
      line $ write msg
prettifySemanticError sourceCode (RepeatedDefinitionError pos oldPos msg)
  = writeCode $ do
      -- start by giving the position of the error
      writePos pos >> newline
      -- then attempt to display the error within the source program, with
      -- 3 lines of pre-context and 1 column leading up to the error position:
      writeContext pos 3 1 red1 sourceCode
      -- display the message describing te problem:
      line $ write msg
      -- and point to the original definition (less context necessary):
      line $ write "orginal definition " >> writePos oldPos >> colon
      writeContext oldPos 1 1 blu1 sourceCode



-- ----------------------------------------------------------------------------
-- Displaying snippets of source code in error messages
-- ----------------------------------------------------------------------------

-- writePos
-- Create a CodeWriter to pretty-print a source file position
-- Format example: `"tests/samples/example.gt" (line 5, column 15)`
writePos :: Pos -> CodeWriter ()
writePos NoPos
  = return ()
writePos (Pos lineNum colNum sourceFileName)
  = do
      write "at "
      showWrite sourceFileName
      space
      parens $ commaSep $ [ write "line" >> space >> showWrite lineNum
                          , write "column" >> space >> showWrite colNum
                          ]
      colon

-- writeContext
-- Parameters: pos numLines numCols colourer sourceCode
-- Create a CodeWriter to display a position within a source code file with a
-- given number of lines leading up to the position includes, and a given
-- number of columns leading up the the position highlghted (in a given colour)
writeContext :: Pos -> Int -> Int -> Colourer -> String -> CodeWriter ()
writeContext NoPos _ _ _ _
  = return () -- no position -> nothing to write
writeContext (Pos lineNum colNum _) numLines numCols colour sourceCode
  = withIncreasedIndentation (space >> space) $ do
      -- show some lines of source file context leading up to the problem:
      mapM_ (line . write) (lineContext numLines lineNum sourceCode)
      -- point to the problem within this last line:
      line $ write $ columnPoint numCols colNum colour

-- lineContext
-- Extract (up to) a number of lines before a particular line number from some
-- source code (a string of source file content), returning the resulting lines
-- as a list of strings (suitable for recombining with `unlines`).
lineContext :: Int -> Int -> String -> [String]
lineContext numLines lineNum sourceCode
  = take (min lineNum numLines) $ drop (lineNum-numLines) $ lines sourceCode
--  ^ take the first (up to)      ^ ... from the part of the file starting
--    n lines ...                       n lines before the given position

-- columnPoint
-- Create a string to be printed as a colourful pointer of a given width to a 
-- specified column position. E.g. `columnPoint 3 7 red1` would give the string 
-- `"     " ++ red1 "^^^"` (3 red arrows, with the final arrow in column 7).
columnPoint :: Int -> Int -> Colourer -> String
columnPoint numCols colNum colour
  = nChar padding ' ' ++ colour (nChar numCols '^')
  where
    padding = colNum - numCols
    
    -- Helper function to create a string with n repetitions of a given Char
    nChar :: Int -> Char -> String
    nChar n char
      = take n $ repeat char
