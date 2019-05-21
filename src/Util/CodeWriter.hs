module Util.CodeWriter
(
-- choosing a colour scheme
  ColourSchemeName(..)
, ColourScheme
, getColourSchemeByName
-- CodeWrite monad
, CodeWriter
-- run the monad
, writeCodeColoured
, writeCode
-- writing to the monad
, write
, showWrite
-- manipulating indentation
, indentation
, line
, semiLine
, withIncreasedIndentation
, increaseIndentation
, decreaseIndentation
, decreaseIndentation_
-- syntax-highlighting writers and combinators
, asKeyword
, asString
, asNumber
, asIdent
, asComment
, writeKeyword
, writeIdent
-- generally useful code writers
, space
, newline
, semi
-- and generally useful codewriter combinators
, parens
, brackets
, quote
, spaces
, sepBy
, commaSep
) where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--        UTILITY - MONADIC SYNTAX-HIGHLIGHTING STRING BUILDERS
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

import Control.Monad.State (State, get, put, runState)

import Util.DiffList
import Util.ColourParTTY


data ColourSchemeName
  = NoColours
  | LightTerminal
  | DarkTerminal

data ColourScheme
  = ColourScheme { keywordColour :: Colour
                 , stringColour  :: Colour
                 , numberColour  :: Colour
                 , identColour   :: Colour
                 , commentColour :: Colour
                 , resetColour   :: Colour
                 }

defaultColourScheme :: ColourScheme
defaultColourScheme
  = ColourScheme "" "" "" "" "" reset

getColourSchemeByName :: ColourSchemeName -> ColourScheme
getColourSchemeByName NoColours
  = defaultColourScheme { resetColour   = "" }
getColourSchemeByName LightTerminal
  = defaultColourScheme { keywordColour = setMgn1
                        , stringColour  = setGrn1
                        , numberColour  = setBlu1
                        , identColour   = setCyn1
                        , commentColour = setWht1
                        }
getColourSchemeByName DarkTerminal
  = defaultColourScheme { keywordColour = setMgn2
                        , stringColour  = setGrn2
                        , numberColour  = setYel2
                        , identColour   = setCyn2
                        , commentColour = setBlk2
                        }



-- ----------------------------------------------------------------------------
-- We will adapt the State monad for our purposes as an efficient and Monadic
-- String builder. You can think of this as a kind of 'opposite' of Parsec!
--
-- This approach is inspired by Chapter 13/14 of "Learn You a Haskell", "For A
-- Few Monads More" (http://www.learnyouahaskell.com/for-a-few-monads-more)
-- (but uses an original implementation of difference lists, and adds syntax
-- highlighting, switching to using the State monad).
-- ----------------------------------------------------------------------------

-- A CodeWriter is a State Monad that stores an accumulated program output
-- (as a difference list/string) and carries along a colour scheme.
type CodeWriter a
  = State CodeWriterState a

-- -- To actually build a string, we just need to run the Writer, discard
-- -- the output (which will be ()), and transform the log from a difference
-- -- list back into a normal string:
data CodeWriterState
  = CodeWriterState { scheme :: ColourScheme    -- the provided colour scheme
                    , output :: DiffList Char   -- the accumulated program code
                    , istack :: [CodeWriter ()] -- indentation stack
                    }


-- run the monad with colours
writeCodeColoured :: ColourScheme -> CodeWriter () -> String
writeCodeColoured colours codeWriter
  = outputString
  where
    start = CodeWriterState { scheme = colours   -- save provided colour scheme
                            , output = mempty    -- no code yet
                            , istack = []        -- no indentation yet
                            }
    (_, end) = runState codeWriter start
    outputString = listify $ output end

-- run the monad with no colours
writeCode :: CodeWriter () -> String
writeCode = writeCodeColoured (getColourSchemeByName NoColours)


-- ----------------------------------------------------------------------------
-- Writing to the CodeWriter
-- ----------------------------------------------------------------------------

-- The following functions will be useful for constructing
-- Code Writers, one string at a time:

-- write
-- Create an action to add a string to a Code Writer
-- (We just need to convert the string to a difference
-- list and then append it to the Code Writer's state)
write :: String -> CodeWriter ()
write string
  = do
      state <- get
      let code = output state
      put $ state {output = code `mappend` dlistify string}

-- showWrite
-- Create an action to add any Showable thing as a string
-- to a Code Writer
showWrite :: Show a => a -> CodeWriter ()
showWrite showable
  = write $ show showable


-- ----------------------------------------------------------------------------
-- Manipulating the level of indentation
-- ----------------------------------------------------------------------------

-- indentation
-- Write out all of the current indenters (do this at the start of each line,
-- or use `line`).
indentation :: CodeWriter ()
indentation
  = do
      state <- get
      let indentationStack = istack state
      sequence_ indentationStack

-- line
-- Surround a writer's output with the current level of indentation and a
-- newline
line :: CodeWriter () -> CodeWriter ()
line lineContentWriter
  = indentation >> lineContentWriter >> newline

-- semiLine
-- Helper action to add a semicolon before terminating a CodeWriter line
semiLine :: CodeWriter () -> CodeWriter ()
semiLine lineContentWriter
  = line $ lineContentWriter >> semi


-- withIncreasedIndentation
-- Perform a writer action with an increased level of indentation, and restore
-- the indentation level afterwards.
withIncreasedIndentation :: CodeWriter () -> CodeWriter () -> CodeWriter ()
withIncreasedIndentation indenter writer
  = do
      increaseIndentation indenter
      writer
      decreaseIndentation_

-- increaseIndentation
-- Add an indentation writer to the indentation stack
increaseIndentation :: CodeWriter () -> CodeWriter ()
increaseIndentation indenter
  = do
      state <- get
      let indentationStack = istack state
      put $ state { istack = indenter : indentationStack }

-- decreaseIndentation
-- Remove (and return?) the previously-added indentation writer from the
-- indentation stack (or `return ()` if no more levels of indentation).
decreaseIndentation :: CodeWriter (CodeWriter ())
decreaseIndentation
  = do
      state <- get
      let indentationStack = istack state
      case indentationStack of
          []            -> return (return ())
          (i:indenters) -> (put $ state { istack = indenters }) >> return i

-- decreaseIndentation_
-- Same as decreaseIndentation, but without the return value
decreaseIndentation_ :: CodeWriter ()
decreaseIndentation_
  = decreaseIndentation >> return ()


-- ----------------------------------------------------------------------------
-- Syntax-highlighting CodeWriters
-- ----------------------------------------------------------------------------


-- highlight
-- Given a colour scheme function (e.g. `commentColour` or `keywordColour`,
-- accessors from the ColourScheme record data type) create an action to
-- perform another action using that highlighter.
highlight :: (ColourScheme -> Colour) -> CodeWriter () -> CodeWriter ()
highlight colour writer
  = do
      state <- get
      let colourScheme = scheme state
      write $ colour colourScheme
      writer
      write $ resetColour colourScheme

-- asKeyword
-- keywordColour highlighter combinator
asKeyword :: CodeWriter () -> CodeWriter ()
asKeyword
  = highlight keywordColour

-- asString
-- stringColour highlighter combinator
asString :: CodeWriter () -> CodeWriter ()
asString
  = highlight stringColour

-- asNumber
-- numberColour highlighter combinator
asNumber :: CodeWriter () -> CodeWriter ()
asNumber
  = highlight numberColour

-- asIdent
-- identColour highlighter combinator
asIdent :: CodeWriter () -> CodeWriter ()
asIdent
  = highlight identColour

-- asComment
-- commentColour highlighter combinator
asComment :: CodeWriter () -> CodeWriter ()
asComment
  = highlight commentColour


-- writeKeyword
-- reflecting common useage of asKeyword, this is a shortcut to
-- `write` a keywordColour-highlighted string directly.
writeKeyword :: String -> CodeWriter ()
writeKeyword
  = asKeyword . write

-- writeKeyword
-- reflecting common useage of asKeyword, this is a shortcut to
-- `write` a keywordColour-highlighted string directly.
writeIdent :: String -> CodeWriter ()
writeIdent
  = asIdent . write


-- ----------------------------------------------------------------------------
-- Generally helpful CodeWriters
-- ----------------------------------------------------------------------------

-- space
-- Action to add a single space character
space :: CodeWriter ()
space
  = write " "

-- newline
-- To add a newline character
newline :: CodeWriter ()
newline
  = write "\n"

-- semi
-- To add a semicolon character
semi :: CodeWriter ()
semi
  = write ";"


-- ----------------------------------------------------------------------------
-- Generally helpful CodeWriter Combinators
-- ----------------------------------------------------------------------------

-- parens
-- To enclose the output of another action in parentheses
parens :: CodeWriter () -> CodeWriter ()
parens writer
  = write "(" >> writer >> write ")"

-- brackets
-- To enclose the output of another action in square brackets
brackets :: CodeWriter () -> CodeWriter ()
brackets writer
  = write "[" >> writer >> write "]"

-- quote
-- To surround the output of another action in double quotes
quote :: CodeWriter () -> CodeWriter ()
quote writer
  = write "\"" >> writer >> write "\""

-- spaces
-- To surround the output of another action in single spaces
spaces :: CodeWriter () -> CodeWriter ()
spaces writer
  = space >> writer >> space


-- sepBy
-- To intersperse a list of writers with a separating writer
sepBy :: CodeWriter () -> [CodeWriter ()] -> CodeWriter ()
sepBy _ []
  = return ()
sepBy separator (writer:writers)
  = do
      writer
      mapM_ (separator >>) writers

-- commaSep
-- To intersperse commas/spaces between a list of writers
-- (reflecting common usage of sepBy)
commaSep :: [CodeWriter ()] -> CodeWriter ()
commaSep
  = sepBy (write ", ")
