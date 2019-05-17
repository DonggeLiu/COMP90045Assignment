module GoatLang.CodeWriter where

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
  | LightColours
  | DarkColours

data ColourScheme
  = ColourScheme { keywordColour :: Colour
                 , stringColour  :: Colour
                 , numberColour :: Colour
                 , identColour   :: Colour
                 }

getColourScheme :: ColourSchemeName -> ColourScheme
getColourScheme NoColours
  = ColourScheme id id id id
getColourScheme LightColours
  = ColourScheme { keywordColour = dMgn
                 , stringColour  = dGrn
                 , numberColour  = dBlu
                 , identColour   = dCyn
                 }
getColourScheme DarkColours
  = ColourScheme { keywordColour = bMgn
                 , stringColour  = bGrn
                 , numberColour  = bYel
                 , identColour   = bCyn
                 }



-- ----------------------------------------------------------------------------
-- We will adapt the State monad for our purposes as an efficient and Monadic
-- String builder. You can think of this as a kind of 'opposite' of Parsec!
--
-- This approach is inspired by Chapter 13/14 of "Learn You a Haskell", "For A
-- Few Monads More" (http://www.learnyouahaskell.com/for-a-few-monads-more)
-- (but uses an original implementation of difference lists, and adds syntax 
-- highlighting).
-- ----------------------------------------------------------------------------

-- A CodeWriter is a State Monad that stores an accumulated program output
-- (as a difference list/string) and carries along a colour scheme.
type CodeWriter a
  = State CodeWriterState a

-- -- To actually build a string, we just need to run the Writer, discard
-- -- the output (which will be ()), and transform the log from a difference
-- -- list back into a normal string:
data CodeWriterState
  = CodeWriterState { output :: DiffList Char
                    , scheme :: ColourScheme
                    }


-- run the monad with colours
writeCodeColoured :: ColourScheme -> CodeWriter () -> String
writeCodeColoured colours codeWriter
  = outputString
  where
    start = CodeWriterState (mempty :: DiffList Char) colours
    (_, CodeWriterState _ outputDiffString) = runState codeWriter start
    outputString = listify outputDiffString

-- run the monad with no colours
writeCode :: CodeWriter () -> String
writeCode = writeCodeColoured (getColourScheme NoColours)



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
      put $ state {output = code <> dlistify string}

-- writeLn
-- Create an action to add a string to a Code Writer and
-- follow it immediately with a newline.
writeLn :: String -> CodeWriter ()
writeLn s = write s >> write "\n"

-- showWrite
-- Create an action to add any Showable thing as a string to a Code Writer
showWrite :: Show a => a -> CodeWriter ()
showWrite showable
  = write $ show showable




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
-- Syntax-highlighting CodeWriters
-- ----------------------------------------------------------------------------

writeKeyword :: String -> CodeWriter ()
writeKeyword word
  = do
      state <- get
      let highlighter = keywordColour $ scheme $ state
      write $ highlighter word

writeStrLit :: String -> CodeWriter ()
writeStrLit unparsedString
  = do
      state <- get
      let highlighter = stringLiteralColour $ scheme $ state
      write $ highlighter unparsedString




-- writeCharEsc
-- Write a single character, taking care to 'unparse' escaped characters back
-- into escape sequences (namely `\n` --> `\` followed by `n`).
writeCharEsc :: Char -> StringBuilder
writeCharEsc '\n'
    -- a `\` (slash) followed by `n`.
    = write "\\" >> write "n"
writeCharEsc c
    -- just the character itself (as a string)
    = write (c:"")


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
