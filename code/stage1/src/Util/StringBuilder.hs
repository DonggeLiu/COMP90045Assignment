module Util.StringBuilder where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                     UTILITY - MONADIC STRING BUILDER
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

import Control.Monad.Writer (Writer, tell, runWriter)

import Util.DiffList

-- ----------------------------------------------------------------------------
-- We will adapt the Writer monad for our purposes as an efficient and Monadic
-- String builder. You can think of this as a kind of 'opposite' of Parsec!
--
-- This approach is inspired by Chapter 13/14 of "Learn You a Haskell", "For A
-- Few Monads More" (http://www.learnyouahaskell.com/for-a-few-monads-more)
-- (but uses an original implementation of difference lists).
-- ----------------------------------------------------------------------------

-- A StringBuilder is a Writer Monad that doesn't compute a value;
-- it just builds a difference list (as the Writer's 'log').
type StringBuilder
  = Writer (DiffList Char) ()

-- To actually build a string, we just need to run the Writer, discard
-- the output (which will be ()), and transform the log from a difference
-- list back into a normal string:

-- buildString
-- Perform a String Builder action to produce a string.
buildString :: StringBuilder -> String
buildString
  = listify . snd . runWriter

-- The following functions will be useful for constructing
-- String Builders, one string at a time:

-- write
-- Create an action to add a string to a String Builder
-- (We just need to convert the string to a difference
-- list and then use Writer Monad's 'tell')
write :: String -> StringBuilder
write = tell . dlistify

-- writeLn
-- Create an action to add a string to a String Builder and
-- follow it immediately with a newline.
writeLn :: String -> StringBuilder
writeLn s = write s >> write "\n"


-- ----------------------------------------------------------------------------
-- Generally helpful StringBuilders
-- ----------------------------------------------------------------------------

-- space
-- Action to add a single space character
space :: StringBuilder
space
  = write " "

-- newline
-- To add a newline character
newline :: StringBuilder
newline
  = write "\n"

-- semi
-- To add a semicolon character
semi :: StringBuilder
semi
  = write ";"


-- ----------------------------------------------------------------------------
-- Generally helpful StringBuilder Combinators
-- ----------------------------------------------------------------------------

-- parens
-- To enclose the output of another action in parentheses
parens :: StringBuilder -> StringBuilder
parens writer
  = write "(" >> writer >> write ")"

-- brackets
-- To enclose the output of another action in square brackets
brackets :: StringBuilder -> StringBuilder
brackets writer
  = write "[" >> writer >> write "]"

-- quote
-- To surround the output of another action in double quotes
quote :: StringBuilder -> StringBuilder
quote writer
  = write "\"" >> writer >> write "\""

-- spaces
-- To surround the output of another action in single spaces
spaces :: StringBuilder -> StringBuilder
spaces writer
  = space >> writer >> space


-- sepBy
-- To intersperse a list of writers with a separating writer
sepBy :: StringBuilder -> [StringBuilder] -> StringBuilder
sepBy _ []
  = return ()
sepBy separator (writer:writers)
  = do
      writer
      mapM_ (separator >>) writers

-- commaSep
-- To intersperse commas/spaces between a list of writers
-- (reflecting common usage of sepBy)
commaSep :: [StringBuilder] -> StringBuilder
commaSep
  = sepBy (write ", ")

