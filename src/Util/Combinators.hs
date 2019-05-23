module Util.Combinators where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
-- 
--                      UTIL - ADDITIONAL GENERAL PARSER COMBINATORS
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

import Control.Applicative (liftA2)

import Text.Parsec
import Text.Parsec.Pos

-- ----------------------------------------------------------------------------
-- Automatically annotating parser results with start position
-- ----------------------------------------------------------------------------

-- withPosition
-- Combinator to automatically supply the result of a parser with a position.
-- The given parser should return a function that is expecting a position in
-- order to complete its annotation.
withPosition :: Stream s m t =>
                ParsecT s u m (SourcePos -> a) -> ParsecT s u m a
withPosition parser
  = do
      position <- getPosition
      result <- parser
      return (result position)


-- ----------------------------------------------------------------------------
-- Parsing for lists of bounded size
-- ----------------------------------------------------------------------------

-- manyMN
-- Inspired by parsec's many and many1 combinators which gives us a way to
-- express Kleene * and +, this parser combinator gives us a way to express
-- minimum/maximum repetitions (that is, the {m, n} construct from regular
-- expressions).
-- Parse at least m and at most n occurrences of whatever p is looking for;
-- returning the result as a list.
-- Note: There may be more instances of p after this parser completes if it
-- found exactly n, these will be left to the next parser to parse (or reject).
manyMN :: Stream s m t => 
          Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
manyMN m n p
  | m<0 || n<m = error "manyMN expects 0 <= m <= n"
  | n == 0     = return []
  | m == 0     = option [] $ p <:> manyMN 0 (n-1) p
  | otherwise  = p <:> manyMN (m-1) (n-1) p

-- sepByMN
-- Insipired by parsec's sepBy1 and sepBy, this combinator allows us to
-- look for a list of results from parser `p`, parsing a `sep` in between
-- each one.
-- It mainly relies on `manyMN (m-1) (n-1) (sep >> p)' but we must take
-- some care to treat the first instance correctly (it may be not allowed,
-- optional, or mandatory depending on m and n) and set off the right call
-- to manyMN.
sepByMN :: Stream s m t =>
           ParsecT s u m a -> Int -> Int -> ParsecT s u m b -> ParsecT s u m [b]
sepByMN sep m n p
  | m<0 || n<m = error "sepByMN expects 0 <= m <= n"
  | n == 0     = return []
  | m == 0     = option [] $ p <:> manyMN 0 (n-1) (sep >> p)
  | otherwise  = p <:> manyMN (m-1) (n-1) (sep >> p)


-- ----------------------------------------------------------------------------
-- Automatically combining the results of list-producing parsers
-- ----------------------------------------------------------------------------

-- (<:>)
-- 'applicative cons' operator - A mnemonic shortcut for using cons as an
-- applicative function, to combine the results of parsec parsers.
(<:>) :: Stream s m t =>
         ParsecT s u m a -> ParsecT s u m [a] -> ParsecT s u m [a]
(<:>)
  = liftA2 (:)
-- NOTE: This is effectively:
--     (<:>) p_x p_xs = (:) <$> p_x <*> p_xs
-- or, basically:
--     (<:>) p_x p_xs = do {x <- p_x; xs <- p_xs; return (x:xs)}

-- (<++>)
-- 'applicative append' operator - A mnemonic shortcut for using (++) as an
-- applicative function, to combine the results of parsec parsers.
(<++>) :: Stream s m t =>
          ParsecT s u m [a] -> ParsecT s u m [a] -> ParsecT s u m [a]
(<++>)
  = liftA2 (++)
-- NOTE: This is effectively:
--     (<++>) p_xs p_ys = (++) <$> p_xs <*> p_ys
-- or, basically:
--     (<++>) p_xs p_ys = do {xs <- p_xs; ys <- p_ys; return (xs++ys)}
