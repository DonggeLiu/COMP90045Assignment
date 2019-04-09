module Util.Combinators where

import Control.Applicative (liftA2)

import Text.Parsec

type Parser a
  = Parsec String () a

-- manyMN
-- Inspired by parsec's many and many1 combinators which gives us a way to
-- express Kleene * and +, this parser combinator gives us a way to express
-- minimum/maximum repetitions (that is, the {m, n} construct from regular
-- expressions).
-- Parse at least m and at most n occurrences of whatever p is looking for;
-- returning the result as a list.
-- Note: There may be more instances of p after this parser completes if it
-- found exactly n, these will be left to the next parser to parse (or reject).
manyMN :: Int -> Int -> (Parser a) -> (Parser [a])
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
sepByMN :: (Parser a) -> Int -> Int -> (Parser b) -> (Parser [b])
sepByMN sep m n p
  | m<0 || n<m = error "sepByMN expects 0 <= m <= n"
  | n == 0     = return []
  | m == 0     = option [] $ p <:> manyMN 0 (n-1) (sep >> p)
  | otherwise  = p <:> manyMN (m-1) (n-1) (sep >> p)


-- (<:>)
-- 'applicative cons' operator - A mnemonic shortcut for using cons as an
-- applicative function
(<:>) :: Parser a -> Parser [a] -> Parser [a]
(<:>)
  = liftA2 (:)
-- NOTE: This is effectively:
-- (<:>) a_x a_xs = (:) <$> a_x <*> a_xs
-- or, basically:
-- (<:>) m_x m_xs = do {x <- m_x; xs <- m_xs; return (x:xs)}

-- (<:>)
-- 'applicative append' operator - A mnemonic shortcut for using (++) as an
-- applicative function
(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>)
  = liftA2 (++)

