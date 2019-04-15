module Util.DiffList where

-- ----------------------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Assignment Stage 1
--
--                     UTILITY - DIFFERENCE LISTS
--
-- ----------------------------------------------------------------------------

import Data.Semigroup

-- ----------------------------------------------------------------------------
-- Storing lists as functions representing 'list differences' gives us efficient
-- append functionality through function composition! This will be very useful
-- for our string builder.
-- ----------------------------------------------------------------------------

-- A 'difference list' is just a function that will prepend a particular
-- list to its argument. That is, a function from lists to lists.
newtype DiffList a
  = DiffList ([a] -> [a])

-- So to represent a list, we create a function that prepends that list.
-- An operator section with ++ will do the trick!
dlistify :: [a] -> DiffList a
dlistify l
  = DiffList (l++)

-- And to convert a difference list back to a normal list we just apply
-- the function to an empty list (leaving only the 'difference' part!)
listify :: DiffList a -> [a]
listify (DiffList d)
  = d []


-- We require our difference lists to be a member of the Monoid
-- typeclass* with an efficient implementation of mappend. That way,
-- the Writer monad can use them efficiently.

-- * On nutmeg2's version of GHC, we also need to instance Semigroup for
-- some reason... Note that (<>) is just mappend.
instance Semigroup (DiffList a) where
  -- To append two list differences, we'll compose them to get a new function
  -- that prepends the second difference and then prepends the first after
  -- that.
  (<>) (DiffList d1) (DiffList d2)
    = DiffList (d1 . d2)

instance Monoid (DiffList a) where
  -- The 'empty difference' is just a function that prepends *nothing*; id
  mempty
    = DiffList id
  mappend
    = (<>)

-- Oh, and, if we ever want to show a diff list (e.g. for debugging
-- or testing), let's just convert it to a regular list first.
instance (Show a) => Show (DiffList a) where
  show d
    = show (listify d)
