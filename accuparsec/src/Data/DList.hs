module Data.DList where

import Data.Monoid(Endo(..))

newtype DList a =
  DList { ($:) :: [a] -> [a] }
  deriving (Semigroup, Monoid) via Endo [a]

infixr 4 $:

singleton :: a -> DList a
singleton x = DList (x :)

fromList :: [a] -> DList a
fromList xs = DList (xs <>)

toList :: DList a -> [a]
toList d = d $: []
