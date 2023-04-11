module Data.SList where

data SList a
  = Nil
  | SList a :! a
  deriving stock Show

infixl 5 :!

toList :: SList a -> [a]
toList = go []
  where
    go acc Nil = reverse acc
    go acc (xs :! x) = go (x : acc) xs
    {-# INLINE go #-}
{-# INLINE toList #-}
