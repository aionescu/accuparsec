module Data.DList where

newtype DList a = DList { ($:) :: [a] -> [a] }
infixr 4 $:

instance Semigroup (DList a) where
  (<>) :: DList a -> DList a -> DList a
  DList f <> DList g = DList (f . g)
  {-# INLINE (<>) #-}

instance Monoid (DList a) where
  mempty :: DList a
  mempty = DList id
  {-# INLINE mempty #-}

singleton :: a -> DList a
singleton x = DList (x :)
{-# INLINE singleton #-}

fromList :: [a] -> DList a
fromList xs = DList (xs <>)
{-# INLINE fromList #-}

toList :: DList a -> [a]
toList d = d $: []
{-# INLINE toList #-}

snoc' :: DList a -> a -> DList a
snoc' (DList f) !x = DList (f . (x :))
{-# INLINE snoc' #-}
