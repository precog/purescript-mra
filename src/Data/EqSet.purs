module Data.EqSet
  ( Set()
  , contains
  , toList
  , toSet
  ) where

import Prelude (class Eq, class Semigroup, ($), (&&), append, eq, (<<<))
import Data.List as L
import Data.Foldable(class Foldable, foldr, foldl, foldMap, all, any)
import Data.Monoid(class Monoid, mempty)

-- | An inefficient implementation of `Set` which does not require ordering on the values.
newtype Set a = Set (L.List a)

toSet :: forall f a. Foldable f => Eq a => f a -> Set a
toSet = Set <<< L.nub <<< foldr L.Cons L.Nil

toList :: forall a. Set a -> L.List a
toList (Set v) = v

contains :: forall a. (Eq a) => Set a -> Set a -> Boolean
contains (Set ls) (Set rs) = all (\v -> any (eq v) ls) rs

union :: forall a. (Eq a) => Set a -> Set a -> Set a
union (Set l) (Set r) = Set $ L.union l r

intersect :: forall a. (Eq a) => Set a -> Set a -> Set a
intersect (Set l) (Set r) = Set $ L.intersect l r

instance semigroupSet :: (Eq a) => Semigroup (Set a) where
  append (Set l) (Set r) = Set (append l r)

instance monoidSet :: (Eq a) => Monoid (Set a) where
  mempty = Set mempty

instance eqSet :: (Eq a) => Eq (Set a) where
  eq l r = (l `contains` r) && (r `contains` l)

instance foldableSet :: Foldable Set where
  foldr f z (Set v) = foldr f z v

  foldl f z (Set v) = foldl f z v

  foldMap f (Set v) = foldMap f v
