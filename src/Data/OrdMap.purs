module Data.OrdMap
  ( Map()
  , alter
  , empty
  , fromList
  , insert
  , keys
  , lookup
  , singleton
  , toList
  , unionWith
  , values ) where

import Prelude(class Eq, class Functor, class Ord, class Semigroup, class Show, ($), (<$>), (<>), (>>>), (==), (&&), compare, eq, flip, map, pure, show)

import Data.Map as M
import Data.Monoid(class Monoid)
import Data.Maybe(Maybe(), maybe)
import Data.Tuple(Tuple(..), fst, snd)
import Data.List(List(..), nub, reverse)
import Data.Foldable(foldl, any)

data Map k v = Map { reversed :: List k, values :: M.Map k v }

alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
alter f k (Map r) = Map $ { reversed : if any (eq k) r.reversed then r.reversed else Cons k r.reversed, values : M.alter f k r.values }

empty :: forall k v. Map k v
empty = Map { reversed : Nil, values : M.empty }

fromList :: forall k v. (Ord k) => List (Tuple k v) -> Map k v
fromList l = Map { reversed : fst <$> reverse l, values : M.fromFoldable l }

insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
insert k v (Map r) = Map { reversed : Cons k r.reversed, values : M.insert k v r.values }

lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
lookup k (Map r) = M.lookup k r.values

singleton :: forall k v. (Ord k) => k -> v -> Map k v
singleton k v = Map { reversed : pure k, values : M.singleton k v }

toList :: forall k v. (Ord k) => Map k v -> List (Tuple k v)
toList (Map r) = foldl (\l k -> maybe l (Tuple k >>> flip Cons l) $ M.lookup k r.values) Nil r.reversed

unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f (Map r1) (Map r2) = Map { reversed : nub (r1.reversed <> r2.reversed), values : M.unionWith f r1.values r2.values }

values :: forall k v. (Ord k) => Map k v -> List v
values = toList >>> map snd

keys :: forall k v. (Ord k) => Map k v -> List k
keys = toList >>> map fst

instance semigroupMap :: (Ord k) => Semigroup (Map k v) where
  append (Map l) (Map r) = Map { reversed : nub (l.reversed <> r.reversed), values : l.values <> r.values }

instance monoidMap :: (Ord k) => Monoid (Map k v) where
  mempty = empty

instance eqMap :: (Ord k, Eq v) => Eq (Map k v) where
  eq (Map l) (Map r) = l.values == r.values && l.reversed == r.reversed

instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
  compare l r = toList l `compare` toList r

instance showMap :: (Ord k, Show k, Show v) => Show (Map k v) where
  show m = "fromList (" <> show (toList m) <> ")"

instance functorMap :: (Ord k) => Functor (Map k) where
  map f (Map r) = Map r { values = map f r.values }
