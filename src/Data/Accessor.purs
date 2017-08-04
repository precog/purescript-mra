module Data.Accessor where

import Prelude (class Category, class Semigroupoid, id, const)

type Setter s a = s -> a -> s
type Getter s a = s -> a

data Accessor s a = Accessor (Setter s a) (Getter s a)

instance semigroupoidAccessor :: Semigroupoid Accessor where
  compose (Accessor setC getC) (Accessor setB getB) = Accessor set' get'
    where
      set' a c = setB a (setC (getB a) c)

      get' a = getC (getB a)

instance categoryAccessor :: Category Accessor where
  id = Accessor const id

xmap :: forall s a b. (b -> a) -> (a -> b) -> Accessor s a -> Accessor s b
xmap f g (Accessor setter getter) = Accessor setter' getter'
  where
    setter' s b = setter s (f b)
    getter' s   = g (getter s)

set :: forall s a. Accessor s a -> s -> a -> s
set (Accessor f _) = f

get :: forall s a. Accessor s a -> s -> a
get (Accessor _ g) = g
