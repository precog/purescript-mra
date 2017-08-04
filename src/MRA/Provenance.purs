module MRA.Provenance
  ( Provenance(..)
  , JoinKeys(..)
  , (\/)
  , (/\)
  , (>>)
  , _Both
  , _Left
  , _OneOf
  , _Right
  , _Then
  , both
  , joinKeys
  , makeBoth
  , makeOneOfLeft
  , makeOneOfRight
  , makeThen
  , oneOf
  , then0
  , unJoinKeys
  ) where

import Prelude (class Eq, class Semigroup, class Show, (<$>), (==), (/=), map, (>>>), (<<<), (<>), (>), ($), bind, eq, id, pure, show, const, discard)

import Data.List (List(Nil), length, take, filter, takeWhile, zipWith)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.EqSet (toSet)
import Data.Monoid(class Monoid, mempty)
import Data.Profunctor.Strong(first)
import Control.MonadPlus(guard)

import Data.Accessor (get, set)
import MRA.Data (DataGetter, Data, DataAccessor, fieldAccessor, emptyMap)

data Provenance
  = Nada
  | Value
  | Proj  Data
  | Both  Provenance Provenance
  | OneOf Provenance Provenance
  | Then  Provenance Provenance

data JoinKeys = JoinKeys (List { left :: DataGetter, right :: DataGetter })

unJoinKeys :: JoinKeys -> (List { left :: DataGetter, right :: DataGetter })
unJoinKeys (JoinKeys jk) = jk

oneOf :: Provenance -> Provenance -> Provenance
oneOf = OneOf

both :: Provenance -> Provenance -> Provenance
both = Both

then0 :: Provenance -> Provenance -> Provenance
then0 = Then

infix 6 oneOf as \/
infix 6 both as /\
infix 6 then0 as >>

_Left :: DataAccessor
_Left = fieldAccessor "Left"

_Right :: DataAccessor
_Right = fieldAccessor "Right"

_Both :: DataAccessor
_Both = fieldAccessor "Both"

_Then :: DataAccessor
_Then = fieldAccessor "Then"

_OneOf :: DataAccessor
_OneOf = fieldAccessor "OneOf"

makeBoth :: Data -> Data -> Data
makeBoth = makeTwo _Both

makeThen :: Data -> Data -> Data
makeThen = makeTwo _Then

makeOneOfLeft :: Data -> Data
makeOneOfLeft v = set _OneOf emptyMap (set _Left emptyMap v)

makeOneOfRight :: Data -> Data
makeOneOfRight v = set _OneOf emptyMap (set _Right emptyMap v)

makeTwo :: DataAccessor -> Data -> Data -> Data
makeTwo d l r = set d emptyMap (set _Left (set _Right emptyMap r) l)

valueJoin :: JoinKeys
valueJoin = JoinKeys (pure { left : id :: DataGetter, right : id :: DataGetter })

joinKeys :: Provenance -> Provenance -> JoinKeys
joinKeys     (Nada     ) r               = mempty
joinKeys l                   (Nada     ) = mempty
joinKeys     (Value    )     (Value    ) = valueJoin
joinKeys     (Proj   d1)     (Proj   d2) = if d1 == d2 then valueJoin else mempty
joinKeys     (Value    )     (Proj   d2) = JoinKeys (pure { left : id :: DataGetter, right : const d2 })
joinKeys     (Proj   d1)     (Value    ) = JoinKeys (pure { left : const d1, right : id :: DataGetter })
joinKeys l @ (Both  _ _) r               = joinBoths  l r
joinKeys l               r @ (Both  _ _) = joinBoths  l r
joinKeys l @ (OneOf _ _) r               = joinOneOfs l r
joinKeys l               r @ (OneOf _ _) = joinOneOfs l r
joinKeys l @ (Then  _ _) r               = joinThens  l r
joinKeys l               r @ (Then  _ _) = joinThens  l r

joinBoths :: Provenance -> Provenance -> JoinKeys
joinBoths l r = JoinKeys do
  lefts  <- flattenBoth l
  rights <- flattenBoth r
  left   <- lefts
  right  <- rights
  guard (snd left == snd right)
  key    <- unJoinKeys $ joinKeys (snd left) (snd right)
  pure { left : fst left >>> key.left, right : fst right >>> key.right }

joinOneOfs :: Provenance -> Provenance -> JoinKeys
joinOneOfs l r = JoinKeys do
  left  <- flattenOneOf l
  right <- flattenOneOf r
  guard (snd left == snd right)
  key   <- unJoinKeys $ joinKeys (snd left) (snd right)
  pure { left : fst left >>> key.left, right : fst right >>> key.right }

joinThens :: Provenance -> Provenance -> JoinKeys
joinThens l r = JoinKeys do
  lefts  <- flattenThen l
  rights <- flattenThen r
  let n = longestPrefix (snd <$> lefts) (snd <$> rights)
  guard (n > 0)
  let boths = zipWith Tuple ((take n) lefts) ((take n) rights)
  Tuple left right <- boths
  key  <- unJoinKeys $ joinKeys (snd left) (snd right)
  pure { left : fst left >>> key.left, right : fst right >>> key.right }
  where
    longestPrefix :: forall a. (Eq a) => List a -> List a -> Int
    longestPrefix l' r' = length <<< takeWhile id $ zipWith eq l' r'

type Alternatives a = List a

type Flattening = Alternatives (List (Tuple DataGetter Provenance))

nest0 :: DataGetter -> List (Tuple DataGetter Provenance) -> List (Tuple DataGetter Provenance)
nest0 root v = first ((>>>) root) <$> v

nest :: DataGetter -> Flattening -> Flattening
nest root alts = nest0 root <$>  alts

-- | Flattens products, correctly handling the distributivity of sums.
-- | The result is a sum (Alternatives) of the flattened terms in the product.
flattenBoth :: Provenance -> Alternatives (List (Tuple DataGetter Provenance))
flattenBoth (Both  l r) =
  do
    l' <- flattenBoth l
    r' <- flattenBoth r
    pure $ nest0 (get _Both >>> get _Left) l' <> nest0 (get _Both >>> get _Right) r'
flattenBoth (OneOf l r) = nest (get _OneOf >>> get _Left) (flattenBoth l) <> nest (get _OneOf >>> get _Right) (flattenBoth r)
flattenBoth v = pure (pure (Tuple id v))

-- | Flattens sequences, correctly handling the distributivity of sums.
-- | The result is a sum (Alternatives) of the flattened terms in the sequence.
flattenThen :: Provenance -> Alternatives (List (Tuple DataGetter Provenance))
flattenThen (Then l r) = do
  l' <- flattenThen l
  r' <- flattenThen r
  pure $ nest0 (get _Then >>> get _Left) l' <> nest0 (get _Then >>> get _Right) r'
flattenThen (OneOf l r) = nest (get _OneOf >>> get _Left) (flattenThen l) <> nest (get _OneOf >>> get _Right) (flattenThen r)
flattenThen v = pure (pure (Tuple id v))

-- | Flattens sums, correctly handling the distributivity of sums.
-- | The result is a sum (Alternatives) of the flattened terms.
flattenOneOf :: Provenance -> Alternatives (Tuple DataGetter Provenance)
flattenOneOf (Both  l r) = nest0 (get _Both  >>> get _Left) (flattenOneOf l) <> nest0 (get _Both  >>> get _Right) (flattenOneOf r)
flattenOneOf (Then  l r) = nest0 (get _Then  >>> get _Left) (flattenOneOf l) <> nest0 (get _Then  >>> get _Right) (flattenOneOf r)
flattenOneOf (OneOf l r) = nest0 (get _OneOf >>> get _Left) (flattenOneOf l) <> nest0 (get _OneOf >>> get _Right) (flattenOneOf r)
flattenOneOf v = pure (Tuple id v)

nubNadas :: List Provenance -> List Provenance
nubNadas = filter ((/=) Nada)

instance semigroupJoinKeys :: Semigroup JoinKeys where
  append (JoinKeys l) (JoinKeys r) =
    JoinKeys (l <> r)

instance monoidJoinKeys :: Monoid JoinKeys where
  mempty = JoinKeys Nil

instance eqProvenance :: Eq Provenance where
  eq     (Nada       )     (Nada       ) = true
  eq     (Value      )     (Value      ) = true
  eq     (Proj      l)     (Proj      r) = l == r
  eq l @ (Both    _ _) r                 = bothEq l r
  eq l                 r @ (Both    _ _) = bothEq l r
  eq l @ (OneOf   _ _) r                 = oneOfEq l r
  eq l                 r @ (OneOf   _ _) = oneOfEq l r
  eq l @ (Then    _ _) r                 = thenEq l r
  eq l                 r @ (Then    _ _) = thenEq l r
  eq                _                 _  = false

instance showProvenance :: Show Provenance where
  show (Nada     ) = "Nada"
  show (Value    ) = "Value"
  show (Proj  v  ) = "(Proj " <> show v <> ")"
  show (Both  l r) = "(" <> show l <> ") /\\ (" <> show r <> ")"
  show (OneOf l r) = "(" <> show l <> ") \\/ (" <> show r <> ")"
  show (Then  l r) = "(" <> show l <> ") >> (" <> show r <> ")"

thenEq :: Provenance -> Provenance -> Boolean
thenEq l r = toSet (map snd <$> flattenThen l) == toSet (map snd <$> flattenThen r)

bothEq :: Provenance -> Provenance -> Boolean
bothEq l r = toSet (map snd >>> nubNadas >>> toSet <$> flattenBoth l) == toSet (map snd >>> nubNadas >>> toSet <$> flattenBoth r)

oneOfEq :: Provenance -> Provenance -> Boolean
oneOfEq l r = toSet (nubNadas (snd <$> flattenOneOf l)) == toSet (nubNadas (snd <$> flattenOneOf r))
