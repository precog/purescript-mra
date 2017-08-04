module MRA.Core
  ( Dataset()
  , autojoin_d
  , dimensionality
  , filter_d
  , identities
  , literal_d
  , lshift_d
  , map_d
  , nest_d
  , project_d
  , reduce_d
  , swap_d
  , values
  ) where

import Prelude (class Ord, class Show, (>>=), (<$>), (>>>), (<<<), (<*>), (<>), ($), (-), (>), (<), bind, flip, id, pure, map, show)

import Data.List (List(Nil, Cons), length, (..), zipWith, drop, take, (!!), filter, reverse, updateAt)
import Data.OrdMap as M
import Data.Set(Set(), fromFoldable)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Tuple (Tuple(Tuple))
import Data.Monoid(mempty)
import Data.Accessor(Accessor(..), xmap, set, get)
import Data.Foldable(foldl)

import MRA.Provenance (Provenance(..), JoinKeys(..), joinKeys, makeBoth, makeThen, makeOneOfLeft, makeOneOfRight)
import MRA.Data (Data(Map, Undefined, Array, Primitive), Primitive(PrimInt), emptyMap, primInt, fieldAccessor, isWhollyDefined)

data Dataset = Dataset { dims :: List Provenance, values :: List Data }

-- | Returns the static dimensionality of the dataset, AKA the length of the
-- | dimensional stack.
dimensionality :: Dataset -> Int
dimensionality (Dataset r) = length r.dims

-- | Applies a 1-to-1 value transformation over a dataset.
map_d :: (Data -> Data) -> Dataset -> Dataset
map_d f (Dataset r) = Dataset $ r { values = bimapIdentityValue id f r.values }

-- | Filters a dataset based on a boolean predicate applied to the values.
filter_d :: (Data -> Boolean) -> Dataset -> Dataset
filter_d f (Dataset r) = Dataset $ r { values = filter (get _Value >>> f) r.values }

-- | Projects a key / index from arrays and maps.
project_d :: Data -> Dataset -> Dataset
project_d d (Dataset r) =
  Dataset { values : bimapIdentityValue (extend makeThen d) f r.values, dims : extend Then (Proj d) r.dims }
  where
    f (Map   v) = fromMaybe Undefined (M.lookup d v)
    f (Array v) = case d of
                    (Primitive (PrimInt idx)) -> fromMaybe Undefined $ v !! idx
                    _                         -> Undefined
    f (_      ) = Undefined

-- | Swaps two dimensions in the dimensional stack. The dimension with index
-- | `0` is the dimension at the top / head of the stack, i.e. the *current*
-- | dimension.
swap_d :: Int -> Int -> Dataset -> Dataset
swap_d n m (Dataset r) =
  Dataset { values : liftToIdentity (listSwap n m) <$> r.values, dims : listSwap n m r.dims }

-- | Performs a left dimensional shift, peeling off a dimension from value
-- | space and pushing it on as a new dimension of identity space.
-- |
-- | The new dimension is added to the head of the dimensional stack.
lshift_d :: Dataset -> Dataset
lshift_d (Dataset r) =
  Dataset { values : r.values >>= f, dims : Cons Value r.dims }
  where
    f v =
      let
        i = get _Identity v
        x = get _Value    v
      in case x of
        Array l -> lshiftd1 i $ zipWith Tuple (primInt <$> (0 .. (length l - 1))) l
        Map   m -> lshiftd1 i $ M.toList m
        _       -> Cons (makeIdentityValue (Cons x i) Undefined) Nil

    lshiftd1 :: List Data -> List (Tuple Data Data) -> List Data
    lshiftd1 i = map (\(Tuple i0 v) -> makeIdentityValue (Cons i0 i) v)

-- | Nests the current dimension within the previous dimension.
-- | AKA "squash1".
nest_d :: Dataset -> Dataset
nest_d (Dataset r) =
  Dataset { values : liftToIdentity (squash makeThen) <$> r.values, dims : (squash Then) r.dims }
  where
    squash :: forall a. (a -> a -> a) -> List a -> List a
    squash f (Cons x xs) = extend f x xs
    squash f Nil         = Nil

-- | Performs a cartesian cross product of the specified datasets, passing
-- | each pair to the provided function to obtain a new value.
-- |
-- | The resulting dataset has dimensionality equal to the greater
-- | dimensionality of the two input datasets.
cross_d :: (Data -> Data -> Data) -> Dataset -> Dataset -> Dataset
cross_d f (Dataset l) (Dataset r) =
  Dataset { values : do
    left  <- l.values
    right <- r.values
    let i = zipBackwardsWithPadding makeBoth id id (get _Identity left) (get _Identity right)
    let x = f (get _Value left) (get _Value right)
    pure $ makeIdentityValue i x, dims : zipBackwardsWithPadding Both id id l.dims r.dims }

-- | Performs a set union of the two datasets.
union_d :: Dataset -> Dataset -> Dataset
union_d (Dataset l) (Dataset r) =
  Dataset {
    values : bimapIdentityValue (map makeOneOfLeft) id l.values <> bimapIdentityValue (map makeOneOfRight) id r.values,
    dims   : zipBackwardsWithPadding OneOf (\l -> OneOf l Nada) (\r -> OneOf Nada r) l.dims r.dims }

type JoinMap = M.Map (Set Data) (List Data)

-- | Performs an auto-join between two datasets, using the specified function
-- | to combine the values.
autojoin_d :: (Data -> Data -> Data) -> Dataset -> Dataset -> Dataset
autojoin_d f (Dataset l) (Dataset r) =
  let
    allKeys :: List { left :: List Data -> Data, right :: List Data -> Data }
    allKeys =
      let
        zipped :: List JoinKeys
        zipped  = zipWith joinKeys l.dims r.dims

        zipped' :: List (Tuple Int JoinKeys)
        zipped' = zipWith Tuple (0 .. (length zipped - 1)) zipped

        f :: Tuple Int JoinKeys -> List { left :: List Data -> Data, right :: List Data -> Data }
        f (Tuple idx (JoinKeys jk)) = (\k -> { left  : get (_index idx) >>> k.left,
                                               right : get (_index idx) >>> k.right }) <$> jk
      in
        zipped' >>= f

    groupByJoinKeys :: List (List Data -> Data) -> List Data -> JoinMap
    groupByJoinKeys fs vs = groupBy f vs
      where
        f v = fromFoldable <<< filter isWhollyDefined $ fs <*> pure (get _Identity v)

    leftGrouped :: JoinMap
    leftGrouped  = groupByJoinKeys (_.left  <$> allKeys) l.values

    rightGrouped :: JoinMap
    rightGrouped = groupByJoinKeys (_.right <$> allKeys) r.values

    grouped :: List (Tuple (List Data) (List Data))
    grouped = M.values (M.unionWith (\(Tuple l1 r1) (Tuple l2 r2) -> Tuple (l1 <> l2) (r1 <> r2))
              (flip Tuple Nil <$> leftGrouped) (Tuple Nil <$> rightGrouped))

    newDims :: List Provenance
    newDims = zipBackwardsWithPadding Both id id l.dims r.dims

    f' :: Data -> Data -> Data
    f' l r =
      let
        i  = zipBackwardsWithPadding makeBoth id id (get _Identity l) (get _Identity r)
        x  = f (get _Value l) (get _Value r)
      in
        makeIdentityValue i x

    joiner :: Tuple (List Data) (List Data) -> List Data
    joiner (Tuple (l @ Nil) (r @ Nil)) = pure (f' Undefined Undefined)
    joiner (Tuple (l @ Nil) (r      )) = f' <$> (pure Undefined) <*> r
    joiner (Tuple (l      ) (r @ Nil)) = f' <$> l <*> (pure Undefined)
    joiner (Tuple (l      ) (r      )) = f' <$> l <*> r

    newVals :: List Data
    newVals = reverse $ foldl (\l e -> l <> joiner e) Nil grouped
  in
    Dataset { dims : newDims, values : newVals }

-- | Peels off the current dimension by reducing over all values that have the
-- | same position in (n-1)th dimensional space.
-- |
-- | NOTE: This definition is currently broken, in the sense that it attempts
-- |       no normalization, so two points in the same (n-1th) location
-- |       may not fall in the same reduction bucket because the structural
-- |       representation of their location may be different.
reduce_d :: (Data -> Data -> Data) -> Data -> Dataset -> Dataset
reduce_d f z (Dataset r) =
  let
    reduce :: M.Map (List Data) (List Data) -> List Data
    reduce = M.toList >>> (map $ \(Tuple i vs) -> makeIdentityValue i (foldl (\z d -> f z (get _Value d)) z vs))
  in
    Dataset {
      dims   : drop 1 r.dims,
      values : reduce $ groupBy (\d -> drop 1 (get _Identity d)) r.values }

-- | Lifts a literal value into a data set. This is the only possible way of
-- | constructing a dataset.
literal_d :: Data -> Dataset
literal_d d = Dataset { dims : Nil, values : pure (makeIdentityValue Nil d) }

values :: Dataset -> List Data
values (Dataset r) = (get _Value) <$> r.values

identities :: Dataset -> List (List Data)
identities (Dataset r) = (get _Identity) <$> r.values

-- | Values are stored in a "Value" fieldAccessor of a top-level map.
_Value :: Accessor Data Data
_Value = fieldAccessor "Value"

-- | Identity is stored in a "Identity" fieldAccessor of a top-level map.
-- |
-- | The dimensions of identity are stored in a stack, here represented by
-- | a list.
_Identity :: Accessor Data (List Data)
_Identity = xmap Array d (fieldAccessor "Identity")
  where
    d (Array v) = v
    d        _  = Nil

_index :: Int -> Accessor (List Data) Data
_index idx = Accessor set get
  where
    set l e = fromMaybe Nil (updateAt idx e l)

    get l = fromMaybe Undefined (l !! idx)

groupBy :: forall k v. (Ord k) => (v -> k) -> List v -> M.Map k (List v)
groupBy f = foldl (flip \v -> M.alter (\old -> Just $ fromMaybe mempty old <> pure v) (f v)) M.empty

zipBackwardsWithPadding :: forall a b c. (a -> b -> c) -> (a -> c) -> (b -> c) -> List a -> List b -> List c
zipBackwardsWithPadding fab fa fb l r = zip0 (reverse l) (reverse r)
  where
    zip0 (Cons a as) (Nil      ) = Cons (fa a) (zip0 as Nil)
    zip0 (Cons a as) (Cons b bs) = Cons (fab a b) (zip0 as bs)
    zip0 (Nil      ) (Cons b bs) = Cons (fb b) (zip0 Nil bs)
    zip0 (Nil      ) (Nil      ) = Nil

liftToValue :: (Data -> Data) -> (Data -> Data)
liftToValue f = \d -> set _Value d (f (get _Value d))

liftToIdentity :: (List Data -> List Data) -> (Data -> Data)
liftToIdentity f = \d -> set _Identity d (f (get _Identity d))

makeIdentityValue :: List Data -> Data -> Data
makeIdentityValue i v = set _Value (set _Identity emptyMap i) v

bimapIdentityValue :: (List Data -> List Data) -> (Data -> Data) -> List Data -> List Data
bimapIdentityValue fi fv v = liftToIdentity fi >>> liftToValue fv <$> v

extend :: forall a. (a -> a -> a) -> a -> List a -> List a
extend f x0 (Cons x xs) = Cons (f x0 x) xs
extend f x0 (Nil      ) = Cons x0 Nil

listSwap :: forall a. Int -> Int -> List a -> List a
listSwap n m l =
  let
    n' = if n < m then n else m
    m' = if m > n then m else n

    list :: forall b. Maybe b -> List b
    list Nothing = Nil
    list (Just a) = Cons a Nil

    listIdx :: forall b. Int -> List b -> List b
    listIdx idx l = list $ l !! idx
  in
    (take    n' l  <>
     listIdx m' l  <>
     take    (m' - n' - 1) (drop n' l) <>
     listIdx n' l  <>
     drop    m' l)

instance showDataset :: Show Dataset where
  show (Dataset r) = "Dataset { values : " <> show r.values <> ", dims : " <> show r.dims <> "}"
