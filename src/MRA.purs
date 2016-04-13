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

module MRA.Data where
  import Prelude (class Eq, class Ord, class Semigroup, class Show, Ordering(GT, LT, EQ), (==), (<$>), (>>>), (<<<), (<>), (<=), ($), (++), (+), (-), append, compare, map, pure, show)
  import Data.Monoid (class Monoid)
  import Data.Accessor (Getter, Accessor(Accessor))
  import Data.Maybe (Maybe(..), fromMaybe)
  import Data.Foldable(foldr, foldl)
  import Data.String(toCharArray)
  import Data.Tuple(Tuple(..), snd)

  import Data.OrdMap as M
  import Data.List as L

  data Primitive = PrimNull | PrimInt Int | PrimChar Char -- etc

  data Data =
    Undefined               | -- Error!!!
    Primitive Primitive     |
    Array (L.List Data)     |
    Map   (M.Map Data Data)

  type DataAccessor = Accessor Data Data

  type DataGetter = Getter Data Data

  isDefined :: Data -> Boolean
  isDefined Undefined = false
  isDefined _         = true

  isWhollyDefined :: Data -> Boolean
  isWhollyDefined = foldData f true
    where
      f b Undefined = false
      f b _         = b

  foldData :: forall z. (z -> Data -> z) -> z -> Data -> z
  foldData f = foldData0
    where
      foldData0 z (Array l) = foldl foldData0 z l
      foldData0 z (Map   m) = foldl foldData0 (foldl foldData0 z (M.keys m)) (M.values m)
      foldData0 z v = f z v

  definedWith :: Data -> Data -> Data
  definedWith d1 d2 = if isDefined d1 then d1 else d2

  primInt :: Int -> Data
  primInt = Primitive <<< PrimInt

  primNull :: Data
  primNull = Primitive PrimNull

  primChar :: Char -> Data
  primChar = Primitive <<< PrimChar

  primString :: String -> Data
  primString = Array <<< foldr L.Cons L.Nil <<< map primChar <<< toCharArray

  makeMap :: Array (Tuple Data Data) -> Data
  makeMap = L.fromFoldable >>> M.fromList >>> Map

  spaces :: Int -> String
  spaces n = if n <= 0 then "" else " " ++ spaces (n - 1)

  prettyArray :: Int -> Tuple Int String -> Data -> Tuple Int String
  prettyArray n (Tuple i z) v = Tuple (i + 1) (z ++ if i == 0 then " " else ",\n" ++ spaces n ++ (pretty v (n + 2)))

  prettyMap :: Int -> Tuple Int String -> Tuple Data Data -> Tuple Int String
  prettyMap n (Tuple i z) (Tuple k v) = Tuple (i + 1) (z ++ if i == 0 then " " else ",\n" ++ spaces n ++ pretty k (n + 2) ++ " : " ++ pretty v (n + 2))

  pretty :: Data -> Int -> String
  pretty (Undefined             ) _ = "undefined"
  pretty (Primitive (PrimNull  )) _ = "null"
  pretty (Primitive (PrimInt  v)) _ = show v
  pretty (Primitive (PrimChar v)) _ = "'" ++ show v ++ "'"
  pretty (Array                v) n = "[" ++ snd (foldl (prettyArray n) (Tuple 0 "") v) ++ " ]"
  pretty (Map                  v) n = "{" ++ snd (foldl (prettyMap   n) (Tuple 0 "") (M.toList v)) ++ " }"

  emptyMap :: Data
  emptyMap = Map M.empty

  fieldAccessor :: String -> Accessor Data Data
  fieldAccessor = primString >>> keyAccessor

  asString :: Data -> Maybe String
  asString d =
    case d of
      Array cs -> foldl (\s c -> s ++ Data.Char.toString c) "" <$> asString0 cs
      _        -> Nothing
    where
      asString0 :: L.List Data -> Maybe (L.List Char)
      asString0 (L.Cons (Primitive (PrimChar c)) cs) = L.Cons c <$> asString0 cs
      asString0 L.Nil                                = Just L.Nil
      asString0 _                                    = Nothing

  indexAccessor :: Int -> Accessor Data Data
  indexAccessor idx = Accessor set get
    where
      set (Array l) v = fromMaybe Undefined (Array <$> L.insertAt idx v l)
      set        _  _ = Undefined

      get (Array l) = fromMaybe Undefined (l L.!! idx)
      get        _  = Undefined

  keyAccessor :: Data -> Accessor Data Data
  keyAccessor k = Accessor set get
    where
      set (Map m) v = Map $ M.insert k v m
      set _ _       = Undefined

      get (Map m) = fromMaybe Undefined (M.lookup k m)
      get _       = Undefined

  instance eqPrimitive :: Eq Primitive where
    eq PrimNull PrimNull = true
    eq (PrimInt  x) (PrimInt  y) = x == y
    eq (PrimChar x) (PrimChar y) = x == y
    eq _ _ = false

  instance ordPrimitive :: Ord Primitive where
    compare PrimNull PrimNull = EQ
    compare PrimNull _ = LT
    compare (PrimInt x) (PrimInt y) = compare x y
    compare (PrimInt _) (PrimNull) = GT
    compare (PrimInt _) _ = LT
    compare (PrimChar x) (PrimChar y) = compare x y
    compare (PrimChar _) (PrimNull) = GT
    compare (PrimChar _) (PrimInt _) = GT

  instance eqData :: Eq Data where
    eq Undefined Undefined = true
    eq (Primitive x) (Primitive y) = x == y
    eq (Array x) (Array y) = x == y
    eq (Map x) (Map y) = x == y
    eq _ _ = false

  instance ordData :: Ord Data where
    compare Undefined Undefined = EQ
    compare Undefined _ = LT
    compare _ Undefined = GT
    compare (Primitive x) (Primitive y) = compare x y
    compare (Primitive _) _ = LT
    compare _ (Primitive _) = GT
    compare (Array x) (Array y) = compare x y
    compare (Array x) _ = LT
    compare _ (Array y) = GT
    compare (Map x) (Map y) = compare x y

  instance semigroupData :: Semigroup Data where
    append Undefined y = y
    append x Undefined = x
    append (Array x) (Array y) = Array $ x <> y
    append (Array x) y = Array $ x <> (pure y)
    append (x) (Array y) = Array $ (pure x) <> y
    append (x) (y) = Array $ (pure x) <> (pure y)

  instance monoidData :: Monoid Data where
    mempty = Undefined

  instance showData :: Show Data where
    show (Undefined             ) = "Undefined"
    show (Primitive (PrimNull  )) = "primNull"
    show (Primitive (PrimInt  v)) = "(primInt " ++ show v ++ ")"
    show (Primitive (PrimChar v)) = "(primChar " ++ show v ++ ")"
    show (Array                v) = fromMaybe ("(Array (" ++ show v ++ "))") ((\s -> "(primString " ++ show s ++ ")") <$> asString (Array v))
    show (Map                  v) = "(Map (" ++ show v ++ "))"

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

  import Prelude(class Eq, class Functor, class Ord, class Semigroup, class Show, ($), (<$>), (++), (>>>), (==), (&&), compare, flip, map, pure, show)

  import Data.Map as M
  import Data.Monoid(class Monoid)
  import Data.Maybe(Maybe(), maybe)
  import Data.Tuple(Tuple(..), fst, snd)
  import Data.List(List(..), nub, reverse)
  import Data.Foldable(foldl)

  data Map k v = Map { reversed :: List k, values :: M.Map k v }

  alter :: forall k v. (Ord k) => (Maybe v -> Maybe v) -> k -> Map k v -> Map k v
  alter f k (Map r) = Map $ r { values = M.alter f k r.values }

  empty :: forall k v. Map k v
  empty = Map { reversed : Nil, values : M.empty }

  fromList :: forall k v. (Ord k) => List (Tuple k v) -> Map k v
  fromList l = Map { reversed : fst <$> reverse l, values : M.fromList l }

  insert :: forall k v. (Ord k) => k -> v -> Map k v -> Map k v
  insert k v (Map r) = Map { reversed : Cons k r.reversed, values : M.insert k v r.values }

  lookup :: forall k v. (Ord k) => k -> Map k v -> Maybe v
  lookup k (Map r) = M.lookup k r.values

  singleton :: forall k v. (Ord k) => k -> v -> Map k v
  singleton k v = Map { reversed : pure k, values : M.singleton k v }

  toList :: forall k v. (Ord k) => Map k v -> List (Tuple k v)
  toList (Map r) = foldl (\l k -> maybe l (Tuple k >>> flip Cons l) $ M.lookup k r.values) Nil r.reversed

  unionWith :: forall k v. (Ord k) => (v -> v -> v) -> Map k v -> Map k v -> Map k v
  unionWith f (Map r1) (Map r2) = Map { reversed : nub (r1.reversed ++ r2.reversed), values : M.unionWith f r1.values r2.values }

  values :: forall k v. (Ord k) => Map k v -> List v
  values = toList >>> map snd

  keys :: forall k v. (Ord k) => Map k v -> List k
  keys = toList >>> map fst

  instance semigroupMap :: (Ord k) => Semigroup (Map k v) where
    append (Map l) (Map r) = Map { reversed : nub (l.reversed ++ r.reversed), values : l.values ++ r.values }

  instance monoidMap :: (Ord k) => Monoid (Map k v) where
    mempty = empty

  instance eqMap :: (Ord k, Eq v) => Eq (Map k v) where
    eq (Map l) (Map r) = l.values == r.values && l.reversed == r.reversed

  instance ordMap :: (Ord k, Ord v) => Ord (Map k v) where
    compare l r = toList l `compare` toList r

  instance showMap :: (Ord k, Show k, Show v) => Show (Map k v) where
    show m = "fromList (" ++ show (toList m) ++ ")"

  instance functorMap :: (Ord k) => Functor (Map k) where
    map f (Map r) = Map r { values = map f r.values }

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

  toSet :: forall f a. (Foldable f, Eq a) => f a -> Set a
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

  import Prelude (class Eq, class Semigroup, class Show, (<$>), (++), (==), (/=), map, (>>>), (<<<), (<>), (>), ($), bind, eq, id, pure, return, show, const)
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
    return $ { left : fst left >>> key.left, right : fst right >>> key.right }

  joinOneOfs :: Provenance -> Provenance -> JoinKeys
  joinOneOfs l r = JoinKeys do
    left  <- flattenOneOf l
    right <- flattenOneOf r
    guard (snd left == snd right)
    key   <- unJoinKeys $ joinKeys (snd left) (snd right)
    return $ { left : fst left >>> key.left, right : fst right >>> key.right }

  joinThens :: Provenance -> Provenance -> JoinKeys
  joinThens l r = JoinKeys do
    lefts  <- flattenThen l
    rights <- flattenThen r
    let n = longestPrefix (snd <$> lefts) (snd <$> rights)
    guard (n > 0)
    let boths = zipWith Tuple ((take n) lefts) ((take n) rights)
    Tuple left right <- boths
    key  <- unJoinKeys $ joinKeys (snd left) (snd right)
    return $ { left : fst left >>> key.left, right : fst right >>> key.right }
    where
      longestPrefix :: forall a. (Eq a) => List a -> List a -> Int
      longestPrefix l r = length <<< takeWhile id $ zipWith eq l r

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
      return $ nest0 (get _Both >>> get _Left) l' <> nest0 (get _Both >>> get _Right) r'
  flattenBoth (OneOf l r) = nest (get _OneOf >>> get _Left) (flattenBoth l) <> nest (get _OneOf >>> get _Right) (flattenBoth r)
  flattenBoth v = pure (pure (Tuple id v))

  -- | Flattens sequences, correctly handling the distributivity of sums.
  -- | The result is a sum (Alternatives) of the flattened terms in the sequence.
  flattenThen :: Provenance -> Alternatives (List (Tuple DataGetter Provenance))
  flattenThen (Then l r) = do
    l' <- flattenThen l
    r' <- flattenThen r
    return $ nest0 (get _Then >>> get _Left) l' <> nest0 (get _Then >>> get _Right) r'
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
    show (Proj  v  ) = "(Proj " ++ show v ++ ")"
    show (Both  l r) = "(" ++ show l ++ ") /\\ (" ++ show r ++ ")"
    show (OneOf l r) = "(" ++ show l ++ ") \\/ (" ++ show r ++ ")"
    show (Then  l r) = "(" ++ show l ++ ") >> (" ++ show r ++ ")"

  thenEq :: Provenance -> Provenance -> Boolean
  thenEq l r = toSet (map snd <$> flattenThen l) == toSet (map snd <$> flattenThen r)

  bothEq :: Provenance -> Provenance -> Boolean
  bothEq l r = toSet (map snd >>> nubNadas >>> toSet <$> flattenBoth l) == toSet (map snd >>> nubNadas >>> toSet <$> flattenBoth r)

  oneOfEq :: Provenance -> Provenance -> Boolean
  oneOfEq l r = toSet (nubNadas (snd <$> flattenOneOf l)) == toSet (nubNadas (snd <$> flattenOneOf r))

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
  , peek_d
  , project_d
  , reduce_d
  , swap_d
  , values
  ) where

  import Prelude (class Ord, (>>=), (<$>), (>>>), (<<<), (<*>), ($), (-), (<>), (>), (<), bind, id, return, pure, map, flip)
  import Data.List (List(Nil, Cons), length, (..), zipWith, drop, take, (!!), head, filter, reverse, updateAt)
  import Data.OrdMap as M
  import Data.Set(Set(), fromList)
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

  -- | Replicates the last value in the dimensional stack into value space.
  peek_d :: Dataset -> Dataset
  peek_d (Dataset r) = Dataset $ r { values = f <$> r.values }
    where
      f v = set _Value v (fromMaybe Undefined $ head (get _Identity v))

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
      return $ makeIdentityValue i x, dims : zipBackwardsWithPadding Both id id l.dims r.dims }

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
          f v = fromList <<< filter isWhollyDefined $ fs <*> pure (get _Identity v)

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
  reduce_d :: (Data -> Data -> Data) -> Data -> Dataset -> Dataset
  reduce_d f z (Dataset r) =
    let
      reduce :: M.Map Data (List Data) -> List Data
      reduce m = M.values (foldl (\z -> liftToValue (f z)) z <$> m)
    in
      Dataset {
        dims   : drop 1 r.dims,
        values : reduce $ groupBy (liftToIdentity $ drop 1) r.values }

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
  groupBy f = foldl (flip \v -> M.alter (\old -> Just $ (fromMaybe mempty old) <> pure v) (f v)) M.empty

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

module MRA.Combinators where
  import Prelude ((>>>), (+), eq, id)

  import Data.OrdMap as M
  import Data.Maybe(fromMaybe)
  import Data.List((!!))

  import MRA.Data (Data(Map, Array, Primitive, Undefined), Primitive(PrimInt, PrimChar, PrimNull), primString, primInt)
  import MRA.Core (Dataset, autojoin_d, peek_d, lshift_d, map_d, nest_d, swap_d, reduce_d, project_d)

  -- | Fractures maps and arrays, no matter how deeply nested, into path
  -- | segments that terminate in leaves, stored in an array.
  fracture :: Dataset -> Dataset
  fracture = map_d id -- TODO

  -- | Inverse of `fracture`.
  -- | `fracture >>> unfracture = id`
  unfracture :: Dataset -> Dataset
  unfracture = map_d id -- TODO

  -- | Forms a new dimension by transforming the values.
  -- |
  -- | This dimension is swapped with the last one in the dimensional stack,
  -- | so that the next reduction eliminates whatever was the last dimension
  -- | prior to the grouping operation.
  group :: (Data -> Data) -> Dataset -> Dataset
  group f = map_d (\d -> Map (M.singleton (f d) d)) >>> lshift_d >>> swap_d 0 1

  -- | Flattens arrays and maps, without increasing the dimensionality of the
  -- | dataset.
  flatten :: Dataset -> Dataset
  flatten = lshift_d >>> nest_d

  -- | Flattens arrays and maps, returning the values of the new dimension, and
  -- | without increasing the dimensionality of the datset.
  flatten_id :: Dataset -> Dataset
  flatten_id = lshift_d >>> peek_d >>> nest_d

  -- | Coerces to some type.
  coerce :: (Data -> Boolean) -> Dataset -> Dataset
  coerce f = map_d (\d -> if f d then d else Undefined)

  -- | Coerce to ints.
  coerce_ints :: Dataset -> Dataset
  coerce_ints = coerce (typeOf >>> eq (primString "int"))

  -- | Coerce to chars.
  coerce_chars :: Dataset -> Dataset
  coerce_chars = coerce (typeOf >>> eq (primString "char"))

  -- | Coerces to maps. :: 'Map
  coerce_maps :: Dataset -> Dataset
  coerce_maps = coerce (typeOf >>> eq (primString "map"))

  -- | Coerces to arrays. :: 'Array
  coerce_arrays :: Dataset -> Dataset
  coerce_arrays = coerce (typeOf >>> eq (primString "array"))

  -- | Zooms into and returns the keys of maps. {_:}
  map_zoom_keys :: Dataset -> Dataset
  map_zoom_keys = map_zoom_values >>> peek_d

  -- | Zooms into and returns the values of maps. {_} / {:_}
  map_zoom_values :: Dataset -> Dataset
  map_zoom_values = coerce_maps >>> lshift_d

  -- | Zooms into and returns the indices of array elements. [_:]
  array_zoom_indices :: Dataset -> Dataset
  array_zoom_indices = array_zoom_values >>> peek_d

  -- | Zooms into and returns the values of array elements. [_] / [:_]
  array_zoom_values :: Dataset -> Dataset
  array_zoom_values = coerce_arrays >>> lshift_d

  -- | Flattens maps and returns the values. {*} / {:*}
  map_flatten_values :: Dataset -> Dataset
  map_flatten_values = coerce_maps >>> flatten

  -- | Flattens maps and returns the keys. {*:}
  map_flatten_keys :: Dataset -> Dataset
  map_flatten_keys = coerce_maps >>> flatten_id

  -- | Flattens arrays and returns the values. [*] / [:*]
  array_flatten_values :: Dataset -> Dataset
  array_flatten_values = coerce_arrays >>> flatten

  -- | Flattens arrays and returns the indices. [*:]
  array_flatten_indices :: Dataset -> Dataset
  array_flatten_indices = coerce_arrays >>> flatten_id

  -- | Projects a single statically known key from maps. foo.bar{1}
  static_project_key :: Data -> Dataset -> Dataset
  static_project_key k = coerce_maps >>> project_d k

  -- | Projects a single statically known index from arrays. foo[0][2]
  static_project_index :: Int -> Dataset -> Dataset
  static_project_index idx = coerce_arrays >>> project_d (Primitive (PrimInt idx))

  -- | Projects dynamic keys from maps. foo{bar}
  dynamic_project_key :: Dataset -> Dataset -> Dataset
  dynamic_project_key = autojoin_d f
    where
      f k (Map m) = fromMaybe Undefined (M.lookup k m)
      f _      _  = Undefined

  -- | Projects dynamic indices from arrays. foo[bar]
  dynamic_project_index :: Dataset -> Dataset -> Dataset
  dynamic_project_index = autojoin_d f
    where
      f (Primitive (PrimInt idx)) (Array a) = fromMaybe Undefined (a !! idx)
      f _                                _  = Undefined

  -- | Computes the type of a value, as a string. Note that the type of
  -- | `Undefined` is not defined (i.e. is `Undefined`).
  typeOf :: Data -> Data
  typeOf = f
    where
      f Undefined                = Undefined
      f (Primitive (PrimNull  )) = primString "null"
      f (Primitive (PrimChar _)) = primString "char"
      f (Primitive (PrimInt  _)) = primString "int"
      f (Array               _ ) = primString "array"
      f (Map                 _ ) = primString "map"

  -- | Count reduction.
  count :: Dataset -> Dataset
  count = reduce_d f (primInt 0)
    where
      f (Primitive (PrimInt v)) _ = primInt (v + 1)
      f (                    _) _ = Undefined

  -- join :: (Data -> Data -> Data) -> (Data -> Data -> Boolean) -> Dataset -> Dataset -> Dataset
