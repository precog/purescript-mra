module MRA.Data where

import Prelude (class Eq, class Ord, class Semigroup, class Show, Ordering(GT, LT, EQ), (==), (<$>), (>>>), (<<<), (<>), (<=), ($), (+), (-), compare, map, pure, show)

import Data.Monoid (class Monoid)
import Data.Accessor (Getter, Accessor(Accessor))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable(foldr, foldl)
import Data.String(singleton, toCharArray)
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
spaces n = if n <= 0 then "" else " " <> spaces (n - 1)

prettyArray :: Int -> Tuple Int String -> Data -> Tuple Int String
prettyArray n (Tuple i z) v = Tuple (i + 1) (z <> if i == 0 then " " else ",\n" <> spaces n <> (pretty v (n + 2)))

prettyMap :: Int -> Tuple Int String -> Tuple Data Data -> Tuple Int String
prettyMap n (Tuple i z) (Tuple k v) = Tuple (i + 1) (z <> if i == 0 then " " else ",\n" <> spaces n <> pretty k (n + 2) <> " : " <> pretty v (n + 2))

pretty :: Data -> Int -> String
pretty (Undefined             ) _ = "undefined"
pretty (Primitive (PrimNull  )) _ = "null"
pretty (Primitive (PrimInt  v)) _ = show v
pretty (Primitive (PrimChar v)) _ = "'" <> show v <> "'"
pretty (Array                v) n = "[" <> snd (foldl (prettyArray n) (Tuple 0 "") v) <> " ]"
pretty (Map                  v) n = "{" <> snd (foldl (prettyMap   n) (Tuple 0 "") (M.toList v)) <> " }"

emptyMap :: Data
emptyMap = Map M.empty

fieldAccessor :: String -> Accessor Data Data
fieldAccessor = primString >>> keyAccessor

asString :: Data -> Maybe String
asString d =
  case d of
    Array cs -> foldl (\s c -> s <> singleton c) "" <$> asString0 cs
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
  show (Primitive (PrimInt  v)) = "(primInt " <> show v <> ")"
  show (Primitive (PrimChar v)) = "(primChar " <> show v <> ")"
  show (Array                v) = fromMaybe ("(Array (" <> show v <> "))") ((\s -> "(primString " <> show s <> ")") <$> asString (Array v))
  show (Map                  v) = "(Map (" <> show v <> "))"
