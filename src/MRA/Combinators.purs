module MRA.Combinators where

import Prelude ((>>>), (<$>), (+), (-), eq, id)

import Data.OrdMap as M
import Data.Maybe(fromMaybe)
import Data.Tuple(Tuple(..))
import Data.List((!!), (..), zipWith, length)

import MRA.Data (Data(Map, Array, Primitive, Undefined), Primitive(PrimInt, PrimChar, PrimNull), primString, primInt)
import MRA.Core (Dataset, autojoin_d, lshift_d, map_d, nest_d, swap_d, reduce_d, project_d)

-- | Fractures maps and arrays, no matter how deeply nested, into path
-- | segments that terminate in leaves, stored in an array.
fracture :: Dataset -> Dataset
fracture = map_d id -- TODO

-- | Inverse of `fracture`.
-- | `fracture >>> unfracture = id`
unfracture :: Dataset -> Dataset
unfracture = map_d id -- TODO

-- | Replicates the next dimension of information.
replicate :: Dataset -> Dataset
replicate = map_d f
  where
    f (Map    v) = (M.fromList >>> Map) ((\(Tuple k v) -> Tuple k k) <$> M.toList v)
    f (Array v0) = let v = zipWith Tuple (primInt <$> (0 .. (length v0 - 1))) v0 in (M.fromList >>> Map) ((\(Tuple i e) -> Tuple i i) <$> v)
    f v          = Map (M.singleton v Undefined)

-- | Pulls the domain out of the partial functions described by maps and arrays.
domain :: Dataset -> Dataset
domain = replicate >>> lshift_d

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
flatten_id = domain >>> nest_d

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
map_zoom_keys = coerce_maps >>> domain

-- | Zooms into and returns the values of maps. {_} / {:_}
map_zoom_values :: Dataset -> Dataset
map_zoom_values = coerce_maps >>> lshift_d

-- | Zooms into and returns the indices of array elements. [_:]
array_zoom_indices :: Dataset -> Dataset
array_zoom_indices = coerce_arrays >>> domain

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

-- | Projects a single statically known value. \foo\bar\baz\0\
static_project :: Data -> Dataset -> Dataset
static_project = project_d

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
