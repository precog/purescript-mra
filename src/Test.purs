module Main where
  import Prelude(Unit, class Show, class Eq, ($), (==), (/=), (++), bind, show)

  import Data.Tuple(Tuple(..))
  -- import Data.List(List(), fromFoldable)

  import Control.Monad.Eff(Eff)
  import Control.Monad.Eff.Console(CONSOLE, log)

  import MRA.Provenance(Provenance(..), (/\), (\/), (>>))
  import MRA.Data(makeMap, primString, primInt)
  import MRA.Core(Dataset(), literal_d, project_d)

  type TestResult = forall r. Eff (console :: CONSOLE | r) Unit

  assertEqual :: forall a r. (Show a, Eq a) => a -> a -> Eff (console :: CONSOLE | r) Unit
  assertEqual l r =
    if l == r then log $ "Pass: " ++ show l ++ " == " ++ show r
    else log $ "FAIL: Expected " ++ show l ++ " but found " ++ show r

  assertNotEqual :: forall a r. (Show a, Eq a) => a -> a -> Eff (console :: CONSOLE | r) Unit
  assertNotEqual l r =
    if l /= r then log $ "Pass: " ++ show l ++ " /= " ++ show r
    else log $ "FAIL: " ++ show l ++ " == " ++ show r

  entuple :: forall a b. a -> b -> Tuple a b
  entuple = Tuple

  infix 6 entuple as ~

  universe :: Dataset
  universe = literal_d $ makeMap [
    primString "olympics" ~ makeMap [
      primInt 1 ~ makeMap [primString "year" ~ primInt 1924, primString "city" ~ primString "Boulder", primString "sport" ~ primString "Skating", primString "country" ~ primString "USA" ],
      primInt 2 ~ makeMap [primString "year" ~ primInt 1926, primString "city" ~ primString "Boulder", primString "sport" ~ primString "Bodybuilding", primString "country" ~ primString "USA"],
      primInt 3 ~ makeMap [primString "year" ~ primInt 1928, primString "city" ~ primString "Billings", primString "sport" ~ primString "Skating", primString "country" ~ primString "USA", primString "gender" ~ primString "M"]
    ] ]

  olympics :: Dataset
  olympics = project_d (primString "olympics") universe

  testProvenance :: TestResult
  testProvenance =
    let
      projPrimInt4 = Proj (primInt 4)
      projPrimInt3 = Proj (primInt 3)
      projPrimInt2 = Proj (primInt 2)
    in do
      assertEqual (Value /\ Value) Value
      assertEqual Value (Value \/ Value)

      assertEqual (Nada \/ Value) Value
      assertEqual Value (Nada \/ Value)

      -- primitive equality
      assertEqual projPrimInt4 projPrimInt4
      assertNotEqual projPrimInt4 projPrimInt3

      -- associativity of sums
      assertEqual (projPrimInt4 \/ projPrimInt3) (projPrimInt3 \/ projPrimInt4)

      -- associativity of products
      assertEqual (projPrimInt4 /\ projPrimInt3) (projPrimInt3 /\ projPrimInt4)

      -- non-associativity of seqs
      assertNotEqual (projPrimInt4 >> projPrimInt3) (projPrimInt3 >> projPrimInt4)

      -- distributivity of sums through products
      assertEqual (projPrimInt4 /\ (projPrimInt3 \/ projPrimInt2)) ((projPrimInt4 /\ projPrimInt3) \/ (projPrimInt4 /\ projPrimInt2))

      -- distributivity of sums through seqs
      assertEqual (projPrimInt4 >> (projPrimInt3 \/ projPrimInt2)) (projPrimInt4 >> projPrimInt3 \/ projPrimInt4 >> projPrimInt2)

  testJoinKeys :: TestResult
  testJoinKeys = do
    log "Dummy"

  main :: forall r. Eff (console :: CONSOLE | r) Unit
  main = do
    testProvenance
    testJoinKeys
