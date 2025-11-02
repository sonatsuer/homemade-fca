module Test.TransitiveReduction where

import Prelude

import App.TransitiveReduction (immediateSuccessors)
import Data.Array ((..), concatMap, all)
import Data.Int (pow)
import Data.Int.Bits ((.|.))
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Test.Unit (Test)
import Test.Unit.Assert (equal')

transitiveReductionTests :: Test
transitiveReductionTests = do
  compareSuccessorMap "Linear" (<) 5 (linear 5)
  compareSuccessorMap "Powerset" (powerset 5).comparator (powerset 5).size (powersetSuccessors 5)

type SuccessorMap = Map Int (Set Int)

compareSuccessorMap :: String -> (Int -> Int -> Boolean) -> Int -> SuccessorMap -> Test
compareSuccessorMap name comparator size expected = equal' msg result expected
  where
  result = immediateSuccessors comparator size
  msg = name <> "\nExpected : " <> show expected
    <> "\nGot      : "
    <> show result

linear :: Int -> SuccessorMap
linear n = Map.fromFoldable $
  map
    (\i -> Tuple i (if i < n - 1 then Set.singleton (i + 1) else Set.empty))
    (0 .. (n - 1))

powerset :: Int -> { comparator :: Int -> Int -> Boolean, size :: Int }
powerset size = { comparator, size: pow 2 size }
  where
  comparator x y = x .|. y == y && x /= y

powersetSuccessors :: Int -> SuccessorMap
powersetSuccessors size = Map.fromFoldable $ map getSuccessors sizedRange
  where
  pset = powerset size
  comp = pset.comparator
  sizedRange = 0 .. (pset.size - 1)
  getSuccessors i = Tuple i (Set.fromFoldable $ concatMap (bruteForce i) sizedRange)
  bruteForce i j = if comp i j && all (notBetween i j) sizedRange then [ j ] else []
  notBetween i j x = not (comp i x && comp x j)
