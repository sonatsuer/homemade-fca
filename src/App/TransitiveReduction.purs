module App.TransitiveReduction (immediateSuccessors) where

import Prelude (class Semiring, map, not, zero, ($), (&&), (*), (+), (-), (<$>), (==))

import Control.Apply (lift2)
import Data.Array (catMaybes, foldl, mapWithIndex, transpose, zipWith, (..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

type Matrix a = Array (Array a)

multiply :: forall sr. Semiring sr => Matrix sr -> Matrix sr -> Matrix sr
multiply ass bss = mkRow <$> ass
  where
  bssTransposed = transpose bss
  mkRow as = mkCell as <$> bssTransposed
  mkCell as bs = foldl (+) zero $ zipWith (*) as bs

constructAdjacencyMatrix :: (Int -> Int -> Boolean) -> Int -> Matrix (Disj Boolean)
constructAdjacencyMatrix comparator size = mkRow <$> sizedRange
  where
  comparatorConj i j = Disj (comparator i j)
  sizedRange = 0 .. (size - 1)
  mkRow j = comparatorConj j <$> sizedRange

transitiveReductionFromAdjacencyMatrix :: Matrix (Disj Boolean) -> Map Int (Set Int)
transitiveReductionFromAdjacencyMatrix m =
  Map.fromFoldable $
    mapWithIndex (\i as -> Tuple i (mkSet as)) immediateSuccessorMatrix
  where
  mkSet as = Set.fromFoldable $ catMaybes $ mapWithIndex markTT as
  markTT i a = if a == Disj true then Just i else Nothing
  immediateSuccessorMatrix = deepZipWith (&&) m (negate $ multiply m m)
  negate = map (map (map not))
  deepZipWith op = zipWith (zipWith (lift2 op))

immediateSuccessors :: (Int -> Int -> Boolean) -> Int -> Map Int (Set Int)
immediateSuccessors comparator size =
  transitiveReductionFromAdjacencyMatrix (constructAdjacencyMatrix comparator size)
