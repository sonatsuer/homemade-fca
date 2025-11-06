module App.Mermaid (generateMermaid) where

import Data.Array (concatMap, foldMap, intercalate)
import Prelude (bind, map, show, ($), (<$>), (<>), (>>>))

import App.CSV (parseCSV, CSV)
import App.FCA (Concept, computeAllConcepts, generateFormalContext)
import App.TransitiveReduction (immediateSuccessors)
import Control.Apply (lift2)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

generateMermaid :: String -> Either String String
generateMermaid raw = do
  csv <- parseCSV raw
  ctx <- generateFormalContext csv
  let concepts = computeAllConcepts ctx
  let encoding = encode concepts
  let successorMap = immediateSuccessors encoding.comparator encoding.size
  Right $ convertToMermaid encoding.mapping successorMap

encode
  :: NonEmptyArray Concept
  -> { comparator :: Int -> Int -> Boolean
     , size :: Int
     , mapping :: Map Int Concept
     }
encode concepts = { comparator, size, mapping }
  where
  mapping = Map.fromFoldable $ NE.mapWithIndex Tuple concepts
  size = NE.length concepts
  getConcept i = map _.intent (Map.lookup i mapping)
  comparator i j =
    fromMaybe false $
      lift2 Set.properSubset (getConcept i) (getConcept j)

convertToMermaid :: Map Int Concept -> Map Int (Set Int) -> String
convertToMermaid conceptMap successorMap = "graph BT\n" <> definitions <> connections
  where
  -- Shared
  flatten :: forall k v. Map k v -> Array (Tuple k v)
  flatten = Map.toUnfoldableUnordered

  mkName :: Int -> String
  mkName i = "Concept_" <> show i

  indentedBlock :: Array String -> String
  indentedBlock = foldMap (\str -> "  " <> str <> "\n")

  -- Definitions
  commaSeparated = Set.toUnfoldable >>> intercalate ", "
  mkNamedConcept (Tuple i c) =
    mkName i
      <> "[\"\\{"
      <> commaSeparated c.intent
      <> "\\}<br/>-----<br/>\\{"
      <> commaSeparated c.extent
      <> "\\}\"]"
  definitions = indentedBlock $ map mkNamedConcept (flatten conceptMap)

  -- Connections
  arrow i j = mkName i <> " --> " <> mkName j
  mkConnection (Tuple i js) = arrow i <$> Set.toUnfoldable js
  connections = indentedBlock $ concatMap mkConnection (flatten successorMap)
