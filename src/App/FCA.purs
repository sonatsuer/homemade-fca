module App.FCA
  ( AttributeKey(..)
  , ObjectKey
  , computeAllIntents
  , generateFormalContext
  , computeAllConcepts
  , Concept(..)
  , FormalContext
  ) where

import Prelude

import App.CSV (CSV)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (filter, mapWithIndex, nub, null, sortBy, transpose, uncons, (..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)

{- Based on

Ganter, B. (2010). Two Basic Algorithms in Concept Analysis. In: Kwuida, L., Sertkaya, B. (eds)
Formal Concept Analysis. ICFCA 2010. Lecture Notes in Computer Science(), vol 5986. Springer,
Berlin, Heidelberg. https://doi.org/10.1007/978-3-642-11928-6_22

-}

newtype ObjectKey = ObjectKey Int

derive newtype instance eqObjectKey :: Eq ObjectKey
derive newtype instance ordObjectKey :: Ord ObjectKey
instance showObjectKey :: Show ObjectKey where
  show (ObjectKey n) = "ObjectKey " <> show n

newtype AttributeKey = AttributeKey Int

derive newtype instance eqAttributeKey :: Eq AttributeKey
derive newtype instance ordAttributeKey :: Ord AttributeKey
instance showAttributeKey :: Show AttributeKey where
  show (AttributeKey n) = "AttributeKey " <> show n

type FormalContext =
  { objects :: Map ObjectKey String
  , attributes :: Map AttributeKey String
  , attributeExtent :: Map AttributeKey (Set ObjectKey)
  , objectIntent :: Map ObjectKey (Set AttributeKey)
  }

type FCA = Reader FormalContext

mkKeyed :: forall k a. Ord k => Ord a => (Int -> k) -> Array a -> Map k a
mkKeyed keyMaker = Map.fromFoldable <<< mapWithIndex (\i a -> Tuple (keyMaker i) a)

mkUniqueMap :: forall k a. Ord k => Ord a => (Int -> k) -> Array a -> Maybe (Map k a)
mkUniqueMap keyMaker as =
  if nub as == as then Just (mkKeyed keyMaker as)
  else Nothing

extractSelected :: forall k. Ord k => (Int -> k) -> Array String -> Set k
extractSelected keyMaker =
  mapWithIndex (\i s -> Tuple (keyMaker i) s)
    >>>
      filter (\tp -> snd tp /= "")
    >>>
      map fst
    >>>
      Set.fromFoldable

generateFormalContext :: CSV -> Either String FormalContext
generateFormalContext initialCSV = do
  let csv = postProcessCSV initialCSV
  let differentRowSizes = NE.length $ NE.nub $ map NE.length csv
  when (differentRowSizes /= 1) (Left $ "Found rows of different sizes" <> show csv)
  objects <- note "Duplicate object name" $ mkUniqueMap ObjectKey $ NE.tail $ map NE.head csv
  attributes <- note "Duplicate attribute name" $ mkUniqueMap AttributeKey $ NE.tail $ NE.head csv
  let matrix = NE.tail $ map NE.tail csv
  let attributeExtent = mkKeyed AttributeKey $ map (extractSelected ObjectKey) $ transpose matrix
  let objectIntent = mkKeyed ObjectKey $ map (extractSelected AttributeKey) matrix
  Right { objects, attributes, attributeExtent, objectIntent }

postProcessCSV :: CSV -> CSV
postProcessCSV csv = NE.head csv `NE.cons'` (removeRedundant $ NE.tail csv)
  where
  removeRedundant = filter ((/=) (NE.cons' "" []))

intersectAll :: forall a b. Ord a => Ord b => Set b -> Map a (Set b) -> Set a -> Set b
intersectAll top m = Set.mapMaybe (flip Map.lookup m) >>> foldl Set.intersection top

computeIntent :: Set ObjectKey -> FCA (Set AttributeKey)
computeIntent os = do
  atr <- asks _.attributes
  objInt <- asks _.objectIntent
  pure $ intersectAll (Map.keys atr) objInt os

computeExtent :: Set AttributeKey -> FCA (Set ObjectKey)
computeExtent as = do
  obj <- asks _.objects
  atrExt <- asks _.attributeExtent
  pure $ intersectAll (Map.keys obj) atrExt as

intentClosure :: Set AttributeKey -> FCA (Set AttributeKey)
intentClosure = computeExtent >=> computeIntent

oPlus :: Set AttributeKey -> AttributeKey -> FCA (Set AttributeKey)
oPlus as a = intentClosure plus
  where
  below (AttributeKey k) = Set.fromFoldable $ map AttributeKey (0 .. (k - 1))
  plus = Set.singleton a `Set.union` (as `Set.intersection` below a)

lecticLessThan :: forall a. Ord a => a -> Set a -> Set a -> Boolean
lecticLessThan ref set1 set2 =
  case Set.findMin $ (set1 `Set.union` set2) `Set.difference` (set1 `Set.intersection` set2) of
    Nothing -> false
    Just x -> x == ref && x `Set.member` set2

stepIntentStep
  :: Set AttributeKey
  -> Array AttributeKey
  -> FCA (Step (Array AttributeKey) (Set AttributeKey))
stepIntentStep candidate additions = do
  case uncons additions of
    Nothing ->
      pure $ Done candidate
    Just { head, tail } -> do
      extended <- candidate `oPlus` head
      if lecticLessThan head candidate extended then pure $ Done extended
      else pure $ Loop tail

computeNextIntent :: Set AttributeKey -> FCA (Maybe (Set AttributeKey))
computeNextIntent as = do
  allAttributes <- asks $ _.attributes >>> Map.keys
  let possibleAdditions = sortBy (flip compare) $ Set.toUnfoldable (allAttributes `Set.difference` as)
  if null possibleAdditions then pure Nothing
  else Just <$> tailRecM (stepIntentStep as) possibleAdditions

stepAllIntents
  :: NonEmptyArray (Set AttributeKey)
  -> FCA (Step (NonEmptyArray (Set AttributeKey)) (NonEmptyArray (Set AttributeKey)))
stepAllIntents collected = do
  let latest = NE.head collected
  mbNextIntent <- computeNextIntent latest
  pure $
    case mbNextIntent of
      Nothing ->
        Done collected
      Just nextIntent ->
        Loop $ nextIntent `NE.cons` collected

computeAllIntents :: FormalContext -> (NonEmptyArray (Set AttributeKey))
computeAllIntents ctx = flip runReader ctx do
  smallestIntent <- intentClosure Set.empty
  tailRecM stepAllIntents (NE.singleton smallestIntent)

type Concept = { intent :: Set String, extent :: Set String }

computeAllConcepts :: FormalContext -> (NonEmptyArray Concept)
computeAllConcepts ctx = map intoContext (computeAllIntents ctx)
  where
  attributeTranslator attributeKey = fromMaybe "<missing>" $ Map.lookup attributeKey ctx.attributes
  objectTranslator objectKey = fromMaybe "<missing>" $ Map.lookup objectKey ctx.objects
  intoContext intent =
    { intent: Set.map attributeTranslator intent
    , extent: Set.map objectTranslator (runReader (computeExtent intent) ctx)
    }