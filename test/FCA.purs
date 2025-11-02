module Test.FCA (fcaTests) where

import Prelude

import App.CSV (CSV)
import App.FCA (AttributeKey(..), computeAllIntents, generateFormalContext)
import Data.Array.NonEmpty (NonEmptyArray, cons')
import Data.Either (Either(..))
import Test.Unit (Test)
import Test.Unit.Assert (equal')
import Data.Set (Set)
import Data.Set as Set

fcaTests :: Test
fcaTests = do
  compareIntents "Duplicate attribute" duplicateAttribute (Left "Duplicate attribute name")
  compareIntents "Duplicate object" duplicateObject (Left "Duplicate object name")
  compareIntents "Duplicate attribute" heterogenousRows (Left "Found rows of different sizes")
  compareIntents "Diagonal context" diagonalCSV (Right diagonalIntents)
  compareIntents "Triangles context" trianglesCSV (Right trianglesIntents)

compareIntents :: String -> CSV -> Either String (NonEmptyArray (Set AttributeKey)) -> Test
compareIntents name csv expected = equal' msg result expected
  where
  result = computeAllIntents <$> generateFormalContext csv
  msg = name <> "\nExpected :" <> show expected
    <> "\nGot      :"
    <> show result

duplicateAttribute :: CSV
duplicateAttribute = "qqq" `cons'` [ "a", "b", "a" ]
  `cons'`
    [ "x" `cons'` [ "", ".", "." ]
    , "y" `cons'` [ ".", "", "" ]
    , "z" `cons'` [ ".", "", "." ]
    ]

duplicateObject :: CSV
duplicateObject = "qqq" `cons'` [ "a", "b", "a" ]
  `cons'`
    [ "x" `cons'` [ "", ".", "." ]
    , "x" `cons'` [ ".", "", "" ]
    , "z" `cons'` [ ".", "", "." ]
    ]

heterogenousRows :: CSV
heterogenousRows = "qqq" `cons'` [ "a", "b", "a" ]
  `cons'`
    [ "x" `cons'` [ "", ".", "." ]
    , "y" `cons'` [ ".", "", "", "" ]
    , "z" `cons'` [ ".", "", "." ]
    ]

diagonalCSV :: CSV
diagonalCSV = "qqq" `cons'` [ "a", "b", "c" ]
  `cons'`
    [ "x" `cons'` [ ".", "", "" ]
    , "y" `cons'` [ "", ".", "" ]
    , "z" `cons'` [ "", "", "." ]
    ]

diagonalIntents :: NonEmptyArray (Set AttributeKey)
diagonalIntents =
  Set.fromFoldable [ AttributeKey 0, AttributeKey 1, AttributeKey 2 ]
    `cons'`
      [ Set.fromFoldable [ AttributeKey 0 ]
      , Set.fromFoldable [ AttributeKey 1 ]
      , Set.fromFoldable [ AttributeKey 2 ]
      , Set.fromFoldable []
      ]

trianglesCSV :: CSV
trianglesCSV = "qqq" `cons'` [ "equilateral", "isosceles", "acute", "obtuse", "right" ]
  `cons'`
    [ "T1" `cons'` [ "", "X", "", "X", "" ]
    , "T2" `cons'` [ "", "X", "", "", "X" ]
    , "T3" `cons'` [ "", "", "X", "", "" ]
    , "T4" `cons'` [ "X", "X", "X", "", "" ]
    , "T5" `cons'` [ "", "", "", "X", "" ]
    , "T6" `cons'` [ "", "X", "X", "", "" ]
    , "T7" `cons'` [ "", "", "", "", "X" ]
    ]

trianglesIntents :: NonEmptyArray (Set AttributeKey)
trianglesIntents =
  Set.fromFoldable [ AttributeKey 0, AttributeKey 1, AttributeKey 2, AttributeKey 3, AttributeKey 4 ]
    `cons'`
      [ Set.fromFoldable [ AttributeKey 0, AttributeKey 1, AttributeKey 2 ]
      , Set.fromFoldable [ AttributeKey 1, AttributeKey 2 ]
      , Set.fromFoldable [ AttributeKey 1, AttributeKey 3 ]
      , Set.fromFoldable [ AttributeKey 1, AttributeKey 4 ]
      , Set.fromFoldable [ AttributeKey 1 ]
      , Set.fromFoldable [ AttributeKey 2 ]
      , Set.fromFoldable [ AttributeKey 3 ]
      , Set.fromFoldable [ AttributeKey 4 ]
      , Set.fromFoldable []
      ]
