module Test.CSV (csvTests) where

import Prelude

import App.CSV (CSV, parseCSV)
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.List.NonEmpty (cons')
import Data.List.Types (List, NonEmptyList)
import Test.Unit (Test)
import Test.Unit.Assert (equal')


csvTests :: Test
csvTests = do
  compareCSV "Simple CSV" "ab,cd,uu\nss,,\nuu,ss,vv" simpleSample
  compareCSV "Quoted CSV" "\"a,b\",cd,uu\n\"s \"\"hhs\",,\nuu , ss, vv " quotedSample

simpleSample :: CSV
simpleSample = mkNonempty r1 $ fromFoldable [r2, r3]
  where
    r1 = mkNonempty "ab" $ fromFoldable ["cd", "uu"]
    r2 = mkNonempty "ss" $ fromFoldable ["", ""]
    r3 = mkNonempty "uu" $ fromFoldable ["ss", "vv"]

quotedSample :: CSV
quotedSample = mkNonempty r1 $ fromFoldable [r2, r3]
  where
    r1 = mkNonempty "a,b" $ fromFoldable ["cd", "uu"]
    r2 = mkNonempty "s \"hhs" $ fromFoldable ["", ""]
    r3 = mkNonempty "uu " $ fromFoldable [" ss", " vv "]


mkNonempty :: forall a. a -> List a -> NonEmptyList a
mkNonempty a as = a `cons'` as

compareCSV :: String -> String -> CSV -> Test
compareCSV name raw expected = equal' msg result wrappedExpected
  where
    result = parseCSV raw
    wrappedExpected = Right expected
    msg = name <> "\nExpected: " <> show wrappedExpected <> "\n Got: " <> show result
