module Test.CSV (csvTests) where

import Prelude

import App.CSV (CSV, parseCSV)
import Data.Either (Either(..))
import Data.Array.NonEmpty (cons')
import Test.Unit (Test)
import Test.Unit.Assert (equal')

csvTests :: Test
csvTests = do
  compareCSV "Simple CSV" "ab,cd,uu\nss,,\nuu,ss,vv" simpleSample
  compareCSV "Quoted CSV" "\"a,b\",cd,uu\n\"s \"\"hhs\",,\nuu , ss, vv " quotedSample

compareCSV :: String -> String -> CSV -> Test
compareCSV name raw expected = equal' msg result wrappedExpected
  where
  result = parseCSV raw
  wrappedExpected = Right expected
  msg = name <> "\nExpected : " <> show wrappedExpected
    <> "\nGot      : "
    <> show result

simpleSample :: CSV
simpleSample = r1 `cons'` [ r2, r3 ]
  where
  r1 = "ab" `cons'` [ "cd", "uu" ]
  r2 = "ss" `cons'` [ "", "" ]
  r3 = "uu" `cons'` [ "ss", "vv" ]

quotedSample :: CSV
quotedSample = r1 `cons'` [ r2, r3 ]
  where
  r1 = "a,b" `cons'` [ "cd", "uu" ]
  r2 = "s \"hhs" `cons'` [ "", "" ]
  r3 = "uu " `cons'` [ " ss", " vv " ]

