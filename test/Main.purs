module Test.Main where

import Prelude

import Effect (Effect)
import Test.Unit (test)
import Test.Unit.Main (runTest)
import Test.CSV (csvTests)

main :: Effect Unit
main = runTest do
  test "CSV " csvTests

