module Main where
import Test.HUnit

-- Import the modules containing the grouped test cases
import Chapter2.StartingOutTest
import Chapter2.BabyFirstFunctionsTest

-- Run all the grouped tests
main :: IO Counts
main = do
    runTestTT Chapter2.StartingOutTest.tests
    runTestTT Chapter2.BabyFirstFunctionsTest.tests