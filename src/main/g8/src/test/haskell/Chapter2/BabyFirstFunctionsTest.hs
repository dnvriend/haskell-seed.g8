module Chapter2.BabyFirstFunctionsTest where
-- In the Haskell test module, import Test.HUnit
import Test.HUnit
-- Import the module that contain the functions to test
import Chapter2.BabyFirstFunctions

-- Group tests together in 'Test' modules
-- like this one called 'CalculationsTest'
test1 = TestCase (assertEqual "doubleMe" 6 (doubleMe 3))

-- Name the test cases and group them together
tests = TestList [TestLabel "test1" test1]

-- to run the tests as a group, add it to `test/haskell/Main.hs`