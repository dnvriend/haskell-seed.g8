module Chapter2.StartingOutTest where
-- In the Haskell test module, import Test.HUnit
import Test.HUnit

-- Define test cases as appropriate
test1 = TestCase (assertEqual "2 + 15" 17 (2 + 15))
test2 = TestCase (assertEqual "49 + 100" 149 (49 + 100))
test3 = TestCase (assertEqual "1892 - 1472" 420 (1892 - 1472))
test4 = TestCase (assertEqual "5 / 2" 2.5 (5 / 2))

-- We can also use several operators on one line,
-- and all the usual precedence rules are obeyed.
-- We can use parentheses to make the precedence explicit or to change it.

test5 = TestCase (assertEqual "(50 * 100) - 4999" 1 ((50 * 100) - 4999))
test6 = TestCase (assertEqual "50 * 100 - 4999" 1 (50 * 100 - 4999))
test7 = TestCase (assertEqual "50 * (100 - 4999)" (-244950) (50 * (100 - 4999)))

-- Boolean algebra
test8 = TestCase (assertEqual "True && False" False (True && False))
test9 = TestCase (assertEqual "True && True" True (True && True))
test10 = TestCase (assertEqual "False || True" True (False || True))
test11 = TestCase (assertEqual "not False" True (not False))
test12 = TestCase (assertEqual "not (True && True)" False (not (True && True)))

-- Testing for equality

test13 = TestCase (assertEqual "5 == 5" True (5 == 5))
test14 = TestCase (assertEqual "1 == 0" False (1 == 0))
test15 = TestCase (assertEqual "5 /= 5" False (5 /= 5))
test16 = TestCase (assertEqual "5 /= 4" True (5 /= 4))
test17 = TestCase (assertEqual "'hello' == 'hello'" True ("hello" == "hello"))

-- More functions

test18 = TestCase (assertEqual "succ 8" 9 (succ 8))
test19 = TestCase (assertEqual "min 9 10" 9 (min 9 10))
test20 = TestCase (assertEqual "min 3.4 3.2" 3.2 (min 3.4 3.2))
test21 = TestCase (assertEqual "max 100 101" 101 (max 100 101))

-- Function application (calling a function by putting a space after it and then typing out the parameters)
-- has the highest precedence of them all.
-- What that means for us is that these two statements are equivalent.

test22 = TestCase (assertEqual "succ 9 + max 5 4 + 1" 16 (succ 9 + max 5 4 + 1))
test23 = TestCase (assertEqual "(succ 9) + (max 5 4) + 1" 16 ((succ 9) + (max 5 4) + 1))

-- Name the test cases and group them together
tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9,
    TestLabel "test10" test10,
    TestLabel "test11" test11,
    TestLabel "test12" test12,
    TestLabel "test13" test13,
    TestLabel "test14" test14,
    TestLabel "test15" test15,
    TestLabel "test16" test16,
    TestLabel "test17" test17,
    TestLabel "test18" test18,
    TestLabel "test19" test19,
    TestLabel "test20" test20,
    TestLabel "test21" test21,
    TestLabel "test21" test22,
    TestLabel "test21" test23 ]

-- to run the tests as a group, add it to `test/haskell/Main.hs`

