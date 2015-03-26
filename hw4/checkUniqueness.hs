import Test.HUnit

checkUniqueness :: (Eq a) => [a] -> Bool
checkUniqueness [] = True
checkUniqueness (x:list) = (length (filter (== x) list) == 0) && checkUniqueness list

-- testing
test1 = TestCase(assertEqual "1" True (checkUniqueness "abcdefgh "))
test2 = TestCase(assertEqual "2" True (checkUniqueness [[1], [2], [3, 3]]))
test3 = TestCase(assertEqual "3" False (checkUniqueness [[1, 2],[0],[1, 2]]))
test4 = TestCase(assertEqual "4" False (checkUniqueness [6, 1, 2, 3, 1]))
test5 = TestCase(assertEqual "5" False (checkUniqueness "caab"))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4, 
                  TestLabel "5" test5]

runTesting = runTestTT tests
