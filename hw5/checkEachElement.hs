import Test.HUnit

checkEachElement :: [a] -> (a -> Bool) -> Bool
checkEachElement [] condition = True
checkEachElement (first:list) condition | condition first = checkEachElement list condition
                                        | otherwise = False

-- testing
test1 = TestCase(assertEqual "1" True (checkEachElement [1, 2, 3] (>0)))
test2 = TestCase(assertEqual "2" False (checkEachElement [0, 1, 2, 3] (>0)))
test3 = TestCase(assertEqual "3" True (checkEachElement [2, 4, 6, 10] isEven))
  where isEven x | x `mod` 2 == 1 = False
                 | otherwise = True

tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
