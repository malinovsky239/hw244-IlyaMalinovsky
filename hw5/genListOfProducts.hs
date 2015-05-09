import Test.HUnit

genListOfProducts :: Int -> [Int]
genListOfProducts n = [1..n] >>= \x -> map (*x) [1..n]

-- testing
test1 = TestCase(assertEqual "1" [1] (genListOfProducts 1))
test2 = TestCase(assertEqual "2" [1, 2, 2, 4] (genListOfProducts 2))
test3 = TestCase(assertEqual "3" 
	             [1, 2, 3, 2, 4, 6, 3, 6, 9] 
	             (genListOfProducts 3))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
