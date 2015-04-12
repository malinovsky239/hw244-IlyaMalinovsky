import Test.HUnit

genDecompositions :: Int -> [[Int]]
genDecompositions num = helper num 1
  where 
    helper 0 minItem = [[]]
    helper x minItem | minItem > x = []
                     | otherwise = map (minItem:) (helper (x - minItem) minItem) ++ helper x (minItem + 1)

-- testing
test1 = TestCase(assertEqual "1" [[1]] (genDecompositions 1))
test2 = TestCase(assertEqual "2" [[1, 1], [2]] (genDecompositions 2))
test3 = TestCase(assertEqual "3" [[1, 1, 1], [1, 2], [3]] (genDecompositions 3))
test4 = TestCase(assertEqual "4" [[1, 1, 1, 1], [1, 1, 2], [1, 3], [2, 2], [4]] (genDecompositions 4))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test3]
                  
runTesting = runTestTT tests
