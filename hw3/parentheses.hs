import Test.HUnit

isCorrect :: String -> Bool
isCorrect s = isCorrect' s []
  where
    isCorrect' "" [] = True
    isCorrect' "" _ = False
    isCorrect' (f:lst) [] = isCorrect' lst [f]
    isCorrect' (f:lst) (top:stack) | f == rotate top = isCorrect' lst stack
                                   | otherwise = isCorrect' lst (f:top:stack)
      where
        rotate '(' = ')'
        rotate '[' = ']'
        rotate '{' = '}'
        rotate x = x                             

-- testing
test1 = TestCase(assertEqual "1" True (isCorrect "([{}])"))
test2 = TestCase(assertEqual "2" True (isCorrect "[({}[])]"))
test3 = TestCase(assertEqual "3" False (isCorrect "[({}][])"))
test4 = TestCase(assertEqual "4" False (isCorrect "([[[]])]"))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4]
                  
runTesting = runTestTT tests
 