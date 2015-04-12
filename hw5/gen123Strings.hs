import Test.HUnit

gen123Strings :: Int -> [String]
gen123Strings 0 = [""]
gen123Strings n = (map ('1':) $ gen123Strings (n - 1)) 
               ++ (map ('2':) $ gen123Strings (n - 1)) 
               ++ (map ('3':) $ gen123Strings (n - 1))
               
-- testing
test1 = TestCase(assertEqual "1" [""] (gen123Strings 0))
test2 = TestCase(assertEqual "2" ["1", "2", "3"] (gen123Strings 1))
test3 = TestCase(assertEqual "3" 
	             ["11", "12", "13", "21", "22", "23", "31", "32", "33"] 
	             (gen123Strings 2))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
