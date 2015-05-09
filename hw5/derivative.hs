import Test.HUnit

data Polynomial = Monomial Double Double | Sum Polynomial Polynomial

derivative :: Polynomial -> Polynomial
derivative (Monomial a b) = Monomial (a * b) (b - 1)
derivative (Sum l r) = Sum (derivative l) (derivative r)

showP :: Polynomial -> String
showP p = format $ helper p
  where 
    helper (Monomial a b) | a == 0 = ""                          
                          | b == 0 = showN a
                          | (b == 1 && a == 1) = "x"
                          | b == 1 = showN a ++ "*x"
                          | a == 1 = "x^" ++ showN b
                          | otherwise = showN a ++ "*x^" ++ showN b
      where 
        showN number | number < 0 = "(" ++ show number ++ ")"
                     | otherwise = show number                      
    helper (Sum l r) = helper l ++ "+" ++ helper r
    format "" = "0"
    format x = x    

-- testing
test1 = TestCase(assertEqual "1" "6.0*x" (showP $ derivative (Monomial 3 2)))
test2 = TestCase(assertEqual "2" "x^(-0.5)+9.0*x^3.5" (showP $ derivative (Sum (Monomial 2 0.5) (Monomial 2 4.5))))
test3 = TestCase(assertEqual "3" "1.0+2.0*x+3.0*x^2.0+4.0*x^3.0" 
	    (showP $ derivative (Sum (Monomial 1 1) (Sum (Monomial 1 2) (Sum (Monomial 1 3) (Monomial 1 4))))))
test4 = TestCase(assertEqual "4" "(-2.0)*x^(-3.0)" (showP $ derivative (Monomial 1 (-2))))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4]
                  
runTesting = runTestTT tests
