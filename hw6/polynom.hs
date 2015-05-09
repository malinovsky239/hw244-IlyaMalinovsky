import qualified Prelude as P
import Test.HUnit

data Polynomial = Monomial P.Double | Sum P.Double Polynomial

show :: Polynomial -> P.String
show p = format P.$ helper p 0
  where 
    helper (Monomial x) 0 = P.show x
    helper (Monomial x) 1 | x P.== 0 = ""
                          | P.otherwise = "+" P.++ P.show x P.++ "*x"
    helper (Monomial x) power | x P.== 0 = ""
                              | x P.== 1 = "+x^" P.++ P.show power
                              | P.otherwise = "+" P.++ P.show x P.++ "*x^" P.++ (P.show power)
    helper (Sum x p) power | x P.== 0 = helper p (power P.+ 1)
                           | P.otherwise = helper (Monomial x) power P.++ helper p (power P.+ 1)                          
    format "" = "0"
    format ('+':str) = str
    format x = x                       

(+) :: Polynomial -> Polynomial -> Polynomial
(Sum x p1) + (Sum y p2) = Sum (x P.+ y) (p1 + p2)
(Monomial x) + (Sum y p) = Sum (x P.+ y) p
(Sum x p) + (Monomial y) = Sum (x P.+ y) p
(Monomial x) + (Monomial y) = Monomial (x P.+ y)

(*) :: Polynomial -> Polynomial -> Polynomial
p * (Sum y p2) = p * (Monomial y) + Sum 0 (p * p2)
(Sum x p) * (Monomial y) = Sum (x P.* y) (p * (Monomial y))
(Monomial x) * (Monomial y) = Monomial (x P.* y)

-- testing
test1 = TestCase(assertEqual "1" 
	                   "1.0+2.0*x+3.0*x^2" 
	                   (show ((Sum 1 (Sum 0 (Monomial 3))) + (Sum 0 (Monomial 2)))))
test2 = TestCase(assertEqual "2" 
	                   "1.0+2.0*x^2" 
	                   (show ((Sum 0 (Sum 0 (Monomial 2))) + (Monomial 1))))
test3 = TestCase(assertEqual "3" 
	                   "2.0*x^2" 
	                   (show ((Sum 0 (Sum 0 (Monomial 2))) * (Monomial 1))))
test4 = TestCase(assertEqual "4" 
	                   "2.0*x+6.0*x^3" 
	                   (show ((Sum 1 (Sum 0 (Monomial 3))) * (Sum 0 (Monomial 2)))))
test5 = TestCase(assertEqual "5" 
	                   "2.0+3.0*x+x^2" 
	                   (show ((Sum 1 (Monomial 1)) * (Sum 2 (Monomial 1)))))
tests = TestList [TestLabel "1" test1,
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4,
                  TestLabel "5" test5]                                  
runTesting = runTestTT tests
