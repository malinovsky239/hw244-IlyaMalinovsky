import Test.HUnit
import Control.Monad.Cont

map_cont :: (a -> b) -> [a] -> Cont r [b]
map_cont _ [] = return []
map_cont f (first:list) = map_cont f list >>= return . (f first :) 

filter_cont :: (a -> Bool) -> [a] -> Cont r [a]
filter_cont _ [] = return []
filter_cont f (first:list) | f first = filter_cont f list >>= return . (first :)
                           | otherwise = filter_cont f list >>= return 

-- testing

test1 = TestCase(assertEqual "1" 9 (runCont (map_cont (+1) [1, 2, 3]) sum))
test2 = TestCase(assertEqual "2" "[2,4,6]" (runCont (map_cont (*2) [1, 2, 3]) show))
test3 = TestCase(assertEqual "3" "[3,4]" (runCont (filter_cont (> 2) [3, 2, 1, 4]) show))
test4 = TestCase(assertEqual "4" "[1,2]" (runCont (filter_cont (/= 0) [1, 0, 2, 0]) show))

tests = TestList [TestLabel "1" test1,
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4]
                  
runTesting = runTestTT tests
