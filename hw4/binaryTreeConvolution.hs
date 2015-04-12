import Test.HUnit

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

binaryTreeConvolution :: (a -> b -> b) -> b -> Tree a -> b
binaryTreeConvolution f accum (Leaf v) = f v accum
binaryTreeConvolution f accum (Branch a b c) = 
	f b (binaryTreeConvolution f (binaryTreeConvolution f accum a) c)

-- testing

tree = Branch 
         (Branch 
           (Leaf 2) 6 (Leaf 3)
                              ) 
         1 
         (Branch 
           (Leaf 4) 7 (Leaf(6))
            	               )

test1 = TestCase(assertEqual "1" [1, 7, 6, 4, 6, 3, 2] (binaryTreeConvolution (:) [] tree))
test2 = TestCase(assertEqual "2" 29 (binaryTreeConvolution (+) 0 tree))
test3 = TestCase(assertEqual "3" 7 (binaryTreeConvolution (max) 0 tree))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
