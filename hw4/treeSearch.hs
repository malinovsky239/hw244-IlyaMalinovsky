import Test.HUnit

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

treeSearch :: Tree a -> (a -> Bool) -> Maybe a
treeSearch (Leaf v) f | f v = Just v
                      | otherwise = Nothing
treeSearch (Branch a b c) f | f b = Just b
                            | otherwise = combine (treeSearch a f) (treeSearch c f)
                                            where
                                              combine :: Maybe a -> Maybe a -> Maybe a
                                              combine Nothing Nothing = Nothing
                                              combine (Just x) _ = Just x
                                              combine _ (Just x) = Just x

-- testing

tree = Branch 
         (Branch 
           (Leaf 2) 6 (Leaf 3)
                              ) 
         1 
         (Branch 
           (Leaf 4) 7 (Leaf(6))
            	                 )

test1 = TestCase(assertEqual "1" Nothing (treeSearch tree (>10)))
test2 = TestCase(assertEqual "2" (Just 6) (treeSearch tree (divisibleBy 3)))
  where
    divisibleBy divisor x = x `mod` divisor == 0
test3 = TestCase(assertEqual "3" (Just 2) (treeSearch tree (inRange 2 5)))
  where
    inRange l r x = (l <= x) && (x <= r)

tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
