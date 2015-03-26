import Test.HUnit

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

maxDepth :: Tree a -> Int
maxDepth (Leaf x) = 0
maxDepth (Branch left v right) = max (maxDepth left) (maxDepth right) + 1

minDepth :: Tree a -> Int
minDepth (Leaf x) = 0
minDepth (Branch left v right) = min (minDepth left) (minDepth right) + 1

-- testing
tree1 = Branch 
          (Branch 
            (Leaf 2) 6 (Leaf 3)
                               ) 
          1 
          (Branch 
            (Leaf 4) 7 (Leaf(6))
            	               )

test1 = TestCase(assertEqual "1" 2 (maxDepth tree1))
test2 = TestCase(assertEqual "2" 2 (minDepth tree1))

tree2 = Branch 
          (Branch 
            (Branch 
              (Leaf 7) 0 (Leaf 2)
            )              
            6 
            (Leaf 3)
          )                                           
          1 
          (Leaf 4)         

test3 = TestCase(assertEqual "3" 3 (maxDepth tree2))
test4 = TestCase(assertEqual "4" 1 (minDepth tree2))


tests = TestList[TestLabel "1" test1,
                 TestLabel "2" test2,
                 TestLabel "3" test3,
                 TestLabel "4" test4]

runTesting = runTestTT tests                 