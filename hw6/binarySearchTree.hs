import Test.HUnit

data BinarySearchTree a = Empty | BST (BinarySearchTree a) a (BinarySearchTree a)

add :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
add x Empty = BST Empty x Empty	
add x (BST l root r) | root > x = BST (add x l) root r
                     | root == x = BST l root r                      
					 | otherwise = BST l root (add x r)		 

delete :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
delete x Empty = Empty
delete x (BST l root r) | root > x = BST (delete x l) root r                        
                        | root < x = BST l root (delete x r)
                        | root == x = merge l r

minElement :: BinarySearchTree a -> a
minElement (BST Empty root _) = root
minElement (BST l _ _) = minElement l

merge :: Ord a => BinarySearchTree a -> BinarySearchTree a -> BinarySearchTree a
merge Empty Empty = Empty
merge Empty r = r
merge l Empty = l
merge l r = BST l (minElement r) (delete (minElement r) r)                         

find :: Ord a => a -> BinarySearchTree a -> Bool
find _ Empty = False
find x (BST l root r) | root > x = find x l
                      | root == x = True
                      | root < x = find x r

size :: BinarySearchTree a -> Int
size Empty = 0
size (BST l root r) = size l + size r + 1

height :: BinarySearchTree a -> Int
height Empty = 0
height (BST l _ r) = (max (height l) (height r)) + 1  

-- testing

test1 = TestCase(assertEqual "1" 3 (height $ add 4 $ add 3 $ add 1 $ add 2 Empty))
test2 = TestCase(assertEqual "2" 3 (size $ delete 1 $ add 3 $ add 2 $ add 1 $ add 4 Empty))
test3 = TestCase(assertEqual "3" True (find 2 $ add 3 $ add 2 $ add 1 $ add 4 Empty))
test4 = TestCase(assertEqual "4" False (find 2 $ delete 2 $ add 3 $ add 2 $ add 1 $ add 4 Empty))
test5 = TestCase(assertEqual "5" True (find 2 $ add 2 $ add 3 $ delete 2 $ 
	                                   delete 1 $ add 2 $ add 1 $ add 4 Empty))
tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4,
                  TestLabel "5" test5]
                  
runTesting = runTestTT tests
