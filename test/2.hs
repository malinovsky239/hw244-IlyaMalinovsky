data Tree a = Leaf a | Branch (Tree a) a (Tree a)

getNegative :: Tree Integer -> [Integer]
getNegative t = helper [] t
  where
    helper list (Leaf x) | x < 0 = x:list
                         | otherwise = list
    helper list (Branch a b c) | b < 0 = b : helper (helper list a) c
                               | otherwise = helper (helper list a) c
                              