findElement :: [Int] -> Int -> Int
findElement list element = foldr (func element) 0 list
  where func element x | (element == x) = (*0)
                       | otherwise = (+1)
