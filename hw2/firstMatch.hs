-- if a given number is not contained in the list 
-- function returns the size of that list
firstMatch :: [Int] -> Int -> Int
firstMatch [] elem = 0
firstMatch (f:list) elem = if f == elem
                           then 0  
                           else firstMatch list elem + 1
