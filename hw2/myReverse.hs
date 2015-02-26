f :: [Int] -> Int -> [Int]
f a b = b:a 

myReverse :: [Int] -> [Int]
myReverse list = foldl f [] list 