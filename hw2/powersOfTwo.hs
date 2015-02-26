f :: [Int] -> Int -> [Int]
f a b = b:a 

myReverse :: [Int] -> [Int]
myReverse list = foldl f [] list

generate :: Int -> Int -> [Int]
generate n power = if n == 0
                   then []
                   else power : generate (n - 1) (power * 2)

powersOfTwo :: Int -> [Int]
powersOfTwo n = generate n 1
