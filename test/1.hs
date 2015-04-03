primes :: [Int]
primes = 2 : (map nextPrime primes)

nextPrime :: Int -> Int
nextPrime x | isPrime (x + 1) 2 = x + 1
            | otherwise = nextPrime (x + 1)

isPrime :: Int -> Int -> Bool
isPrime x divisor | x == divisor = True
                  | x `mod` divisor == 0 = False
                  | otherwise = isPrime x (divisor + 1)               