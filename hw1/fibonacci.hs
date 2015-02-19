fibonacci x = (if x <= 1 
                then x 
                else fibonacci(x - 2) + fibonacci(x - 1))
main = do
    putStrLn "Type some x >= 0 : "
    input <- getLine
    putStr ("Fib(" ++ input ++ ") = ")
    print $ fibonacci (read input :: Int)