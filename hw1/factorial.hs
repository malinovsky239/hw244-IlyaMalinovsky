factorial x = (if x == 0 
                then 1 
                else x * factorial(x - 1))
main = do
    putStrLn "Type some x >= 0 : "
    input <- getLine
    putStr (input ++ "! = ")
    print $ factorial (read input :: Int)