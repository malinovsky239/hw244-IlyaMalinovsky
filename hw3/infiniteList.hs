next :: Int -> Int
next 0 = 1
next 1 = 7
next 7 = 9
next x = if x `mod` 10 == 9
           then next(x `div` 10) * 10 + 1
           else x `div` 10 * 10 + next (x `mod` 10)

infiniteList = 1 : (map next infiniteList)