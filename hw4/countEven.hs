import Test.QuickCheck

countEven :: [Int] -> Int
countEven list = length (filter isEven list)
  where isEven x = x `mod` 2 == 0

countEven' :: [Int] -> Int
countEven' list = length list - sum (map (`mod` 2) list)

countEven'' :: [Int] -> Int
countEven'' list = length list - (foldr (add_rem2) 0 list)
  where add_rem2 a b = a `mod` 2 + b

-- testing

checkCountEven :: [Int] -> Int
checkCountEven list = helper list 0
  where helper [] cur_sum = cur_sum
        helper (x:list) cur_sum = (1 - x `mod` 2) + helper list cur_sum

test1 = quickCheck (\list -> countEven list == checkCountEven list)
test2 = quickCheck (\list -> countEven' list == checkCountEven list)
test3 = quickCheck (\list -> countEven'' list == checkCountEven list)
