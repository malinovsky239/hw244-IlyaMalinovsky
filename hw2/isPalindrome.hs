f :: String -> Char -> String
f a b = b:a 

myReverse :: String -> String
myReverse list = foldl f [] list

isPalindrome :: String -> Bool
isPalindrome string = myReverse string == string
                     