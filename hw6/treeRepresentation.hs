import Test.HUnit

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

treeToString :: Tree Char -> String
treeToString Empty = "e"
treeToString (Node x l r) = 'n':x: treeToString l 
                        ++ treeToString r

stringToTree :: String -> Tree Char
stringToTree "e" = Empty
stringToTree ('n':x:rest) = Node x (stringToTree $ fst (split 1 0 rest "")) 
                                   (stringToTree $ snd (split 1 0 rest ""))
    where    	
        split ns es (first:next:str) res | ns == es = (res, (first:next:str))
                                         | first == 'e' = split ns (es + 1) (next:str) (res ++ "e")		                                
                                         | first == 'n' = split (ns + 1) es str (res ++ ('n':[next]))
                                         | otherwise = split ns es (next:str) (res ++ [first])
        split ns es "e" res = (res, "e")                                

-- testing

tree1 = Node 'a' 
          (Node 'b' 
          	(Node 'd' 
              Empty 
              Empty
            ) 
          	Empty
          ) 
          (Node 'c' 
          	Empty 
          	Empty
          )
test1 = TestCase(assertEqual "1" "nanbndeeencee" (treeToString tree1))
test2 = TestCase(assertEqual "2" tree1 (stringToTree "nanbndeeencee"))

tree2 = Node 'a' 
          (Node 'b' 
            Empty 
            (Node 'd' 
              Empty 
              (Node 'e' 
                Empty 
                Empty
              )
            )
          ) 
        (Node 'c' 
          (Node 'f' 
            Empty 
            Empty
          ) 
          Empty
        )
test3 = TestCase(assertEqual "3" "nanbendeneeencnfeee" (treeToString tree2))
test4 = TestCase(assertEqual "4" tree2 (stringToTree "nanbendeneeencnfeee"))

tests = TestList [TestLabel "1" test1, 
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4]
                  
runTesting = runTestTT tests
