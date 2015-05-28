import Control.Monad.State 
import Test.HUnit (Test(TestCase), Test(TestLabel), Test(TestList), assertEqual, runTestTT)

data HashList = Hash Int [Int]

instance Show HashList where
    show (Hash a b) = show a ++ ": " ++ show b

getHash :: HashList -> Int
getHash (Hash x y) = x

getList :: HashList -> [Int]
getList (Hash x y) = y

calcHash :: Int -> Int
calcHash x = x * 2654435761 `mod` 2^32 

type HashTable = [HashList]

search :: Int -> HashTable -> Bool  
search _ [] = False
search x (first:rest) | calcHash x == getHash first = containedInList x (getList first)
                      | otherwise = search x rest
    where 
        containedInList x [] = False
        containedInList x (first:rest) | x == first = True
                                       | otherwise = containedInList x rest

add :: Int -> State HashTable ()
add x = do  
    table <- get
    if search x table 
      then       
        return ()
      else do
        if searchHash (calcHash x) table 
          then do                     
            put $ add x table
            return ()
          else do            
            put $ (Hash (calcHash x) [x]) : table
            return ()
  where
    searchHash :: Int -> HashTable -> Bool
    searchHash _ [] = False
    searchHash x (first:rest) | calcHash x == getHash first = True
                              | otherwise = searchHash x rest
    add :: Int -> HashTable -> HashTable
    add _ [] = []
    add x (first:rest) | calcHash x == getHash first = (Hash (getHash first) (x:getList first) ) : rest
                       | otherwise = first : (add x rest)                                             

delete :: Int -> State HashTable (Maybe Int)
delete x = do
    table <- get
    if search x table
      then do        
        put $ deleteNum x table 
        return (Just x)
      else do
        return Nothing
  where
    deleteNum x [] = []
    deleteNum x (first:rest) | calcHash x == getHash first = 
            (Hash (getHash first) (deleteFromList x (getList first))) : rest
                             | otherwise = first : deleteNum x rest
      where
        deleteFromList x [] = []
        deleteFromList x (first:rest) | x == first = rest
                                      | otherwise = first : (deleteFromList x rest)                       

-- testing

table1 = execState (add 1) []
table2 = execState (add 4) table1
table3 = execState (add 2) table2
table4 = execState (add 3) table3
table5 = execState (add 2) table4
table6 = execState (delete 2) table5     
table7 = execState (delete 2) table6
table8 = execState (delete 3) table7

test1 = TestCase(assertEqual "1" False (search 2 table2))
test2 = TestCase(assertEqual "2" True (search 2 table5))
test3 = TestCase(assertEqual "3" False (search 2 table6))
test4 = TestCase(assertEqual "4" True (search 3 table7))
test5 = TestCase(assertEqual "5" False (search 3 table8))
test6 = TestCase(assertEqual "6" (Just 2) (evalState (delete 2) table5))
test7 = TestCase(assertEqual "7" Nothing (evalState (delete 2) table6))

tests = TestList [TestLabel "1" test1,
                  TestLabel "2" test2,
                  TestLabel "3" test3,
                  TestLabel "4" test4,
                  TestLabel "5" test5,
                  TestLabel "6" test6,
                  TestLabel "7" test7]
                  
runTesting = runTestTT tests
