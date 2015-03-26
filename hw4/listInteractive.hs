import System.IO

main = do  	
  doLoop []

doLoop curList = do
  putStrLn "Enter one of the following commands:\n\
             \0 - to exit\n\
             \1 x - to add value x to sorted list\n\
             \2 x - to remove value from sorted list\n3 - to print list"
  command <- getLine  
  case command of
    "0" -> return ()
    '1':' ':x -> doLoop (insert curList x)
    '2':' ':x -> doLoop (erase curList x)
    "3" -> do
             putStr "["
             printList curList
             doLoop curList                                                   
    _ -> doLoop curList

printList [] = do
  putStrLn "]" 

printList (x:[]) = do
  putStr x
  printList []  

printList (x:list) = do
  putStr (x++",")
  printList list  

insert curList x = (filter (< x) curList) ++ [x] ++ (filter (> x) curList)

erase curList x = (filter (< x) curList) ++ (filter (> x) curList)
