{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.List
import System.Directory

data CustomError = IncorrectParameters
                 | IncorrectFile
                 | MiscError String                 

instance Show CustomError where
  show IncorrectParameters = "Please, type digit `1` followed by exactly two strings separated by whitespace"
  show IncorrectFile = "Incorrect file format"
  show (MiscError str) = str

instance Error CustomError where
  noMsg = MiscError "Unknown error"
  strMsg str = MiscError str

type ErrorMonad = Either CustomError  

main = do  	
  doLoop []

doLoop curList = do
  putStrLn "\nEnter one of the following commands:\n\
            \0 - to exit\n\
            \1 <name> <phone number> - to add new contact to list\n\
            \2 <name> - to find phone number by name\n\
            \3 <phone number> - to find name by phone number\n\
            \4 <filename> - to read data from file\n\
            \5 <filename> - to save data to file\n"
  command <- getLine  
  case command of
    "0" -> return ()
    '1':' ':name_number -> do
                             r <- runErrorT (arrToPair $ words (trimnl name_number))
                             case (r) of
                              (Left e) -> do
                                 putStrLn $ show e                                
                                 doLoop curList
                              (Right newFirst) -> do
                                 doLoop (newFirst:curList)    
    '2':' ':name -> do                      
                     putStrLn $ output(findNumberByName (trimnl name) curList)
                     doLoop curList                                                            
    '3':' ':number -> do
                       putStrLn $ output(findNameByNumber (trimnl number) curList)            
                       doLoop curList                                                   
    '4':' ':filename -> do
                         isCorrectFilename <- doesFileExist filename
                         if isCorrectFilename then do
                           file <- readFile filename
                           doLoop $ map arrToPair' (map words (lines file))
                         else do
                           putStrLn "Incorrect filename"
                           doLoop curList                                 
    '5':' ':filename -> do                         
                         writeFile filename (intercalate "\n" $ map pairToString curList) 
                         putStrLn "Data saved to file"
                         doLoop curList
    _ -> doLoop curList
   where     
     arrToPair [a, b] = return (a, b)
     arrToPair _ = throwError IncorrectParameters

     arrToPair' [a, b] = (a, b)

     trimnl = reverse . dropWhile (=='\n') . reverse

     pairToString (a, b) = a ++ " " ++ b

output :: Maybe String -> String
output Nothing = "Not found"
output (Just x) = x

findNumberByName :: String -> [(String, String)] -> Maybe String
findNumberByName name [] = Nothing
findNumberByName name (first:other) | fst first == name = Just (snd first)
                                    | otherwise = findNumberByName name other 

findNameByNumber :: String -> [(String, String)] -> Maybe String
findNameByNumber name [] = Nothing
findNameByNumber number (first:other) | snd first == number = Just (fst first)
                                      | otherwise = findNameByNumber number other 
