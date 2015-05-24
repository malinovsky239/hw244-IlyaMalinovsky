import Control.Monad

data State = St Int Int Bool

first :: State -> Int
first (St a b c) = a

second :: State -> Int
second (St a b c) = b

wasPrinted :: State -> Bool
wasPrinted (St a b c) = c

greaterThanNeighbours = foldM (\curState next -> (helper curState next) 
	>> return (St (second curState) next 
		(wasPrinted curState || first curState < second curState && second curState > next))) 
		(St 0 0 False) 
	where
		helper curState third =
			if (not (wasPrinted curState) && first curState < second curState && second curState > third)
				then putStrLn ((show $ first curState) ++ 
					" < " ++ (show $ second curState) ++ " > " ++ show third)
				else putStr "" 


main = do	
	putStr "List to search in\n(just type the list elements separated by whitespaces): "
	input <- getLine
	greaterThanNeighbours (map read $ words input)
