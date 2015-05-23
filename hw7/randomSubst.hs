import System.Random

data Tree a = Leaf a | Tr (Tree a) a (Tree a)

randomSubstitution :: StdGen -> Tree Int -> Tree Int
randomSubstitution gen (Leaf x) = Leaf $ fst $ next gen
randomSubstitution gen (Tr a b c) = Tr (randomSubstitution (fst $ split gen) a) 
                                       (fst $ next gen) 
                                       (randomSubstitution (snd $ split gen) c)

instance (Show a) => Show (Tree a) where
    show (Leaf x) = show x
    show (Tr a b c) = show a ++ "," ++ show b ++ "," ++ show c

sampleTree = Tr (Tr (Leaf 2) 3 (Leaf 4)) 1 (Tr (Leaf 5) 6 (Leaf 7))

main = do
	putStrLn "Type some integer to use as a randseed"
	randSeed <- getLine
	print $ "Initial tree: " ++ show sampleTree	
	print $ "After substitution with random values:"
	print $ show $ randomSubstitution (mkStdGen (read randSeed :: Int)) sampleTree                             