import Test.HUnit

data Graph = Graph Int [(Int,Int,Int)] 

infinity = maxBound :: Int

dijkstra :: Graph -> Int -> [Int]
dijkstra (Graph n edges) start = 
	helper (zip (initDistances 0 n start) (initVisited 0 n)) (Graph n edges)
  where
	initDistances :: Int -> Int -> Int -> [Int] 
	initDistances cur n start | cur == n = []
	                          | cur == start = 0 : initDistances (cur + 1) n start
	                          | otherwise = infinity : initDistances (cur + 1) n start

	initVisited :: Int -> Int -> [Bool]                         
	initVisited cur n | cur == n = []
	                  | otherwise = False : initVisited (cur + 1) n

	helper :: [(Int, Bool)] -> Graph -> [Int]
	helper state (Graph n edges) | isTimeToFinish state = getDistances state
	                             | otherwise = helper (makeStep state edges) (Graph n edges)

	getDistances :: [(Int, Bool)] -> [Int]
	getDistances ((d, v):edges) = d : getDistances edges
	getDistances [] = []

	isTimeToFinish :: [(Int, Bool)] -> Bool
	isTimeToFinish [] = True
	isTimeToFinish ((d, v):state) | d /= infinity && (not v) = False
	                              | otherwise = isTimeToFinish state

	makeStep :: [(Int, Bool)] -> [(Int, Int, Int)] -> [(Int, Bool)]
	makeStep state edges = relax (markAsVisited 0 (getStartVertex (-1) infinity 0 state) state) 
	    (filter (startsWith $ getStartVertex (-1) infinity 0 state) edges)

	markAsVisited :: Int -> Int -> [(Int, Bool)] -> [(Int, Bool)]
	markAsVisited _ _ [] = []
	markAsVisited cur vertexToMark ((distance, visited):state) 
	  | cur == vertexToMark = (distance, True) : markAsVisited (cur + 1) vertexToMark state
	  | otherwise = (distance, visited) : markAsVisited (cur + 1) vertexToMark state 

	startsWith :: Int -> (Int, Int, Int) -> Bool
	startsWith start (u, v, w) = start == u

	getStartVertex :: Int -> Int -> Int -> [(Int, Bool)] -> Int
	getStartVertex bestStart bestRes cur ((a,b):state) 
		| a < bestRes && (not b) = getStartVertex cur a (cur + 1) state
		| otherwise = getStartVertex bestStart bestRes (cur + 1) state
	getStartVertex bestStart _ _ [] = bestStart 

	relax :: [(Int, Bool)] -> [(Int, Int, Int)] -> [(Int, Bool)]
	relax state ((u, v, w):edges) = relax (relaxEdge (u, v, w) 0 state (getDistance u 0 state)) edges
	relax state [] = state

	getDistance :: Int -> Int -> [(Int, Bool)] -> Int
	getDistance v cur ((distance, visited):state) | cur == v = distance
	                                              | otherwise = getDistance v (cur + 1) state
	relaxEdge :: (Int, Int, Int) -> Int -> [(Int, Bool)] -> Int -> [(Int, Bool)]
	relaxEdge (u, v, w) cur ((distance, visited):state) distanceWithoutLast 
	  | cur == v = (min distance (distanceWithoutLast + w), visited) : relaxEdge (u, v, w) (cur + 1) state distanceWithoutLast
	  | otherwise = (distance, visited) : relaxEdge (u, v, w) (cur + 1) state distanceWithoutLast  
	relaxEdge (u, v, w) _ [] _ = []	

-- testing

test1 = TestCase(assertEqual "1" [2, 0] (dijkstra (Graph 2 [(1, 0, 2)]) 1))
test2 = TestCase(assertEqual "2" [3, 1, 2, 0] 
	(dijkstra (Graph 4 [(3, 2, 2), (3, 1, 1), (2, 0, 1), (1, 0, 5)]) 3))
test3 = TestCase(assertEqual "3" [2, 3, 0, 1, infinity] 
	(dijkstra (Graph 5 [(0, 1, 1), (1, 2, 1), (2, 3, 1), (3, 0, 1)]) 2))

tests = TestList [TestLabel "1" test1,
                  TestLabel "2" test2,
                  TestLabel "3" test3]
                  
runTesting = runTestTT tests
