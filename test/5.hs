data PrintedProduct =
	Book {
		name :: String,
		author :: String,
		cost :: Double
	}
	|
	Journal {
		name :: String,
		year :: Int,
		number :: Int,
		cost :: Double
	}

totalCost :: [PrintedProduct] -> Double
totalCost [] = 0
totalCost (first:list) = cost first + totalCost list	