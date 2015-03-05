sumAdjacent :: [Int] -> [Int]
sumAdjacent (first:list) = zipWith (+) (first:list) (list++[0])

maxAdjacentSum :: [Int] -> Int
maxAdjacentSum [] = 0
maxAdjacentSum [elem] = 0
maxAdjacentSum list = find 1 maxValue (sumAdjacent list)
  where find pos value (first:list) = if value == first
                                      then pos
                                      else find (pos + 1) value list
        maxValue = foldl max 0 (sumAdjacent list)
