sumDivCos :: [Double] -> Double
sumDivCos list = helper list 0 1
  where
	helper [] sumNumbers prodCos = sumNumbers / prodCos
	helper (first:list) sumNumbers prodCos = helper list (sumNumbers + first) (prodCos * cos first)