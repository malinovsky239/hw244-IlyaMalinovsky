{-
initial function:        
  func x l = map (\y -> y*x) l
after eta-reduction:     
  func x = map (\y -> y*x) 
replace lambda-abstraction with a function                        
  func x = map (* x)
once more eta-reduction: 
  func = map . (*)
-}

func = map . (*)
