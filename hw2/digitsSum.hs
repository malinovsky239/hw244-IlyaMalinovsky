digitsSum :: Int -> Int
digitsSum value = if value < 10
                  then value
                  else digitsSum (div value 10) 
                     + value - (div value 10) * 10