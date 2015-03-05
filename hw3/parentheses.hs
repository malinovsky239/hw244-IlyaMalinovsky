isCorrect :: String -> Bool
isCorrect s = isCorrect' s 0
  where
    isCorrect' "" 0 = True
    isCorrect' "" balance = False
    isCorrect' ('(' : s) balance = isCorrect' s (balance + 1)
    isCorrect' (')' : s) balance = if balance == 0 
                                     then False 
                                     else isCorrect' s (balance - 1)
    isCorrect' (first : s) balance = isCorrect' s balance



 