verse :: [a] -> [a]
verse [] = []
verse (x:xs) = verse xs ++ [x]
