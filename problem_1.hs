myLength           :: [a] -> Int
myLength []        =  0
myLength (_:xs)    =  1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
