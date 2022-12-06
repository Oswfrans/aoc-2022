module Main where

import Data.List
import Data.List.Split (splitOn)

main :: IO ()

main = do

    ls <- (readFile "input_day4.txt")
    -- we read the lines
    --  we parse

    let processedInput = map (parse) $ lines ls

    let answer = length $ filter completeOverlap processedInput
    let answer2 = length $ filter partialOverlap processedInput

    print answer
    print answer2

msplit :: (Eq a) => [[a]] -> [a] -> [[a]]
msplit ds xs = foldl (\acc d -> concatMap (splitOn d) acc) [xs] ds

parse :: String -> [Int]
parse = map read . msplit [",", "-"]

lteOrdered :: Ord a => [a] -> Bool
lteOrdered xs = xs == sort xs

completeOverlap :: [Int] -> Bool
completeOverlap [a,b,c,d] = any lteOrdered [[a,c,d,b], [c,a,b,d]]

partialOverlap :: [Int] -> Bool
partialOverlap [a,b,c,d] = any lteOrdered [[a,c,b], [c,a,d]]
