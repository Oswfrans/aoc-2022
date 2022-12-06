module Main where

import Data.List (intersect)
import Data.Maybe (fromJust)

main :: IO ()

main = do

    ls <- (readFile "input_day3.txt")
    -- we read the lines 
    -- split each line down the middle in to a tuple or list
    -- change each letter to a number
    -- find the common ones
    -- sum over all the lines
    
    let processedInput = map (splitLine) $ lines ls

    --not exactly sure how this works
    let answer = sum $ map (points . head . foldr1 intersect) processedInput

    print answer

    let chunkedInput = (chunksOf 3) $ lines ls

    let answer2 = sum $ map (points . head . foldr1 intersect) chunkedInput

    print answer2


chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

splitLine :: [a] -> ([a], [a])
splitLine xs = splitAt l xs
    where l = length xs `div` 2

points :: Char -> Int
points c = fromJust $ lookup c (zip ['a' .. 'z'] [1 ..] ++ zip ['A' .. 'Z'] [27 ..])


