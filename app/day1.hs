module Main where

import Data.List.Split
import Data.List

main :: IO ()

main = do

    ls <- (readFile "input.txt")
    let bigGroups = splitOn "\n\n" ls
    let calories = sumElf bigGroups
    let maxCalories = maximum calories
    
    print maxCalories

    -- sort p2 and take the top 3 and then sum
    -- let sortedCalories = reverse.(Data.List.sort) calories
    let sortedCalories = sort calories
    let sortedCaloriesRev = reverse sortedCalories
    let topThree = take 3 sortedCaloriesRev
    let sumTopThree = sum topThree
    
    print sumTopThree

sumElf :: [String] -> [Int]
sumElf = map (sum . map read . lines)


