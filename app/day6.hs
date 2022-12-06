module Main where

import Data.List
import Data.List.Split (splitOn)

main :: IO ()

main = do

    ls <- (readFile "input_day6.txt")
    -- we read the file

    print (findStart ls 0 4)

    print (findStart ls 0 14)

findStart :: String -> Int -> Int -> Int
findStart s startIndex length = 
  -- Get the subsequence of the input string starting at the startIndex position
  -- and with the specified length
  let subsequence = subseq s startIndex length
  in 
  -- If all characters in the subsequence are different, return the index of the end of the subsequence
  if allDifferent subsequence
  then 
    startIndex + length
  -- Otherwise, try again with the next character in the input string
  else
    findStart s (startIndex + 1) length

-- A helper function to get a subsequence of a string
subseq :: String -> Int -> Int -> String
subseq s startIndex length = take length $ drop startIndex s

-- A helper function to check if all characters in a string are different
allDifferent :: String -> Bool
allDifferent str = length str == length (nub str)



-- -- Define a function to compute the result
-- firstFourDifferent :: String -> Int
-- firstFourDifferent input =
--   -- Find the first subsequence of length 4 where all characters are different
--   let subseqs = tails input
--       firstFour = head $ filter allDifferent $ map (take 4) subseqs
--   in
--   -- Return the length of the input up to the end of the first such subsequence
--   length input - length firstFour

-- -- Define a helper function to check if all characters in a string are different
-- allDifferent :: String -> Bool
-- allDifferent str =
--   length str == length (nub str)
--     -- we have a string which we traverse and we count the number of characters 
--     -- until we have the last four chars which are different


