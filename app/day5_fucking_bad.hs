module Main where

import Data.List
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, Maybe)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Functor (fmap)
import System.IO

type Stack = [Char]

boxChars :: String -> [Char]
boxChars s = [s !! i | i <- [1,5..(length s)]]

parseStacks :: [String] -> [Stack]
parseStacks xs = let boxLines = init xs
                     boxes = reverse $ map boxChars boxLines
                     stacks = map (filter (/=' ')) $ transpose boxes
                 in stacks

parseMoves :: [String] -> [[Int]]
parseMoves xs = let moveLines = tail xs
                    wordLines = map words moveLines
                    maybeNums = (map  (map readMaybe) wordLines ) :: [[Maybe Int]]
                in map catMaybes maybeNums

dropL :: Seq.Seq a -> Int -> Seq.Seq a
dropL xs i = (Seq.reverse . (Seq.drop i) . Seq.reverse) xs
            
moveStacks :: [Stack] -> [Int] -> [Stack]
moveStacks stacks (count:from:to:_) = let sequenceStack = Seq.fromList $ map Seq.fromList stacks
                                          addedStack = (Seq.index sequenceStack (to - 1))
                                          subtractedStack = (Seq.index sequenceStack (from - 1))
                                          movingSection = ((Seq.take count) . Seq.reverse) subtractedStack
                                          newStacks1 = Seq.update (to - 1) (addedStack Seq.>< movingSection) sequenceStack
                                          newStacks2 = Seq.update (from - 1) (dropL subtractedStack count) newStacks1
                                      in Foldable.toList $ fmap Foldable.toList newStacks2

moveStacks' :: [Stack] -> [Int] -> [Stack]
moveStacks' stacks (count:from:to:_) = let sequenceStack = Seq.fromList $ map Seq.fromList stacks
                                           addedStack = (Seq.index sequenceStack (to - 1))
                                           subtractedStack = (Seq.index sequenceStack (from - 1))
                                           movingSection = (Seq.reverse . (Seq.take count) . Seq.reverse) subtractedStack
                                           newStacks1 = Seq.update (to - 1) (addedStack Seq.>< movingSection) sequenceStack
                                           newStacks2 = Seq.update (from - 1) (dropL subtractedStack count) newStacks1
                                       in Foldable.toList $ fmap Foldable.toList newStacks2

part1 :: [Stack] -> [[Int]] -> [Stack]
part1 stacks moves = foldl moveStacks stacks moves

part2 :: [Stack] -> [[Int]] -> [Stack]
part2 stacks moves = foldl moveStacks' stacks moves

printStacks :: [Stack] -> IO ()
printStacks xs = mapM_ putStrLn xs

parseInput :: String -> ([Stack], [[Int]])
parseInput contents = let (stacks, moves) = break (=="") (lines contents)
                          startStacks = parseStacks stacks
                          startMoves = parseMoves moves
                      in (startStacks, startMoves)

main = do
    -- Test
    -- contents <- readFile "test.txt"
    -- let (testStacks, testMoves) = parseInput contents
    -- let testStack = part1 testStacks testMoves
    -- putStrLn $ map last testStack

    inputContents <- readFile "input_day5.txt"
    let (inputStacks, inputMoves) = parseInput inputContents

    putStrLn "Part 1"
    let part1Stack = part1 inputStacks inputMoves
    putStrLn $ map last part1Stack

    putStrLn "Part 2"
    let part2Stack = part2 inputStacks inputMoves
    putStrLn $ map last part2Stack



-- import Data.List
-- import Data.List.Split (splitOn)

-- main :: IO ()

-- -- data Crate = Crate Char deriving (Show, Eq)
-- -- type Wharf = M.IntMap [Crate]

-- -- eName :: Crate -> Char
-- -- eName (Crate c) = c   

-- main = do

--     ls <- (readFile "input_day5_snippet.txt")
--     -- we read the lines
--     let lsx = lines ls
--     --  we parse

--     -- drop the first 10 lines
--     let commands = drop 10 lsx
--     -- initialize the state of the wharf
--     let initialState = [["H", "L", "R", "F", "B", "C", "J", "M"], ["D", "C", "Z"], ["W", "G", "N", "C", "F", "J", "H"],["B", "S", "T", "M", "D", "J", "P"], ["J", "R", "D", "C", "N"], ["Z", "G", "J", "P", "Q", "D", "L", "W"],["H", "R", "F", "T", "Z", "P"], ["G", "M", "V", "L"], ["J","R", "Q", "F", "P", "G", "B", "C"]]

--     -- ["Z", "W", "G", "N", "C", "D", "C", "Z"],["J", "B", "S", "T", "M", "F", "J", "H"],["", "G", "J", "J", "R", "D", "J", "P"],["", "", "H", "P", "Q", "D", "C", "N"],["", "", "", "R", "F", "D", "L", "W"],["", "", "", "F", "G", "T", "Z", "P"],["", "", "", "", "P", "M", "V", "L"],["", "", "", "", "", "G", "B", "C"]]
--     --process all the commands

--     let finalState = applyMoves initialState commands

--     print finalState
--     -- -- -- Get the top of the stack in each column
--     -- let tops = map (topOfStack finalState) [0..8]

--     -- -- Convert the list of tops to a string and print it
--     -- putStrLn (unwords tops)

-- topOfStack :: [[String]] -> Int -> String
-- topOfStack state col = head (dropWhile (== "") (state !! col))

-- -- Apply the given list of moves to the given stacks of crates, returning the resulting stacks.
-- applyMoves :: [[Char]] -> [[String]] -> [[Char]]
-- applyMoves = foldl' applyMove

-- -- Apply a single move to the given stacks of crates, returning the resulting stacks.
-- applyMove :: [[Char]] -> [String] -> [[Char]] 
-- applyMove stacks [m, n, _, _, _, _, _] = foldl' makeMove stacks (replicate (read n) m)

-- -- Make a single move on the given stacks of crates, returning the resulting stacks.
-- makeMove :: [[Char]] -> String -> [[Char]] 
-- makeMove stacks [_, _, _, _, from, _, _, _, _, to] =
--   let fromIndex = read from
--       toIndex = read to
--       (c:origin) = stacks !! fromIndex
--       destination = c:(take toIndex stacks) ++ [origin] ++ (drop (toIndex + 1) stacks)
--   in destination

-- -- processCommands :: [[String]] -> [String] -> [[String]]
-- -- processCommands state [] = state -- If there are no more commands, return the state
-- -- processCommands state (cmd:cmds) =
-- --   -- Split the command string into its parts
-- --   let parts = words cmd
-- --       -- Extract the parts of the command
-- --       count = read (parts !! 1) :: Int -- The number of crates to move
-- --       from = read (parts !! 3) - 1 :: Int -- The index of the source row
-- --       to = read (parts !! 5) - 1 :: Int -- The index of the destination row
-- --   in
-- --   -- Check that the from and to indices are valid
-- --   if from >= 0 && from < length state && to >= 0 && to < length state
-- --   then
-- --     -- Apply the command to the state and process the remaining commands
-- --     processCommands (moveCrates count (state !! from) (state !! to)) cmds
-- --   else
-- --     -- If the from or to indices are invalid, return the current state without applying the command
-- --     state

-- -- -- Define a function that moves a given number of crates from one row to another
-- -- moveCrates :: Int -> [String] -> [String] -> [[String]]
-- -- moveCrates count from to =
-- --   -- Move the specified number of crates from the source row to the destination row
-- --   let newFrom = drop count from
-- --       newTo = take count from ++ to
-- --   in
-- --   -- Check if the source stack is now empty
-- --   if null newFrom
-- --   then
-- --     -- If the source stack is empty, return the state with the source stack as an empty row
-- --     [newFrom, newTo]
-- --   else
-- --     -- Return the updated state
-- --     [newFrom, newTo]


-- --     let processedInput = map (parse) $ lines ls

-- --     let answer = length $ filter completeOverlap processedInput
-- --     let answer2 = length $ filter partialOverlap processedInput

-- --     print answer
-- --     print answer2

-- -- msplit :: (Eq a) => [[a]] -> [a] -> [[a]]
-- -- msplit ds xs = foldl (\acc d -> concatMap (splitOn d) acc) [xs] ds

-- -- parse :: String -> [Int]
-- -- parse = map read . msplit [",", "-"]

-- -- lteOrdered :: Ord a => [a] -> Bool
-- -- lteOrdered xs = xs == sort xs

-- -- completeOverlap :: [Int] -> Bool
-- -- completeOverlap [a,b,c,d] = any lteOrdered [[a,c,d,b], [c,a,b,d]]

-- -- partialOverlap :: [Int] -> Bool
-- -- partialOverlap [a,b,c,d] = any lteOrdered [[a,c,b], [c,a,d]]
