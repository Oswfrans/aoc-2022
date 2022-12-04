module Main where

data Janken = Rock | Paper | Scissors deriving (Enum, Eq, Show)

instance Ord Janken where
    compare x        y        | x == y = EQ
    compare Rock     Paper             = LT
    compare Paper    Scissors          = LT
    compare Scissors Rock              = LT
    compare _        _                 = GT

main :: IO ()

main = do

    ls <- (readFile "input_day2.txt")
    let processedInput = process ls
    let answer = sum $ map (uncurry decode) processedInput
    print answer
    let answer2 = sum $ map (uncurry decode2) processedInput
    print answer2

decode ::  Janken -> Janken -> Int
decode x y =
    let score1 = power y
        --can we simplify this?
        score2 = (*3) $ (2-) $ fromEnum $ compare x y
    in  score1 + score2

decode2 :: Janken -> Janken -> Int
decode2 x y =
    let yourpick = picker x y
        score1 = power yourpick
        score2 = (*3) $ (2-) $ fromEnum $ compare x yourpick
    in score1+score2
 
picker :: Janken -> Janken -> Janken
picker x y
    | y == Rock && x == Rock = Scissors
    | y == Rock && x == Paper = Rock
    | y == Rock && x == Scissors = Paper
    | y == Paper = x
    | y == Scissors && x == Rock = Paper
    | y == Scissors && x == Paper = Scissors
    | y == Scissors && x == Scissors = Rock

power :: Janken -> Int
power y
    | y == Rock = 1
    | y == Paper = 2
    | y == Scissors = 3


process :: String -> [(Janken, Janken)]
process = (zip <$> map (lparse . head) <*> map (rparse . last)) . lines
    where
        lparse 'A' = Rock
        lparse 'B' = Paper
        lparse 'C' = Scissors
        rparse 'X' = Rock
        rparse 'Y' = Paper
        rparse 'Z' = Scissors

