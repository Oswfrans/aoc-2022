import Data.List.Split 
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

main = do
    ls <- fmap Text.lines (Text.readFile "input.txt")
    let  elves = sumElf ls
    let	maxElf = maximum elves

sumElf :: String -> [Int]
sumElf = map (sum ). splitOn "\n\n"

