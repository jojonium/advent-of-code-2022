import System.Environment (getArgs)
import Data.List (sortBy)
import Data.List.Split (splitOn)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day01.txt"
  let elves = splitOn "\n\n" input
  putStrLn $ "Part 1: " ++ show (part1 elves)
  putStrLn $ "Part 2: " ++ show (part2 elves)

part1 :: [String] -> Integer
part1 = maximum . map (sum . map read . lines)

part2 :: [String] -> Integer
part2 = sum . take 3 . sortBy (flip compare) . map (sum . map read . lines)

