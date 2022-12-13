module Day01 (main) where

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
  let paras = splitOn "\n\n" input
      elves = (sortBy (flip compare) . map (sum . map read . lines)) paras :: [Integer]
  putStrLn $ "Part 1: " ++ show (head elves)
  putStrLn $ "Part 2: " ++ show (sum (take 3 elves))

