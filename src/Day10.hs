module Day10 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (foldl')

data CPU = CPU
  { _clock :: Int
  , _reg   :: Int
  , _hist  :: Map.Map Int Int
  }

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day10.txt"
  let initState  = CPU 0 1 Map.empty
      finalState = foldl' execute initState input
  putStrLn $ "Part 1: " ++ show (part1 finalState)
  putStrLn "Part 2:"
  putStrLn (draw finalState)

execute :: CPU -> String -> CPU
execute (CPU clock x hist) s = case words s of
  "noop":_    -> CPU (clock + 1) x (Map.insert (clock + 1) x hist)
  ["addx", n] -> CPU (clock + 2) (x + read n) hist''
    where hist'  = Map.insert (clock + 1) x hist
          hist'' = Map.insert (clock + 2) x hist'
  _ -> error $ "Illegal instruction: " ++ s

part1 :: CPU -> Int
part1 (CPU _ _ hist) = sum $ map (\i -> i * hist Map.! i) indexes
  where indexes = [20, 60, 100, 140, 180, 220]

draw :: CPU -> String
draw (CPU _ _ hist) = fitScreen (map toPixel [0..239])
  where toPixel n | abs (spritePos - (n `mod` 40)) <= 1  = '#'
                  | otherwise = ' '
          where spritePos = hist Map.! (n + 1) `mod` 40
        fitScreen [] = ""
        fitScreen s  = take 40 s ++ "\n" ++ fitScreen (drop 40 s)
