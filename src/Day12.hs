module Day12 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Algorithm.Search (aStar)
import Data.Maybe (fromJust)
import Data.Char (ord)

type Coord = (Int, Int)
type Chart = Map.Map Coord Char

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day12.txt"
  let (chart, start, end) = parse input
      (p1, path) = solve chart start end
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show 
    (length (dropWhile (\c -> chart Map.! c == 'a') path) - 1)

parse :: [String] -> (Chart, Coord, Coord)
parse ls = foldr folder (Map.empty, undefined, undefined) [(x, y) | x <- xs, y <- ys]
  where xs = [0 .. length (head ls) - 1]
        ys = [0 .. length ls - 1]
        folder (x, y) (m, s, e)
          | p == 'S'  = (Map.insert (x, y) 'a' m, (x, y), e)
          | p == 'E'  = (Map.insert (x, y) 'z' m, s, (x, y))
          | otherwise = (Map.insert (x, y) p m, s, e)
          where p = ls !! y !! x

solve :: Chart -> Coord -> Coord -> (Int, [Coord])
solve chart s e = fromJust $ aStar neighbors costs manhattan (==e) s
  where neighbors (x, y) = filter climbable adjacent
          where adjacent = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                climbable (x', y') =
                  let h  = ord (chart Map.! (x, y)) 
                      h' = ord (Map.findWithDefault '~' (x', y') chart) 
                  in h - h' >= -1
        manhattan (i, j) = abs (fst s - i) + abs (snd s - j)
        costs _ _ = 1

