module Day08 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map

type Coord = (Int, Int)
data Chart = Chart
  { _m :: Map.Map Coord Int
  , _w :: Int
  , _h :: Int
} deriving (Show)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day08.txt"
  let grid = parse input
  putStrLn $ "Part 1: " ++ show (part1 grid)
  putStrLn $ "Part 2: " ++ show (part2 grid)

parse :: [String] -> Chart
parse ls = Chart (foldr folder Map.empty cs) w h
  where h  = length ls
        w  = length (head ls)
        cs = [(x, y) | x <- [0..(w - 1)], y <- [0..(h - 1)]]
        folder (x, y) m = let t = read [ls !! y !! x]
                          in Map.insert (x, y) t m

part1 :: Chart -> Int
part1 c = Map.size (Map.filterWithKey (visible c) (_m c))

part2 :: Chart -> Int
part2 c = maximum $ Map.elems (Map.mapWithKey (scenicScore c) (_m c))

dirs :: Chart -> Coord -> [[Int]]
dirs (Chart m w h) (x, y) = 
  [ [m Map.! (x, b) | b <- [y-1, y-2 .. 0]]
  , [m Map.! (a, y) | a <- [x-1, x-2 .. 0]]
  , [m Map.! (x, b) | b <- [y + 1 .. h - 1]]
  , [m Map.! (a, y) | a <- [x + 1 .. w - 1]] ]


visible :: Chart -> Coord -> Int -> Bool
visible chart coord height = any (all (< height)) (dirs chart coord)

scenicScore :: Chart -> Coord -> Int -> Int
scenicScore chart coord height = product (map viewingDist (dirs chart coord))
  where viewingDist cs
          | null trees = 1
          | nt == length cs = nt
          | otherwise = nt + 1
          where trees = takeWhile (< height) cs
                nt    = length trees
