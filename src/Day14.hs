module Day14 (main) where

import System.Environment (getArgs)
import Data.Ix (range)
import Data.Bifunctor (bimap)
import qualified Data.Set as Set

type Coord = (Int, Int)
type Chart = Set.Set Coord

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day14.txt"; x:_ -> x
  rocks <- parse <$> (readFile . withDefault =<< getArgs)
  let maxy = (maximum . map snd . Set.elems) rocks
  putStrLn $ "Part 1: " ++ show (part1 maxy (rocks, 0))
  putStrLn $ "Part 2: " ++ show (Set.size (part2 maxy rocks (500, 0) Set.empty))

parse :: String -> Chart
parse = foldr ((Set.union . makeOne) . toPath) Set.empty . lines
  where stripComma = (filter (/= ',') <$>)
        readBoth   = bimap read read
        toPath     = map (readBoth . stripComma . break (==',')) . filter (/= "->") . words
        toRange ((x1, y1), (x2, y2)) = range ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
        makeOne = foldr (flip (foldr Set.insert) . toRange) Set.empty . (zip <*> tail)

part1 :: Int -> (Chart, Int) -> Int
part1 maxy (chart, n)
  | n == n' = n
  | otherwise = part1 maxy (chart', n')
  where (chart', n') = drip maxy (500, 0) (chart, n)

drip :: Int -> Coord -> (Chart, Int) -> (Chart, Int)
drip maxy (x, y) (chart, n)  -- check for falling into the abyss
  | all (`Set.notMember` chart) [(x, y') | y' <- [y + 1 .. maxy]] = (chart, n)
drip maxy (x, y) (chart, n) = case filter (`Set.notMember` chart) toTry of
  (c:_) -> drip maxy c (chart, n)
  []    -> (Set.insert (x, y) chart, n + 1) -- came to a rest
  where toTry = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

part2 :: Int -> Chart -> Coord -> Set.Set Coord -> Set.Set Coord
part2 maxy chart (x, y) visited
  | y >= maxy + 2 = visited
  | (x, y) `Set.member` visited = visited
  | (x, y) `Set.member` chart   = visited
  | otherwise = foldr (part2 maxy chart) (Set.insert (x, y) visited) toTry
  where toTry = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

