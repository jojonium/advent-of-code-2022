module Day15 (main) where

import System.Environment (getArgs)
import Text.Regex.TDFA
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Range

type Coord = (Int, Int)
data S = S { pos :: Coord, beac :: Coord } deriving (Show)

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day15.txt"; x:_ -> x
  sensors <- parse <$> (readFile . withDefault =<< getArgs)
  putStrLn $ "Part 1: " ++ show (part1 2000000 sensors)
  putStrLn $ "Part 2 (this may take a while): " ++ show (part2 4000000 sensors)
 
parse :: String -> [S]
parse = map (toSensor . nums) . lines
  where nums = map read . getAllTextMatches . (=~ "-?[0-9]+")
        toSensor (a:b:c:d:_) = S (a, b) (c, d)
        toSensor _ = error "No parse"

rangeSize :: Num a => Range a -> a
rangeSize (SpanRange (Bound a Inclusive) (Bound b Inclusive)) = b - a + 1
rangeSize (SpanRange (Bound a Exclusive) (Bound b Inclusive)) = b - a
rangeSize (SpanRange (Bound a Inclusive) (Bound b Exclusive)) = b - a
rangeSize (SpanRange (Bound a Exclusive) (Bound b Exclusive)) = b - a - 1
rangeSize _ = error "Whoops"

lowerBound :: Num a => Range a -> a
lowerBound (SpanRange (Bound a Inclusive) _) = a
lowerBound (SpanRange (Bound a Exclusive) _) = a + 1
lowerBound _ = error "Whoops"

mhtn :: Coord -> Coord -> Int
mhtn (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

fullRanges :: Int -> [S] -> [Range Int]
fullRanges target = mergeRanges . mapMaybe toRange
  where toRange (S (px, py) b)
          | d > m     = Nothing
          | otherwise = let r = m - d in Just ((px - r) +=+ (px + r))
          where m = mhtn (px, py) b
                d = abs (target - py)

part1 :: Int -> [S] -> Int
part1 target sensors = sum occ - beacs
  where occ   = map rangeSize (fullRanges target sensors)
        beacs = (length . filter ((==target) . snd) . nub . map beac) sensors

firstEmpty :: Int -> [S] -> Range Int -> Int
firstEmpty target sensors bounds = 
  case difference [bounds] (fullRanges target sensors) of
    []  -> maxBound
    x:_ -> lowerBound x

part2 :: Int -> [S] -> Int
part2 bound sensors = tx * bound + ty
  where inBounds a = a >= 0 && a <= bound
        cs = map (\y -> (firstEmpty y sensors (0 +=+ bound), y)) [bound `div` 2 .. bound]
        (tx, ty) = head (filter (\(a, b) -> inBounds a && inBounds b) cs)
