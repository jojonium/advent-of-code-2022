module Day09 (main) where

import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.List (foldl', scanl')

type Coord = (Int, Int)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day09.txt"
  let p1Start = ([(0, 0), (0, 0)], Set.singleton (0, 0))
      p2Start = (replicate 10 (0, 0), Set.singleton (0, 0))
      (_, p1) = foldl' step p1Start input
      (_, p2) = foldl' step p2Start input
  putStrLn $ "Part 1: " ++ show (Set.size p1)
  putStrLn $ "Part 2: " ++ show (Set.size p2)

step :: ([Coord], Set.Set Coord) -> String -> ([Coord], Set.Set Coord)
step ([], _) _ = error "Shouldn't happen"
step (h:ts, set) line = (rope, set')
  where headTrail  = moveHead line h
        tailTrails = tail $ scanl' (flip (scanl' moveTail)) headTrail ts
        set'       = foldr Set.insert set (last tailTrails)
        rope       = last headTrail : map last tailTrails

moveHead :: String -> Coord -> [Coord]
moveHead line (x, y) = case words line of
  ["U", n] -> [(x, y') | y' <- [y - 1, y - 2 .. y - read n]]
  ["R", n] -> [(x', y) | x' <- [x + 1 .. x + read n]]
  ["D", n] -> [(x, y') | y' <- [y + 1 .. y + read n]]
  ["L", n] -> [(x', y) | x' <- [x - 1, x - 2 .. x - read n]]
  _ -> error "Failed to parse"

moveTail :: Coord -> Coord -> Coord
moveTail (tx, ty) (hx, hy)
  | abs (tx - hx) <= 1 && abs (ty - hy) <= 1 = (tx, ty)
  | otherwise = let yDelta  = signum (hy - ty)
                    xDelta  = signum (hx - tx)
                in (tx + xDelta, ty + yDelta) 

