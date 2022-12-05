import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (intersect)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <-  readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day04.txt"
  let pairs = map parse input
  putStrLn $ "Part 1: " ++ show (sum (map fullyContains pairs))
  putStrLn $ "Part 2: " ++ show (sum (map overlapAtAll pairs))

parse :: String -> ((Int, Int), (Int, Int))
parse s = case splitOn "," s of
          (s1:s2:_) -> (toTuple s1, toTuple s2)
          _ -> error "Failed to parse"
  where toTuple pair = case splitOn "-" pair of
                       (p1:p2:_) -> (read p1, read p2)
                       _         -> error "Failed to parse"

fullyContains :: ((Int, Int), (Int, Int)) -> Int
fullyContains ((a1, a2), (b1, b2)) =
  if (a1 >= b1 && a2 <= b2) || (b1 >= a1 && b2 <= a2) then 1 else 0

overlapAtAll :: ((Int, Int), (Int, Int)) -> Int
overlapAtAll ((a1, a2), (b1, b2)) =
  case [a1..a2] `intersect` [b1..b2] of (_:_) -> 1; [] -> 0
