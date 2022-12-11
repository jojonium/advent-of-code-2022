module Day11 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (foldl', sortBy, isInfixOf)
import Data.List.Split (splitOn)
import Text.Regex.TDFA

data Monkey = Monkey
  { _id    :: Int
  , _items :: [Int]
  , _op    :: Int -> Int -> Int
  , _opN   :: Int
  , _testD :: Int -- divisor for test
  , _testT :: Int -- throw here if true
  , _testF :: Int -- throw here if false
  , _inspC :: Int  -- total number of items inspected
}
type Monkeys = Map.Map Int Monkey

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (splitOn "\n\n" raw)

main :: IO ()
main = do
  input <- getInput "inputs/day11.txt"
  let monkeys = parse input
  putStrLn $ "Part 1: " ++ show (solve 20 (doRound False) monkeys)
  putStrLn $ "Part 2: " ++ show (solve 10000 (doRound True) monkeys)

parse :: [String] -> Monkeys
parse [] = Map.empty
parse (para:rest) = Map.insert i (Monkey i items op opN td tt tf 0) (parse rest)
  where ls    = lines para
        i     = read [head ls !! 7]
        items = map read (getAllTextMatches (ls !! 1 =~ "[0-9]+"))
        (op, opN) 
          | "d * o" `isInfixOf` (ls !! 2) = ((^), 2)
          | "+" `isInfixOf` (ls !! 2)     = ((+), read (ls !! 2 =~ "[0-9]+"))
          | otherwise                     = ((*), read (ls !! 2 =~ "[0-9]+"))
        td = read (ls !! 3 =~ "[0-9]+")
        tt = read (ls !! 4 =~ "[0-9]+")
        tf = read (ls !! 5 =~ "[0-9]+")

turn :: Bool -> Monkeys -> Int -> Monkeys
turn p2 monkeys i = Map.adjust cleanCur i monkeys'
  where (Monkey _ items op opN td tt tf c) = monkeys Map.! i
        cleanCur tm = tm { _inspC = c + length items, _items = [] }
        monkeys'    = foldl' throw monkeys items
        bigMod      = product $ map _testD (Map.elems monkeys)
        throw ms w  = Map.adjust appItem target ms
          where w'     = if p2 then w `op` opN else (w `op` opN) `div` 3
                target = if w' `mod` td == 0 then tt else tf
                appItem tm@(Monkey { _items = is }) = tm { _items = is ++ [w' `mod` bigMod] }

doRound :: Bool -> Monkeys -> Monkeys
doRound p2 monkeys = foldl' (turn p2) monkeys (Map.keys monkeys)

solve :: Int -> (Monkeys -> Monkeys) ->  Monkeys -> Int
solve n fun = product . take 2 . sortBy (flip compare) . map _inspC . Map.elems . (!! n) . iterate fun

