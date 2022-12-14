module Day13 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

data PD = Leaf Int | Node [PD] deriving (Eq, Show)

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day13.txt"; x:_ -> x
  input <- readFile . withDefault =<< getArgs
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry compare . toPair) . splitOn "\n\n"

part2 :: String -> Int
part2 input = index divA * index divB
  where divA    = parse "[[2]]"
        divB    = parse "[[6]]"
        sortPs  = sort . (++ [divA, divB]) . map parse . filter (/= "") . lines
        packets = sortPs input
        index a = fromJust (elemIndex a packets) + 1

parse :: String -> PD
parse = snd . (`parseR` [])

parseR :: String -> [PD] -> (String, PD)
parseR []       ps = ([], head ps)
parseR ('[':xs) ps = let (xs', n) = parseR xs [] in parseR xs' (ps ++ [n])
parseR (']':xs) ps = (xs, Node ps)
parseR (',':xs) ps = parseR xs ps
parseR xs       ps = let (token, rest) = break (`elem` ",][") xs 
                     in parseR rest (ps ++ [Leaf (read token)])

toPair :: String -> (PD, PD)
toPair s = let ls = lines s in (parse (head ls), parse (ls !! 1))

instance Ord PD where
  compare (Leaf i)  (Leaf j)  = compare i j
  compare (Node is) (Node js) = compare is js
  compare (Leaf i)  (Node js) = compare (Node [Leaf i]) (Node js)
  compare (Node is) (Leaf j)  = compare (Node is) (Node [Leaf j])
