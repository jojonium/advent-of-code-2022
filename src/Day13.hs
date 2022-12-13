module Day13 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromJust)

data PD = Leaf Int | Node [PD] deriving (Eq, Show)

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day13.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

part1 :: String -> Int
part1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry compair . toPair) . splitOn "\n\n"

part2 :: String -> Int
part2 input = index divA * index divB
  where divA    = parse "[[2]]"
        divB    = parse "[[6]]"
        sortPs  = sortBy compair . (++ [divA, divB]) . map parse . filter (/= "") . lines
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

compair :: PD -> PD -> Ordering
compair (Leaf i) (Leaf j) = compare i j
compair (Node (i:is)) (Node (j:js)) = case compair i j of
  LT -> LT
  GT -> GT
  EQ -> compair (Node is) (Node js)
compair (Node [])    (Node (_:_)) = LT
compair (Node (_:_)) (Node [])    = GT
compair (Node [])    (Node [])    = EQ
compair (Leaf i)     (Node js)    = compair (Node [Leaf i]) (Node js)
compair (Node is)    (Leaf j)     = compair (Node is) (Node [Leaf j])
