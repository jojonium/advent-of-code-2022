module Day17 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf)
import Debug.Trace (trace)

data Rock  = Hor | Plus | Angle | Vert | Square deriving (Eq, Enum)
type Coord = (Int, Int)
type Chart = Map.Map Coord Char
data State = State
  { _jets  :: String
  , _rocks :: [Rock]
  , _chart :: Chart
}

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day17.txt"; x:_ -> x
  input <- init <$> (readFile . withDefault =<< getArgs)
  let jets  = cycle input
      rocks = cycle [Hor .. Square]
      chart = foldr (`Map.insert` '-') Map.empty [(x, 0) | x <- [0..6]]
      initS = State jets rocks chart
  putStrLn $ "Part 1: " ++ show (part1 initS 2022)
  putStrLn $ "Part 2: " ++ show (part2 (length input) 5 initS)

part1 :: State -> Int -> Int
part1 state n = (maximum . map snd . Map.keys . _chart) final
  where final = iterate fall state !! n

part2 :: Int -> Int -> State -> (Int, Int, Int, Int)
part2 iLen rLen s = trace ("repeatN = " ++ show repeatN) findCycle (fall s) repeatN (0, 0, 0, 1)
  where repeatN  = lcm iLen rLen
        repeatH  = part1 s repeatN
        target   = 2022 -- 1000000000000
        baseT    = (target `div` repeatN) * repeatH
        addT     = part1 s (target `mod` repeatN)

findCycle :: State -> Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
findCycle s@(State _ _ chart) repeatN (lastH, lastDH, lastN, n)
  | n `mod` repeatN == 0 =
    let topY = maximum (map snd (Map.keys chart))
        dh   = topY - lastH
    in if dh == lastDH then (topY, dh, lastN, n)
       else findCycle (fall s) repeatN (topY, dh, n, n + 1)
  | otherwise = findCycle (fall s) repeatN (lastH, lastDH, lastN, n + 1)


findFlat :: State -> Int -> (Int, Int)
findFlat s@(State _ _ chart) n
  | all (`Map.member` chart) [(x, topY) | x <- [0..6]] = (n, topY)
  | otherwise = findFlat (fall s) (n + 1)
  where topY = maximum (map snd (Map.keys chart))

toPoints :: Int -> Int -> Rock -> [Coord]
toPoints left bot Hor    = [(left + dx, bot) | dx <- [0..3]]
toPoints left bot Plus   = [(left, bot+1), (left+1, bot+1), (left+2, bot+1), (left+1, bot), (left+1, bot+2)]
toPoints left bot Angle  = [(left + dx, bot) | dx <- [0..2]] ++ [(left+2, bot + dy) | dy <- [1, 2]]
toPoints left bot Vert   = [(left, bot + dy) | dy <- [0..3]]
toPoints left bot Square = [(left + dx, bot + dy) | dx <- [0, 1], dy <- [0, 1]]

fall :: State -> State
fall (State jets rocks chart) = State jets' (tail rocks) chart'
  where bot  = maximum (map snd (Map.keys chart)) + 4
        rock = toPoints 2 bot (head rocks)
        (jets', chart') = fallHelper rock jets chart

fallHelper :: [Coord] -> String -> Chart -> (String, Chart)
fallHelper _ [] _ = error "impossible"
fallHelper pts (j:js) chart = (js', chart')
  where xd   = case j of '<' -> -1; '>' -> 1; _ -> error "Oops"
        maxX = maximum (map fst pts)
        minX = minimum (map fst pts)
        pts' | j == '>' && maxX == 6 = pts
             | j == '<' && minX == 0 = pts
             | otherwise = let try = map (\(x, y) -> (x + xd, y)) pts
                           in if any (`Map.member` chart) try then pts else try
        pts'' = map (\(x, y) -> (x, y - 1)) pts'
        (js', chart') = if any (`Map.member` chart) pts''
                        then (js, foldr (`Map.insert` '#') chart pts')
                        else fallHelper pts'' js chart

prettyPrint :: Chart -> String
prettyPrint chart = intercalate "\n" ls
  where topY = maximum (map snd (Map.keys chart))
        ls   = [[Map.findWithDefault '.' (x, y) chart | x <- [0..6]] | y <- [topY, topY - 1 .. 0]]
