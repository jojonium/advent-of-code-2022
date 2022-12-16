module Day16 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.Char (isNumber)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)
import Algorithm.Search (dijkstra)

data V = V
  { _name :: String
  , _fr   :: Int
  , _tun  :: [String]
  , _open :: Bool
} deriving (Show, Eq, Ord)

data State = State
  { _cur  :: String
  , _time :: Int
  , _vs   :: Map.Map String V
  , _pres :: Int
} deriving (Show, Eq)

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day16.txt"; x:_ -> x
  valves <- parse <$> (readFile . withDefault =<< getArgs)
  let state = State "AA" 0 valves 0
      final = explore state
  print final
  putStrLn $ "Part 1: " ++ show (_pres final)

parse :: String -> Map.Map String V
parse = foldr (\s m -> let x = toV s in Map.insert (_name x) x m) Map.empty . lines
  where toV s = V name rate tuns False
          where ws = words s
                name = ws !! 1
                rate = (read . dropWhile (not . isNumber) . init . (!! 4)) ws
                tuns = (map dropComma . drop 9) ws
                dropComma x | last x == ',' = init x | otherwise = x

explore :: State -> State
explore s@(State _ 30 _ _) = s
explore (State cur t vs pres)
  | null opts    = State cur 30 vs pres
  | null results = State cur 30 vs pres
  | otherwise = maximumBy (comparing _pres) results
  where curV    = vs Map.! cur
        opts    = filter (not . _open) (Map.elems vs)
        costs   = mapMaybe (\x -> dijkstra (map (vs Map.!) . _tun) (\_ _ -> 1) (==x) curV) opts
        results = mapMaybe toState costs
        toState (_, []) = Nothing
        toState (c, path)
          | t' > 30   = Nothing
          | otherwise = trace (show pres') (Just (explore (State cur' t' vs' pres')))
          where t' = t + c + 1
                next  = last path
                pres' = pres + (_fr next * (30 - t'))
                cur'  = _name next
                vs'   = Map.adjust (\x -> x { _open = True }) cur' vs

