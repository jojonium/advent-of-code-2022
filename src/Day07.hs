module Day07 (main) where

import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

data Dir = Dir
  { dName    :: String
  , dFiles   :: Map.Map String Int
  , dFolders :: Map.Map String Dir
} deriving (Show)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day07.txt"
  let empty = Dir "/" Map.empty Map.empty
      root  = parse input ["/"] empty
      allSizes = map (\d -> (dName d, dirSize d)) (allDirs root)
  putStrLn $ "Part 1: " ++ show (part1 allSizes)
  putStrLn $ "Part 2: " ++ show (part2 (dirSize root) allSizes)

part1 :: [(String, Int)] -> Int
part1 = sum . (filter (<= 100000) . map snd)

part2 :: Int -> [(String, Int)] -> Int
part2 total = snd . minimumBy (comparing snd) . filter (\a -> snd a >= toFree)
  where toFree = 30000000 - (70000000 - total)

allDirs :: Dir -> [Dir]
allDirs d = d : Map.foldr ((++) . allDirs) [] (dFolders d)

dirSize :: Dir -> Int
dirSize (Dir { dFolders = ds, dFiles = fs })
  | Map.null ds = justFiles
  | otherwise   = justFiles + children
  where children  = Map.foldr ((+) . dirSize) 0 ds
        justFiles = sum (Map.elems fs)

parse :: [String] -> [String] -> Dir -> Dir
parse []     _   tree = tree
parse (s:ss) pwd tree = case words s of
  ["$", "cd", "/" ] -> parse ss ["/"] tree
  ["$", "cd", ".."] -> parse ss (init pwd) tree
  ["$", "cd", dir ] -> parse ss (pwd ++ [dir]) tree
  ["$", "ls"] -> let (lst, rest) = break ((=='$') . head) ss
                     tree'       = parseListing lst pwd tree
                 in parse rest pwd tree'
  _ -> parse ss pwd tree

updateDir :: Dir -> [String] -> Dir -> Dir
updateDir dir ["/"] _ = dir
updateDir dir ("/":xs) root = updateDir dir xs root
updateDir _ [] _ = error "Shouldn't have gotten here"
updateDir dir [_] root = let newFolders = Map.insert (dName dir) dir (dFolders root)
                         in root { dFolders = newFolders }
updateDir dir (x:xs) root = let newFolders = Map.adjust (updateDir dir xs) x (dFolders root)
                            in root { dFolders = newFolders }

parseListing :: [String] -> [String] -> Dir -> Dir
parseListing [] _   tree = tree
parseListing ls pwd tree = let current' = foldr addLs currentF ls
                           in updateDir current' pwd tree
  where currentF  = foldl (\t d -> dFolders t Map.! d) tree (tail pwd)
        addLs l d = case words l of
          ["dir", dirName] -> let new = Dir dirName Map.empty Map.empty
                                  old = dFolders d
                              in d { dFolders = Map.insert dirName new old }
          [size, fileName] -> let old = dFiles d
                              in d { dFiles = Map.insert fileName (read size) old }
          _ -> error "Failed to parse listing"
