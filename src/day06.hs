import System.Environment (getArgs)
import qualified Data.Set as Set

getInput :: String -> IO String
getInput defFile = do
  args <- getArgs
  readFile (case args of [] -> defFile; x:_ -> x)

main :: IO ()
main = do
  input <- getInput "inputs/day06.txt"
  putStrLn $ "Part 1: " ++ show (find 4 input 0)
  putStrLn $ "Part 2: " ++ show (find 14 input 0)

find :: Int -> String -> Int -> Int
find chunkSize s n
  | length chunk < chunkSize = error "Not found"
  | Set.size (Set.fromList chunk) == chunkSize = n + chunkSize
  | otherwise = find chunkSize (drop 1 s) (n + 1)
  where chunk = take chunkSize s
