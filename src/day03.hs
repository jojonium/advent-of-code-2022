import System.Environment (getArgs)
import Data.List (intersect)
import Data.Char (isUpper, ord)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <-  readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day03.txt"
  putStrLn $ "Part 1: " ++ show (sum (map (toScore . findRepeat) input))
  putStrLn $ "Part 2: " ++ show (part2 input)

findRepeat :: String -> Char
findRepeat s = head $ intersect a b
  where (a, b) = splitAt (length s `div` 2) s

toScore :: Char -> Int
toScore c
  | isUpper c = ord c - 38
  | otherwise = ord c - 96

part2 :: [String] -> Int
part2 (a:b:c:ds) = toScore (head ((a `intersect` b) `intersect` c)) + part2 ds
part2 _ = 0

