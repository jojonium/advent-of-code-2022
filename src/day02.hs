import System.Environment (getArgs)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <-  readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

data Shape = Rock | Paper | Scissors deriving (Eq)

main :: IO ()
main = do
  input <- getInput "inputs/day02.txt"
  putStrLn $ "Part 1: " ++ show (sum (map (p1score . parse) input))
  putStrLn $ "Part 2: " ++ show (sum (map (p2score . parse) input))

parse :: String -> (Shape, Shape)
parse (opp:_:self:_) = (toShape opp, toShape self)
  where toShape c
          | c `elem` "XA" = Rock
          | c `elem` "YB" = Paper
          | c `elem` "ZC" = Scissors
          | otherwise     = error $ "Invalid char: " ++ [c]
parse s = error $ "Invalid line: " ++ s

beats :: Shape -> Shape
beats Rock     = Paper
beats Paper    = Scissors
beats Scissors = Rock

losesTo :: Shape -> Shape
losesTo Rock     = Scissors
losesTo Paper    = Rock
losesTo Scissors = Paper

outcome :: Shape -> Shape -> Integer
outcome a b 
  | a == b         = 3
  | a == losesTo b = 6
  | otherwise      = 0

shapeScore :: Shape -> Integer
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

p1score :: (Shape, Shape) -> Integer
p1score (opp, self) = shapeScore self + outcome opp self

p2score :: (Shape, Shape) -> Integer
p2score (opp, self) = shapeScore (needToThrow self) + outcome opp (needToThrow self)
  where needToThrow Rock = losesTo opp
        needToThrow Paper = opp
        needToThrow Scissors = beats opp
