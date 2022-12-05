import System.Environment (getArgs)

getInput :: String -> IO [String]
getInput defFile = do
  args <- getArgs
  raw  <- readFile (case args of [] -> defFile; x:_ -> x)
  return (lines raw)

main :: IO ()
main = do
  input <- getInput "inputs/day05.txt"
  let stacks = parseStacks (take 8 input)
      insts  = drop 10 input
      p1     = foldl (execute False) stacks insts
      p2     = foldl (execute True) stacks insts
  putStrLn $ "Part 1: " ++ concatMap (take 1) p1
  putStrLn $ "Part 2: " ++ concatMap (take 1) p2

parseStacks :: [String] -> [[Char]]
parseStacks ls = map (filter (/=' ')) stacks
  where stacks = map (\x -> map (!! x) ls) [1,5..35]

execute :: Bool -> [[Char]] -> String -> [[Char]]
execute p2Mode stacks inst = map rebuild [0..8]
  where ws    = words inst
        n     = read (ws !! 1)
        from  = read (ws !! 3) - 1
        to    = read (ws !! 5) - 1
        prev  = stacks !! from
        new   = if p2Mode then take n prev else reverse (take n prev)
        rebuild x
          | x == from = drop n prev
          | x == to   = new ++ (stacks !! to)
          | otherwise = stacks !! x
